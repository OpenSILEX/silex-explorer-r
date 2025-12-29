#' Retrieve variables associated with a facility and optional date range
#'
#' @param session An opensilex_connection object.
#' @param facility_uri Character, URI of the facility.
#' @param date_beginning Character or NULL, start date filter (YYYY-MM-DD).
#' @param date_end Character or NULL, end date filter (YYYY-MM-DD).
#' @param output_dir Character or NULL, directory to save CSV file (optional).
#' @param verbose Logical, whether to print messages (default TRUE).
#' @return A tibble data frame with variable details.
#' @examples
#' \dontrun{
#' # Assuming session is already created by login function
#'
#' # Example 1: Retrieve all environnemental variables for a given facility
#' variables_by_facility <- lsVarByFacility(session, facility_label = "greenhouse 1")
#' print(variables_by_facility)
#'
#' # Example 2: Retrieve variables and save to a specific output directory
#' variables_by_facility_csv <- lsVarByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   output_dir = "your_output_directory_here"
#' )
#' print(variables_by_facility_csv)
#'
#' # Example 3: Retrieve variables with a start date filter
#' variables_by_facility_beg <- lsVarByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   date_beginning = "2017-04-16"
#' )
#' print(variables_by_facility_beg)
#'
#' # Example 4: Retrieve variables with an end date filter (ISO format)
#' variables_by_facility_end <- lsVarByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   date_end = "2017-04-16T20:00:00.000Z"
#' )
#' print(variables_by_facility_end)
#'
#' # Example 5: Retrieve variables with both start and end date filters
#' variables_by_facility_beg_end <- lsVarByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   date_beginning = "2017-04-16T20:00:00.000Z",
#'   date_end = "2017-04-16T22:00:00.000Z"
#' )
#' print(variables_by_facility_beg_end)
#' }
#'
#' @export
lsVarByFacility <- function(session,
                            facility_label,
                            date_beginning = NULL,
                            date_end = NULL,
                            output_dir = NULL,  verbose = TRUE) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (is.null(facility_label) || !is.character(facility_label)) {
    stop("'facility_label' must be a valid non-null character string")
  }

  # --- Step 0: get facility URI from uri_name table

  facility_uri <- getUrisFromName(facility_label)

  if (length(facility_uri) == 0) {
    warning(paste0("⚠️ No facility found with label '", facility_label, "'."))
    return(tibble::tibble())
  }


  if (length(facility_uri) > 1) {
    selected_uri <- menu(facility_uri, title = "Multiple URIs found. Please choose one URI:")
    facility_uri <- facility_uri[selected_uri]
  }

  # --- Step 1: Build date filter
  date_filter <- list()
  if (!is.null(date_beginning) && !is.null(date_end) && date_beginning == date_end) {
    date_filter <- list(
      gte = paste0(date_beginning, "T00:00:00.000Z"),
      lte = paste0(date_end, "T23:59:59.999Z")
    )
  } else {
    if (!is.null(date_beginning)) {
      if (!grepl("T", date_beginning)) {
        date_filter$gte <- paste0(date_beginning, "T00:00:00.000Z")
      } else {
        date_filter$gte <- date_beginning
      }
    }

    if (!is.null(date_end)) {
      if (!grepl("T", date_end)) {
        date_filter$lte <- paste0(date_end, "T23:59:59.999Z")
      } else {
        date_filter$lte <- date_end
      }
    }
  }

  filter_input <- list(target = facility_uri)
  if (length(date_filter) > 0) {
    filter_input$`_operators` <- list(date = date_filter)
  }

  # --- Step 2: Query unique variables
  query_vars <- '
    query GetEnvironmentalData($filter: FilterFindManyDataInput) {
      Data_findMany(filter: $filter) {
        variable
      }
    }
  '

  response_vars <- httr::POST(
    url = session$urlGraphql,
    body = list(query = query_vars, variables = list(filter = filter_input)),
    encode = "json",
    httr::add_headers(Authorization = paste("Bearer", session$token))
  )

  httr::stop_for_status(response_vars)
  result_vars <- httr::content(response_vars, as = "parsed")
  variable_data <- result_vars$data$Data_findMany

  if (is.null(variable_data) || length(variable_data) == 0) {
    warning("No variables found for the given facility and date range.")
    return(tibble())
  }

  variable_uris <- unique(purrr::map_chr(variable_data, ~ .x$variable %||% NA_character_))

  # --- Step 3: Query variable details
  query_details <- '
    query GetVariableDetails($filter: FilterVariable) {
      Variable(inferred: true, filter: $filter) {
        _id
        label
        hasEntity { label }
        hasCharacteristic { label }
        hasMethod { label }
        hasUnit { label }
      }
    }
  '

  response_details <- httr::POST(
    url = session$urlGraphql,
    body = list(query = query_details, variables = list(filter = list(`_id` = variable_uris))),
    encode = "json",
    httr::add_headers(Authorization = paste("Bearer", session$token))
  )

  httr::stop_for_status(response_details)
  result_details <- httr::content(response_details, as = "parsed")
  vars <- result_details$data$Variable

  if (is.null(vars) || length(vars) == 0) {
    warning("Variable details not found.")
    return(tibble())
  }

  variable_info_df <- purrr::map_dfr(vars, function(v) {
    tibble(
      uri = v$`_id` %||% "",
      name = v$label %||% "",
      entity = v$hasEntity$label %||% "",
      characteristic = v$hasCharacteristic$label %||% "",
      method = v$hasMethod$label %||% "",
      unit = v$hasUnit$label %||% ""
    )
  })

  insertUri_Name(variable_info_df[, c("uri", "name")])

  # --- Step 4: Optional CSV output
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      if (verbose) message("Created output directory: ", output_dir)
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_out <- file.path(output_dir, paste0("env_var_by_fac_",facility_label,"_", timestamp, ".csv"))
    write.csv(variable_info_df, file_out, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) message("Data saved to ", file_out)
  } else if (verbose) {
    print(facilities_df)
  }

  return(variable_info_df)
}
