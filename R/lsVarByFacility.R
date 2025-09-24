#' Retrieve variables associated with a facility and optional date range
#'
#' @param session An opensilex_connection object.
#' @param facility_uri Character, URI of the facility.
#' @param date_beginning Character or NULL, start date filter (YYYY-MM-DD).
#' @param date_end Character or NULL, end date filter (YYYY-MM-DD).
#' @param csv_filepath Character or NULL, path to save CSV file.
#' @return A tibble data frame with variable details.
#' @export
lsVarByFacility <- function(session,
                            facility_uri,
                            date_beginning = NULL,
                            date_end = NULL,
                            csv_filepath = NULL) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tibble)

  # --- Step 1: Build date filter
  date_filter <- list()
  if (!is.null(date_beginning) && !is.null(date_end) && date_beginning == date_end) {
    date_filter <- list(
      gte = paste0(date_beginning, "T00:00:00.000Z"),
      lte = paste0(date_end, "T23:59:59.999Z")
    )
  } else {
    if (!is.null(date_beginning)) {
      date_filter$gte <- paste0(date_beginning, "T00:00:00.000Z")
    }
    if (!is.null(date_end)) {
      date_filter$lte <- paste0(date_end, "T23:59:59.999Z")
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
      URI = v$`_id` %||% "",
      Name = v$label %||% "",
      Entity = v$hasEntity$label %||% "",
      Characteristic = v$hasCharacteristic$label %||% "",
      Method = v$hasMethod$label %||% "",
      Unit = v$hasUnit$label %||% ""
    )
  })

  # --- Step 4: Optional CSV output
  if (!is.null(csv_filepath)) {
    dir_path <- dirname(csv_filepath)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    write.csv(variable_info_df, file = csv_filepath, row.names = FALSE, fileEncoding = "UTF-8")
    message("✅ Data saved to ", csv_filepath)
  } else {
    message("Displaying results without saving to file.")
  }

  return(variable_info_df)
}
