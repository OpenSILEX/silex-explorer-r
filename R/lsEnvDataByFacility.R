#' Retrieve environmental data variables for a facility over an optional date range
#'
#' @param session An opensilex_connection object.
#' @param facility_uri Character, URI of the facility.
#' @param date_beginning Character or NULL, start date filter (YYYY-MM-DD).
#' @param date_end Character or NULL, end date filter (YYYY-MM-DD).
#' @param csv_filepath Character or NULL, path to save CSV file.
#' @return A tibble data frame with environmental data.
#' @export


lsEnvDataByFacility <- function(session,
                                facility_uri,
                                date_beginning = NULL,
                                date_end = NULL, csv_filepath = NULL) {
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(stringr)

  # Step 1: Build date filter
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

  # Step 2: Query full data
  query_data <- '
    query GetEnvironmentalData($filter: FilterFindManyDataInput) {
      Data_findMany(filter: $filter) {
        variable
        value
        date
        provenance {
          provWasAssociatedWith {
            uri
          }
        }
        prov_agent {
          agents {
            uri
          }
        }
      }
    }
  '

  response <- httr::POST(
    url = session$urlGraphql,
    body = list(query = query_data, variables = list(filter = filter_input)),
    encode = "json",
    httr::add_headers(Authorization = paste("Bearer", session$token))
  )

  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed")

  data_items <- result$data$Data_findMany

  if (is.null(data_items) || length(data_items) == 0) {
    warning("No environmental data found for the given parameters.")
    return(list())
  }

  # Step 3: Process data
  process_device_uris <- function(item) {
    devices <- character(0)

    if (!is.null(item$prov_agent$agents)) {
      devices <- map_chr(item$prov_agent$agents, ~ .x$uri %||% NA_character_)
    }

    if (length(devices) == 0 && !is.null(item$provenance$provWasAssociatedWith)) {
      devices <- map_chr(item$provenance$provWasAssociatedWith, ~ .x$uri %||% NA_character_)
    }

    paste(devices, collapse = ", ")
  }

  df_all <- map_dfr(data_items, function(d) {
    tibble(
      VariableURI = d$variable %||% NA_character_,
      VariableShort = str_extract(d$variable %||% "", "[^/]+$"),
      Value = d$value %||% NA_character_,
      Date = d$date %||% NA_character_,
      Device = process_device_uris(d)
    )
  })

  # Step 4: Group by variable and optionally export
  data_by_var <- split(df_all, df_all$VariableShort)

  if (!is.null(csv_filepath)) {
    dir.create(csv_filepath, recursive = TRUE, showWarnings = FALSE)

    walk2(data_by_var, names(data_by_var), function(df, var_name) {
      file_path <- file.path(csv_filepath, paste0(var_name, "_data.csv"))
      write.csv(df %>% select(Value, Date, Device), file = file_path, row.names = FALSE, fileEncoding = "UTF-8")
      message("✅ Data for variable '", var_name, "' saved to ", file_path)
    })
  } else {
    message("Displaying results without saving to file.")
  }

  return(data_by_var)
}
