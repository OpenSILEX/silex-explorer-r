#' Retrieve environmental data for a facility with optional variable and date filters
#'
#' This function retrieves environmental measurements associated with a given facility.
#' Results can be filtered by date range and by specific variable names. Data can optionally
#' be exported as CSV files (one file per variable).
#'
#' @param session An opensilex_connection object.
#' @param facility_label Character, label of the facility (e.g. "greenhouse 1").
#' @param date_beginning Character or NULL, start date filter (YYYY-MM-DD or ISO format).
#' @param date_end Character or NULL, end date filter (YYYY-MM-DD or ISO format).
#' @param variable_names Character vector or NULL, names of variables to filter
#'   (e.g. "air_temperature_thermocouple_degreeCelsius").
#' @param output_dir Character or NULL, directory where CSV files will be saved
#'   (one CSV per variable).
#'
#' @return A named list of tibbles. Each list element corresponds to one variable URI
#'   and contains a data frame with columns:
#'   \itemize{
#'     \item Date: measurement timestamp
#'     \item Value: measured value
#'     \item Device: associated device URI(s)
#'   }
#'
#' @examples
#' \dontrun{
#' # Assuming session is already created using a login function
#'
#' # Example 1: Retrieve all environmental data for a facility
#' env_data_all <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1"
#' )
#' print(env_data_all)
#'
#' # Example 2: Retrieve data for a specific variable
#' variable_names <- c("air_temperature_thermocouple_degreeCelsius")
#' env_data_var <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   variable_names = variable_names
#' )
#' print(env_data_var)
#'
#' # Example 3: Retrieve data with a start date filter
#' env_data_start <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   date_beginning = "2017-04-16"
#' )
#' print(env_data_start)
#'
#' # Example 4: Retrieve data with an end date filter (ISO format)
#' env_data_end <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   date_end = "2017-04-16T20:00:00.000Z"
#' )
#' print(env_data_end)
#'
#' # Example 5: Retrieve data for a variable with a date range
#' env_data_range <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   variable_names = c("air_temperature_thermocouple_degreeCelsius"),
#'   date_beginning = "2017-04-16",
#'   date_end = "2017-04-18"
#' )
#' print(env_data_range)
#'
#' # Example 6: Retrieve data and export CSV files
#' env_data_csv <- lsEnvDataByFacility(
#'   session,
#'   facility_label = "greenhouse 1",
#'   variable_names = c("air_temperature_thermocouple_degreeCelsius"),
#'   output_dir = "your_output_directory_here"
#' )
#' print(env_data_csv)
#' }
#'
#' @export

lsEnvDataByFacility <- function(session,
                                facility_label,
                                date_beginning = NULL,
                                date_end = NULL, variable_names = NULL, output_dir = NULL) {
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  #----------------------------------------------------------
  # Retrieve facility uri from uri_name table
  #----------------------------------------------------------

  facility_uri <- getUrisFromName(facility_label)
  if(length(facility_uri) > 1) {
    warning("Multiple URIs found, the first one will be used by default")
    facility_uri <- facility_uri[1]
  }

  # Check/create output directory if CSV export is requested
  #----------------------------------------------------------
  save_to_csv <- !is.null(output_dir)

  if (save_to_csv) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
    }
  }

  # --- Step 1: Build date filter

  `%||%` <- function(a, b) if (!is.null(a)) a else b

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
      devices <- purrr::map_chr(item$prov_agent$agents, ~ .x$uri %||% NA_character_)
    }

    if (length(devices) == 0 && !is.null(item$provenance$provWasAssociatedWith)) {
      devices <- purrr::map_chr(item$provenance$provWasAssociatedWith, ~ .x$uri %||% NA_character_)
    }

    paste(devices, collapse = ", ")
  }

  df_all <- purrr::map_dfr(data_items, function(d) {
    tibble(
      VariableURI = d$variable %||% NA_character_,
      Value = d$value %||% NA_character_,
      Date = d$date %||% NA_character_,
      Device = process_device_uris(d)
    )
  })




  if (nrow(df_all) > 0) {

    split_data <- split(df_all, df_all$VariableURI)

    #----------------------------------------------------------
    #  Filter by variable_names (if provided)
    #----------------------------------------------------------
    variable_uris <- NULL  # Initialize variable_uris
    if (!is.null(variable_names)) {
      # Retrieve URIs for each variable name
      variable_uris <- sapply(variable_names, function(var_name) {
        uris <- getUrisFromName(var_name)
        if (length(uris) > 1) {
          warning(paste("Multiple URIs found for variable:", var_name, "the first one will be used"))
          return(uris[1])
        } else if (length(uris) == 0) {
          warning(paste("No URI found for variable:", var_name))
          return(NULL)
        }
        return(uris[1])
      })
      variable_uris <- variable_uris[!is.null(variable_uris)]  # Remove NULLs

      if (length(variable_uris) == 0) {
        warning("WARNING: No valid URIs found for the provided variable names.")
        return(list())  # Return empty list if no valid URIs
      }
    }

    if (length(variable_uris) > 0) {
      split_data <- split_data[names(split_data) %in% variable_uris]
      if (length(split_data) == 0) {
        warning("WARNING: None of the provided variables were found in the data.")
        return(list())  # Return empty list if no match
      }
    }

    #----------------------------------------------------------
    #  Export CSV (optional) + return list of dataframes
    #----------------------------------------------------------
    results <- list()

    for (var_uri in names(split_data)) {
      df <- split_data[[var_uri]] %>%
        dplyr::select(Date, Value, Device)

      results[[var_uri]] <- df

      # Save to CSV if output_dir is provided
      if (save_to_csv) {
        var_name <- getNamesFromUri(var_uri)
        if(length(var_name) > 1) {
          warning("Multiple URIs found, the first one will be used by default")
          var_name <- var_name[1]
        }
        name_clean <- gsub("[^A-Za-z0-9_-]", "_", var_name)

        # Replace spaces with underscores
        var_name <- gsub("\\s+", "_", name_clean)
        csv_path <- file.path(output_dir, paste0(var_name, "_data.csv"))

        tryCatch({
          write.csv(df, csv_path, row.names = FALSE)
          message("OK: Saved data for variable '", var_name, "' -> ", csv_path)
        }, error = function(e) {
          warning("WARNING: Failed to save ", var_name, ": ", e$message)
        })
      }
    }

    return(results)  # Return a list of dataframes, one per variable
} else {
  # If all_data_df is empty
  message("ERROR: No available data.")
  return(list())  # Return empty list
}
}
