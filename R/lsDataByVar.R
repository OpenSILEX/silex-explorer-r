#' Retrieve data by variable from an experiment
#'
#' @param session An opensilex_connection object.
#' @param experiment_label Character, experiment label.
#' @param variable_name Character or NULL, variable(s) to filter.
#' @param output_dir Character or NULL, directory to save CSV files.
#' @param limit Numeric, limit per page.
#' @param max_pages Numeric, max pages to fetch.
#' @param obj_type Character or NULL, scientific object type filter.
#' @param save_to_file Logical, whether to save CSV files.
#' @return A list of data frames, one per variable.
#' @export
lsDataByVar <- function(session,
                        experiment_label,
                        variable_uri = NULL,
                        output_dir = NULL,
                        limit = 500,
                        obj_type = NULL,
                        save_to_file = TRUE) {
  # Required libraries
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)

  # Resolve experiment URI from label
  label_query <- sprintf('
    query {
      Experiment(filter: {label: "%s"}, inferred: true) {
        startDate
      }
    }', experiment_label)

  label_response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      "Content-Type" = "application/json"
    ),
    body = list(query = label_query),
    encode = "json",
    httr::timeout(60)
  )

  httr::stop_for_status(label_response)
  label_result <- httr::content(label_response, as = "parsed")

  if (length(label_result$data$Experiment) == 0) {
    stop("No experiment found with label: ", experiment_label)
  }

  exp_data <- label_result$data$Experiment[[1]]
  start_date <- gsub("-", "_", substr(exp_data$startDate, 1, 10))
  clean_label <- gsub("[^A-Za-z0-9]", "_", experiment_label)
  experiment_id <- paste0("EXP_", clean_label, "_", start_date)
  message("Resolved experiment ID: ", experiment_id)

  clean_variable_name <- function(var) {
    cleaned <- sub("^.*/", "", var)
    cleaned <- gsub("[^[:alnum:]_]", "_", cleaned)
    return(cleaned)
  }

  if (is.null(output_dir)) {
    output_dir <- getwd()
    message("No output_dir provided. Saving CSV files to current directory: ", output_dir)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  fetch_page <- function(page) {
    filter_clause <- if (!is.null(obj_type)) {
      sprintf(', filter: {type: "%s"}', obj_type)
    } else {
      ""
    }

    query <- sprintf('
      query {
        ScientificObject(Experience: %s, inferred: true, limit: %d, page: %d%s) {
          label
          type: _type(inferred: true)
          data {
            target
            variable
            value
            date
          }
        }
      }',
                     experiment_id, limit, page, filter_clause
    )

    response <- httr::POST(
      url = session$urlGraphql,
      httr::add_headers(Authorization = paste("Bearer", session$token)),
      body = list(query = query),
      encode = "json"
    )

    if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      json <- jsonlite::fromJSON(content, flatten = TRUE)
      data <- json$data$ScientificObject

      if (!is.null(data) && is.data.frame(data) && "data" %in% names(data)) {
        valid_data <- data[purrr::map_lgl(data$data, ~ !is.null(.x) && nrow(.x) > 0), ]
        return(valid_data)
      }
    }
    return(NULL)
  }

  page <- 1
  all_data_pages <- list()

  repeat {
    message("Fetching page: ", page)
    page_data <- fetch_page(page)
    if (is.null(page_data) || nrow(page_data) == 0) {
      message("No data found on page ", page, ". Stopping.")
      break
    }
    all_data_pages[[page]] <- page_data
    page <- page + 1
  }

  if (length(all_data_pages) == 0) {
    warning("No data found.")
    return(data.frame(label = character(), object_type = character(), value = numeric(), date = as.POSIXct(character())))
  }

  all_data <- bind_rows(all_data_pages)
  all_data <- tidyr::unnest(all_data, data)
  all_data$date <- format(as.POSIXct(all_data$date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), "%Y-%m-%d %H:%M:%S")

  all_data$object_type <- as.character(all_data$type)

  # Split by variable and optionally filter
  split_data <- split(all_data, all_data$variable)
  if (!is.null(variable_uri)) {
    split_data <- split_data[variable_uri]
  }

  results_list <- list()
  for (var in names(split_data)) {
    result <- split_data[[var]] %>%
      dplyr::select(label, object_type, value, date)

    if (save_to_file) {
      clean_var_name <- clean_variable_name(var)
      output_file <- file.path(output_dir, paste0(clean_var_name, "_data.csv"))

      tryCatch({
        write.csv(result, file = output_file, row.names = FALSE)
        message("Data for variable '", var, "' saved to ", output_file)
      }, error = function(e) {
        warning("Failed to save data for variable '", var, "': ", e$message)
      })
    }

    results_list[[var]] <- result
  }

  if (!is.null(variable_uri)) {
    return(results_list[[variable_uri]])
  } else {
    return(results_list)
  }
}
