#' Retrieve movement/position data of a scientific object within an experiment
#'
#' @param session An opensilex_connection object.
#' @param experiment_label Character, experiment label.
#' @param scientific_object_id Character, ID of the scientific object.
#' @param start_date Character or POSIXct or NULL, start date filter (optional).
#' @param end_date Character or POSIXct or NULL, end date filter (optional).
#' @param output_dir Character or NULL, directory to save CSV file (optional).
#' @param verbose Logical, whether to print messages (default TRUE).
#' @return A tibble data frame with movement records.
#' @export

lsMoveByOs <- function(session, experiment_label, scientific_object_id,
                       start_date = NULL, end_date = NULL,
                       output_dir = NULL, verbose = TRUE) {
  
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }
  
  if (is.null(experiment_label) || !is.character(experiment_label)) {
    stop("'experiment_label' must be a non-null character string.")
  }
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ---- STEP 1: Construct experiment URI from label ----
  if (verbose) message("Fetching start date for experiment label: ", experiment_label)
  
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
  start_date_fmt <- gsub("-", "_", substr(exp_data$startDate, 1, 10))
  clean_label <- gsub("[^A-Za-z0-9]", "_", experiment_label)
  experiment_id <- paste0("EXP_", clean_label, "_", start_date_fmt)
  
  if (verbose) message("Constructed experiment URI: ", experiment_id)
  
  # ---- STEP 2: Validate and parse dates ----
  if (!is.null(start_date)) {
    tryCatch({
      start_date <- as.POSIXct(start_date)
    }, error = function(e) {
      stop("Invalid start_date format. Use YYYY-MM-DD or POSIXct.")
    })
  }
  
  if (!is.null(end_date)) {
    tryCatch({
      end_date <- as.POSIXct(end_date)
    }, error = function(e) {
      stop("Invalid end_date format. Use YYYY-MM-DD or POSIXct.")
    })
  }
  
  # ---- STEP 3: Build GraphQL query ----
  query <- sprintf('query {
    ScientificObject(
      Experience: %s
      inferred: true
      filter: {_id: "%s"}
    ) {
      _id
      label
      positions {
        hasBeginning(source: %s) {
          inXSDDateTimeStamp
        }
        hasEnd(source: %s) {
          inXSDDateTimeStamp
        }
        position {
          targetPositions {
            target
          }
        }
        to {
          label
          withinSite {
            label
          }
        }
        isInstant
        from {
          label
          withinSite {
            label
          }
        }
        comment
        _id
      }
    }
  }', experiment_id, scientific_object_id, experiment_id, experiment_id)
  
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )
  
  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed")
  
  so_data <- result$data$ScientificObject
  if (is.null(so_data) || length(so_data) == 0) {
    warning("No scientific object found.")
    return(data.frame())
  }
  
  so <- so_data[[1]]
  positions <- so$positions
  if (is.null(positions) || length(positions) == 0) {
    warning("No movement data found for the specified object.")
    return(data.frame())
  }
  
  # ---- STEP 4: Apply date filters ----
  filtered_positions <- if (!is.null(start_date) || !is.null(end_date)) {
    purrr::keep(positions, function(pos) {
      # Fallback to hasEnd if hasBeginning is not available
      time_str <- pos$hasBeginning$inXSDDateTimeStamp %||% pos$hasEnd$inXSDDateTimeStamp
      if (!is.null(time_str)) {
        time_val <- tryCatch({
          as.POSIXct(time_str, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        }, error = function(e) NA)
        
        begin_check <- is.null(start_date) || (!is.na(time_val) && time_val >= start_date)
        end_check <- is.null(end_date) || (!is.na(time_val) && time_val <= end_date)
        return(begin_check && end_check)
      }
      return(FALSE)
    })
  } else {
    positions
  }
  
  if (length(filtered_positions) == 0) {
    warning("No movement records match the specified date range.")
    return(data.frame())
  }
  
  # ---- STEP 5: Construct output dataframe ----
  moves_df <- purrr::map_dfr(filtered_positions, function(pos) {
    tibble::tibble(
      scientific_object_id = so$`_id`,
      scientific_object_label = so$label %||% NA,
      position_id = pos$`_id` %||% NA,
      has_beginning = pos$hasBeginning$inXSDDateTimeStamp %||% NA,
      has_end = pos$hasEnd$inXSDDateTimeStamp %||% NA,
      is_instant = pos$isInstant %||% NA,
      from_label = pos$from$label %||% NA,
      from_within_site = pos$from$withinSite$label %||% NA,
      to_label = pos$to$label %||% NA,
      to_within_site = pos$to$withinSite$label %||% NA,
      target_positions = if (!is.null(pos$position$targetPositions)) {
        paste(sapply(pos$position$targetPositions, function(tp) tp$target), collapse = ", ")
      } else NA,
      comment = pos$comment %||% NA
    )
  })
  
  # ---- STEP 6: Optional CSV export ----
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- file.path(output_dir, paste0("moves_", timestamp, ".csv"))
    write.csv(moves_df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
    message(sprintf("✅ Data saved to %s", output_file))
  } else {
    message("✅ Data returned but not saved (output_dir not provided).")
  }
  
  return(moves_df)
}