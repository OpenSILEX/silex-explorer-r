

#' List Experiments
#'
#' Retrieves a list of experiments from OpenSILEX with optional filtering.
#'
#' @param session An opensilex_connection object obtained from login().
#' @param Species Character string. Filter experiments by species (optional).
#' @param projet Character string. Filter experiments by project (optional).
#' @param date Date or character string. Filter experiments active on this date (optional).
#' @param output_dir Character string. Directory to save results as CSV (optional).
#'
#' @return A data frame containing experiment information.
#'
#' @export

lsExp <- function(session, Species = NULL, projet = NULL, date = NULL, output_dir = NULL) {
  # Validate inputs
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  if (!is.null(date)) {
    tryCatch({
      date_converted <- as.Date(date)
      if (is.na(date_converted)) {
        stop("Invalid date format.")
      }
    }, error = function(e) {
      stop("Invalid date format.")
    })
  }

  # Define GraphQL query
  query <- '{
    Experiment(inferred: true) {
      _id
      label
      startDate
      endDate
      hasSpecies { label }
      hasProject { label }
      usesFacility { label }
    }
  }'

  tryCatch({
    # Make the POST request
    response <- httr::POST(
      url = session$urlGraphql,
      httr::add_headers(
        Authorization = paste("Bearer", session$token),
        "Content-Type" = "application/json"
      ),
      body = list(query = query),
      encode = "json",
      httr::timeout(60)
    )

    # Check response status
    httr::stop_for_status(response)

    # Parse response
    result <- httr::content(response, as = "parsed")

    if (is.null(result$data$Experiment)) {
      warning("No experiments found")
      return(data.frame())
    }

    # Process results
    experiments_df <- purrr::map_df(result$data$Experiment, function(exp) {
      insertUri_Name(exp$`_id`, exp$label)
      tibble::tibble(
        id = exp$`_id`,
        label = exp$label %||% "",
        start_date = if (!is.null(exp$startDate)) as.character(exp$startDate) else "",
        end_date = if (!is.null(exp$endDate)) as.character(exp$endDate) else "",
        species = if (!is.null(exp$hasSpecies) && length(exp$hasSpecies) > 0) {
          paste(purrr::map_chr(exp$hasSpecies, "label"), collapse = "; ")
        } else "",
        project = if (!is.null(exp$hasProject) && length(exp$hasProject) > 0) {
          paste(purrr::map_chr(exp$hasProject, "label"), collapse = "; ")
        } else "",
        facilities = if (!is.null(exp$usesFacility) && length(exp$usesFacility) > 0) {
          paste(purrr::map_chr(exp$usesFacility, "label"), collapse = "; ")
        } else ""
      )
    })

    # Apply filters
    if (!is.null(Species)) {
      experiments_df <- dplyr::filter(experiments_df, grepl(Species, species, fixed = TRUE))
    }
    if (!is.null(projet)) {
      experiments_df <- dplyr::filter(experiments_df, grepl(projet, project, fixed = TRUE))
    }
    if (!is.null(date)) {
      experiments_df <- dplyr::filter(
        experiments_df,
        as.Date(start_date) <= date,
        is.na(end_date) | end_date == "" | as.Date(end_date) >= date
      )
    }

    # Run URI consistency check
    UriCons()

    # Write to CSV only if output_dir is specified
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      output_file <- file.path(output_dir, paste0("experiments_", timestamp, ".csv"))

      write.csv(experiments_df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")
      message(sprintf("Data saved to %s", output_file))
    }

    return(experiments_df)

  }, error = function(e) {
    stop(sprintf("Error retrieving experiments: %s", conditionMessage(e)))
  })
}



