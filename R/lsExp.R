

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
#' @examples
#' \dontrun{
#' # Assuming session is already created by login function
#' # List all experiments
#' all_experiments <- lsExp(session)
#' print(all_experiments)
#'
#' # List experiments active on a specific date
#' active_experiments <- lsExp(session, date = "2025-01-10")
#' print(active_experiments)
#'
#' # List experiments filtered by project
#' expe_by_project <- lsExp(session, projet = "EPPN2020")
#' print(expe_by_project)
#'
#' # List experiments filtered by species
#' expe_by_species <- lsExp(session, Species = "Zea mays")
#' print(expe_by_species)
#'
#' # List experiments filtered by both species and project
#' expe_by_species_project <- lsExp(session, date = "2017-05-18", projet = "French plant phenomic network (FPPN)", Species = "Zea mays")
#' print(expe_by_species_project)
#' }
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

    # Process results and prepare all URI-name pairs at once
    all_uri_name_pairs <- purrr::map_df(result$data$Experiment, function(exp) {
      tibble::tibble(
        uri = exp$`_id`,  # URI is taken from the experiment's _id
        name = exp$label %||% ""  # Name is taken from the experiment's label
      )
    })

    # Call insertUri_Name to insert all URI-name pairs at once
    insertUri_Name(all_uri_name_pairs)

    # Process results
    experiments_df <- purrr::map_df(result$data$Experiment, function(exp) {
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
      experiments_df <- dplyr::filter(experiments_df,
                                      sapply(strsplit(species, "; "), function(x) any(x == Species)))
    }

    if (!is.null(projet)) {
      experiments_df <- dplyr::filter(experiments_df,
                                      sapply(strsplit(project, "; "), function(x) any(x == projet)))
    }
    if (!is.null(date)) {
      experiments_df <- dplyr::filter(
        experiments_df,
        as.Date(start_date) <= date,
        is.na(end_date) | end_date == "" | as.Date(end_date) >= date
      )
    }

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



