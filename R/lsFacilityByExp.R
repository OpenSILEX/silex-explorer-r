#' Retrieve facilities used in experiments
#'
#' @param session An opensilex_connection object.
#' @param exp_label Character or NULL, experiment label filter (optional).
#' @param output_dir Character or NULL, directory to save CSV file (optional).
#' @param verbose Logical, whether to print messages (default TRUE).
#' @param check_consistency Logical, whether to check URI-Name consistency (default TRUE).
#' @return A tibble data frame of facilities by experiment.
#' @export

lsFacilityByExp <- function(session,
                            exp_label = NULL,
                            output_dir = NULL,
                            verbose = TRUE) {
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  query <- '
    query {
      Experiment(inferred: true) {
        label
        _id
        usesFacility {
          _id
          label
          type
        }
      }
    }
  '
  
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
  
  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed")
  
  if (is.null(result$data$Experiment)) {
    warning("No experiments found.")
    return(tibble::tibble())
  }
  
  facilities_df <- purrr::map_df(result$data$Experiment, function(exp) {
    if (!is.null(exp_label) && !grepl(exp_label, exp$label, ignore.case = TRUE)) return(NULL)
    if (is.null(exp$usesFacility) || length(exp$usesFacility) == 0) return(NULL)
    purrr::map_df(exp$usesFacility, function(fac) {
      tibble::tibble(
        experiment_label = exp$label %||% "",
        facility_id = fac$`_id` %||% "",
        facility_label = fac$label %||% ""
      )
    })
  })
  
  if (nrow(facilities_df) == 0) {
    if (verbose) message("No facilities found for the given experiment label.")
    return(facilities_df)
  }
  
  # Optional export
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      if (verbose) message("Created output directory: ", output_dir)
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_out <- file.path(output_dir, paste0("facilities_by_exp_", timestamp, ".csv"))
    write.csv(facilities_df, file_out, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) message("Data saved to ", file_out)
  } else if (verbose) {
    print(facilities_df)
  }
  
  return(facilities_df)
}
