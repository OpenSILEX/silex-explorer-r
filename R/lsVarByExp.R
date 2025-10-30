#' Retrieve variables associated with an experiment
#'
#' Queries the OpenSILEX REST API to obtain variables used in a specified experiment,
#' returning details such as URI, name, entity, characteristic, method, and unit.
#' Optionally saves results to CSV.
#'
#' @param session An opensilex_connection object.
#' @param experiment_label Character string specifying the experiment label.
#' @param output_dir Character string or NULL. Directory path to save the output CSV file. If NULL, no file is saved.
#' @param verbose Logical. Whether to print messages about progress.
#' @param check_consistency Logical. Whether to check URI-name mapping consistency after retrieval.
#' @return A tibble with columns `uri`, `name`, `entity`, `characteristic`, `method`, and `unit`.
#' @export
#' @examples
#' \dontrun{
#' # Assuming session is already created by login function
#'
#' Example 1: Retrieve all variables having data (no filter)
#' all_variables <- lsVarByExp(session)
#'
#' # Example 2: Retrieve variables for an experiment
#' variables_by_exp <- lsVarByExp(session, experiment_label = "experiment_001")
#' print(variables_by_exp)
#'
#' # Example 3: Retrieve variables and save to a specific output directory
#' variables_by_exp_with_csv <- lsVarByExp(session, experiment_label = "experiment_002", output_dir = "/path/to/save")
#' print(variables_by_exp_with_csv)
#' }
lsVarByExp <- function(session,
                       experiment_label,
                       output_dir = NULL,
                       verbose = TRUE,
                       check_consistency = TRUE) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  if (is.null(experiment_label) || !is.character(experiment_label)) {
    stop("'experiment_label' must be a valid non-null character string")
  }

  `%||%` <- function(x, y) if (!is.null(x)) x else y

  # --- Step 1: get experiment URI from uri_name table

  experiment_uri <- getUrisFromName(experiment_label)

  if (length(experiment_uri) == 0) {
    warning(paste0("⚠️ No experiment found with label '", experiment_label, "'."))
    return(tibble::tibble())
  }

  if (length(experiment_uri) > 1) {
    selected_uri <- menu(experiment_uri, title = "Multiple URIs found. Please choose one URI:")
    experiment_uri <- experiment_uri[selected_uri]
  }


  # --- Step 2: Original logic begins here
  endpoint <- "/core/variables"
  url <- paste0(session$urlRest, endpoint)

  query_params <- list(
    experiments = experiment_uri,
    withAssociatedData = "True",
    order_by = "uri=asc",
    page = 0,
    page_size = 1000
  )

  response <- httr::GET(
    url = url,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      Accept = "application/json"
    ),
    query = query_params
  )

  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed", type = "application/json")

    if (verbose) message("API request successful. Processing data...")

    if (!is.null(result$result) && length(result$result) > 0) {
      variables_df <- purrr::map_df(result$result, function(var) {
        tibble::tibble(
          uri = var$uri %||% NA_character_,
          name = var$name %||% NA_character_,
          entity = var$entity$name %||% NA_character_,
          characteristic = var$characteristic$name %||% NA_character_,
          method = var$method$name %||% NA_character_,
          unit = var$unit$name %||% NA_character_
        )
      })

      insertUri_Name(variables_df[, c("uri", "name")])

      # Optional CSV export
      if (!is.null(output_dir)) {
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
          if (verbose) message("Created output directory: ", output_dir)
        }

        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        output_file <- file.path(output_dir, paste0("variables_by_exp_", experiment_label,"_",timestamp, ".csv"))
        write.csv(variables_df, file = output_file, row.names = FALSE, fileEncoding = "UTF-8")

        if (verbose) message("Data saved to ", output_file)
      } else if (verbose) {
        message("Output directory not specified. Data not saved to CSV.")
      }

      return(variables_df)

    } else {
      warning("No variables found for the specified experiment.")
      return(tibble::tibble())
    }

  } else {
    stop("REST API request failed with status code ", httr::status_code(response))
  }
}
