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
lsOsTypeByExp <- function(session, experiment_label, output_dir = NULL, verbose = TRUE) {
  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }
  
  if (is.null(experiment_label) || !is.character(experiment_label)) {
    stop("'experiment_label' must be a non-null character string.")
  }
  
  if (!exists("uri_name") || !exists("uri_name_path")) {
    stop("Please call initUriName() before using this function.")
  }
  
  # Step 1: Retrieve experiment start date to build URI
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
  
  start_date <- gsub("-", "_", substr(label_result$data$Experiment[[1]]$startDate, 1, 10))
  clean_label <- gsub("[^A-Za-z0-9]", "_", experiment_label)
  experiment_uri <- paste0("EXP_", clean_label, "_", start_date)
  
  if (verbose) message("Constructed experiment URI: ", experiment_uri)
  
  # Step 2: Query scientific objects from the experiment
  os_query <- sprintf('
    query {
      ScientificObject(Experience: %s, inferred: true) {
        type
      }
    }', experiment_uri)
  
  os_response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      "Content-Type" = "application/json"
    ),
    body = list(query = os_query),
    encode = "json",
    httr::timeout(60)
  )
  
  httr::stop_for_status(os_response)
  os_result <- httr::content(os_response, as = "parsed")
  
  if (length(os_result$data$ScientificObject) == 0) {
    warning("No scientific objects found for this experiment.")
    return(data.frame())
  }
  
  # Extract types and count them
  types <- sapply(os_result$data$ScientificObject, function(x) x$type)
  types_df <- as.data.frame(table(types), stringsAsFactors = FALSE)
  colnames(types_df) <- c("uri", "count")
  
  # Extract name from URI and update CSV
  types_df$name <- vapply(types_df$uri, function(uri) {
    name_extracted <- sub(".*#", "", uri)
    insertUri_Name(uri, name_extracted)
    return(name_extracted)
  }, character(1))
  
  types_df <- types_df[, c("uri", "name")]
  
  # Step 3: Optional CSV export
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      if (verbose) message("Created output directory: ", output_dir)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- file.path(output_dir, paste0("object_types_", timestamp, ".csv"))
    write.csv(types_df, file = output_file, row.names = FALSE)
    
    if (verbose) message("Type summary saved to ", output_file)
  }
  
  return(types_df)
}

