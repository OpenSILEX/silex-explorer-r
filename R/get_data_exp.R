#' Retrieve All Data from an Experiment with Pagination
#'
#' Retrieves all data associated with a given experiment in OpenSILEX using
#' the GraphQL `Data_pagination` service. This includes data linked to
#' scientific objects as well as data associated with facilities.
#'
#' The function paginates results to avoid overloading the server and
#' can optionally export the retrieved data as CSV files.
#'
#' @param session An opensilex_connection object obtained from \code{login()}.
#' @param experiment_uri Character. URI of the experiment.
#' @param variable_names Character vector or NULL. Optional list of variable names to filter on.
#' If NULL, all variables are returned.
#' @param output_dir Character string or NULL. Directory path where CSV files will be saved.
#' If NULL, no files are written.
#' @param per_page Integer. Number of items per page for pagination (default 400000).
#'
#' @return A named list of dataframes (one per variable) containing:
#' \itemize{
#' \item uri: target URI
#' \item value: measured value
#' \item date: measurement date
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Measure execution time and retrieve all data for an experiment
#' ls_data <- lsData(
#'   session,
#'   experiment_uri = "http://phenome.inrae.fr/diaphen/id/experiment/DIA2020-2",
#'   output_dir = "your_output_directory_here"
#' )
#'
#' # Retrieve data without saving to CSV
#' ls_data <- lsData(
#'   session,
#'   experiment_uri = "http://phenome.inrae.fr/diaphen/id/experiment/DIA2020-2"
#' )
#' print(ls_data)
#'
#' # Retrieve data for specific variables
#' variable_names <- c("supply_voltage_measurement_volt")
#' ls_data <- lsData(
#'   session,
#'   experiment_uri = "http://phenome.inrae.fr/diaphen/id/experiment/DIA2020-2",
#'   variable_names = variable_names,
#'   output_dir = "your_output_directory_here"
#' )
#' }
lsData <- function(session,
                   experiment_uri,
                   variable_names = NULL,
                   output_dir = NULL,
                   per_page = 400000) {

  #----------------------------------------------------------
  # 1️⃣ Prepare output directory if needed
  #----------------------------------------------------------
  save_to_csv <- !is.null(output_dir)
  if (save_to_csv && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  #----------------------------------------------------------
  # 2️⃣ Prepare GraphQL query template for pagination
  #----------------------------------------------------------
  query_template <- '
  query GetDataByExp($experiment_uri: String!, $page: Int, $perPage: Int) {
    Data_pagination(
      filter: { provenance: { experiments: [$experiment_uri] } }
      page: $page
      perPage: $perPage
    ) {
      items {
        target
        variable
        value
        date
      }
      pageInfo {
        hasNextPage
      }
    }
  }'

  all_data <- list()
  page <- 1
  has_next <- TRUE

  #----------------------------------------------------------
  # 3️⃣ Pagination loop
  #----------------------------------------------------------
  while (has_next) {
    response <- httr::POST(
      url = session$urlGraphql,
      httr::add_headers(
        Authorization = paste("Bearer", session$token),
        "Content-Type" = "application/json"
      ),
      body = list(
        query = query_template,
        variables = list(
          experiment_uri = experiment_uri,
          page = page,
          perPage = per_page
        )
      ),
      encode = "json"
    )

    result <- httr::content(response, as = "parsed")

    if (!is.null(result$errors)) {
      stop("GraphQL error: ", result$errors[[1]]$message)
    }

    items <- result$data$Data_pagination$items
    has_next <- result$data$Data_pagination$pageInfo$hasNextPage

    if (length(items) > 0) {
      all_data <- c(all_data, items)
    }

    page <- page + 1
  }

  if (length(all_data) == 0) {
    message("No data available for this experiment.")
    return(list())
  }

  #----------------------------------------------------------
  # 4️⃣ Convert JSON result to dataframe
  #----------------------------------------------------------

  all_data_df <- data.table::rbindlist(all_data, fill = TRUE)

  all_data_df <- all_data_df[!is.na(all_data_df$variable), ]
  #----------------------------------------------------------
  # 5️⃣ Filter by variable_names if provided
  #----------------------------------------------------------
  if (!is.null(variable_names)) {

    variable_uris <- sapply(variable_names, function(var_name) {
      uris <- getUrisFromName(var_name)
      if (length(uris) == 0) {
        warning("No URI found for variable: ", var_name)
        return(NULL)
      }
      if (length(uris) > 1) {
        warning("Multiple URIs found for variable: ", var_name, " - first used.")
      }
      uris[1]
    })

    variable_uris <- variable_uris[!is.null(variable_uris)]

    if (length(variable_uris) == 0) {
      warning("No valid variable URIs found.")
      return(list())
    }

    all_data_df <- all_data_df[all_data_df$variable %in% variable_uris, ]
  }

  #----------------------------------------------------------
  # 6️⃣ Split data by variable
  #----------------------------------------------------------
  split_data <- split(all_data_df, all_data_df$variable)
  if (length(split_data) == 0) {
    warning("No matching variables found in retrieved data.")
    return(list())
  }

  #----------------------------------------------------------
  # 7️⃣ Build results + optional CSV export
  #----------------------------------------------------------
  results <- lapply(names(split_data), function(var_uri) {

    df <- split_data[[var_uri]] |>
      dplyr::select(uri = target, value, date)

    if (save_to_csv) {
      var_name <- getNamesFromUri(var_uri)
      if (length(var_name) > 1) var_name <- var_name[1]
      name_clean <- gsub("[^A-Za-z0-9_-]", "_", var_name)
      name_clean <- gsub("\\s+", "_", name_clean)
      csv_path <- file.path(output_dir, paste0(name_clean, "_data.csv"))
      tryCatch(
        write.csv(df, csv_path, row.names = FALSE),
        error = function(e) warning("Failed to save ", name_clean, ": ", e$message)
      )
    }

    df
  })

  names(results) <- names(split_data)
  return(results)
}
