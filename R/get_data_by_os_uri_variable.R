#' Retrieve Data by Scientific Object URI and Experiment Variables
#'
#' This function retrieves scientific data associated with a list of scientific
#' object URIs while processing experiment-specific variables. Data are fetched
#' in chunks and in parallel to improve performance, and the results are exported
#' by variable to CSV files.
#'
#' @param session A connection object containing authentication information and
#'   the GraphQL endpoint URL.
#' @param experience A character string, vector, or list identifying the
#'   experiment(s) to query.
#' @param df_os A data frame containing at least a \code{uri} column listing the
#'   scientific objects to query.
#' @param ls_var_exp Optional data frame of variables by experiment. If
#'   \code{NULL} or empty, the function \code{lsVarByExp()} is called to retrieve
#'   the variables automatically.
#' @param output_dir Optional character string specifying the directory where
#'   variable-specific CSV files will be written. If \code{NULL}, no files are created.
#'
#' @return
#' No value is returned explicitly. Retrieved data are exported using
#' \code{export_data_by_variable_to_csv()}.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks that \code{df_os} contains a \code{uri} column.
#'   \item Retrieves or generates a unique experiment identifier using
#'   \code{get_experiment_id()}.
#'   \item Splits scientific object URIs into chunks to limit the number of IDs
#'   per request.
#'   \item Fetches data for each chunk in parallel using
#'   \code{furrr::future_map()}, with error handling via
#'   \code{purrr::safely()}.
#'   \item Combines all retrieved data into a single data frame.
#'   \item Exports the combined data by variable to CSV files when
#'   \code{output_dir} is provided.
#' }
#'
#' @importFrom purrr safely
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @export

get_data_by_os_uri_variable <- function(session, experience, df_os, ls_var_exp = NULL,output_dir = NULL) {
  max_ids_per_request <- 40

  if (!"uri" %in% names(df_os)) {
    warning("DataFrame doesn't contain URI column.")
    return(NULL)
  }


  if (is.null(ls_var_exp) || nrow(ls_var_exp) == 0) {
    ls_var_exp <- lsVarByExp(session, experience)
  }

  experiment_id <- get_experiment_id(experience, session)

  os_uris <- df_os$uri
  chunks <- chunk_list(os_uris, max_ids_per_request)

  # Utiliser safely pour capter les erreurs dans les appels
  safe_fetch <- purrr::safely(fetch_chunk_data, otherwise = tibble::tibble())

  results <- furrr::future_map(chunks, ~{
    out <- safe_fetch(.x, experiment_id, session)
    if (!is.null(out$error)) {
      message("❌ Error in chunk: ", paste(.x, collapse = ", "))
    }
    out$result
  }, .progress = TRUE)

  all_data <- dplyr::bind_rows(results)

  export_data_by_variable_to_csv(ls_var_exp, all_data, output_dir=output_dir)
}

