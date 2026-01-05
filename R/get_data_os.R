#' Retrieve Scientific Object Data for a Given Experiment
#'
#' This function retrieves data associated with a set of scientific objects
#' for a given experiment. It uses the \code{future::multisession} plan to
#' parallelize data fetching across multiple workers.
#'
#' If an error occurs during execution, the function safely captures the
#' exception and returns \code{NULL}.
#'
#' @param session A list containing session information, including the GraphQL
#'   endpoint URL and the authentication token.
#' @param exp_name Character string specifying the name or identifier of the
#'   experiment.
#' @param df_os A data frame containing scientific objects, with at least a
#'   \code{uri} column.
#' @param output_dir Optional character string specifying the directory where
#'   variable-specific CSV files will be written. If \code{NULL}, no files are created.
#'
#' @return
#' A combined \code{tibble} containing all retrieved data, or \code{NULL} if an
#' error occurs during execution.
#'
#' @details
#' The function temporarily switches the \code{future} execution plan to
#' \code{multisession} in order to enable parallel processing. The original
#' execution plan is restored to \code{sequential} when the function exits,
#' even if an error occurs. The \code{output_dir} parameter is passed to
#' downstream functions to control where CSV files are written.
#'
#' @importFrom future plan multisession sequential
#' @export


get_data_os <- function(session, exp_name, df_os, output_dir= NULL) {
  tryCatch({
    plan(multisession)
    on.exit(plan(sequential), add = TRUE)  # Remise à zéro propre

    get_data_by_os_uri_variable(session, exp_name, df_os, output_dir=output_dir)

  }, error = function(e) {
    message("❌ Erreur pendant l'exécution de get_data_os : ", e$message)
    NULL
  })
}
