#' Retrieve Scientific Object Data for a Given Experiment
#'
#' This function retrieves data associated with a set of scientific objects
#' for a given experiment. It uses the \code{future::multisession} plan to
#' parallelize data fetching across multiple workers.
#'
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
#' A combined \code{tibble} containing the retrieved data.
#'
#' @details
#' The function temporarily switches the \code{future} execution plan to
#' \code{multisession} in order to enable parallel processing. The execution
#' plan is restored to \code{sequential} when the function exits, even if an
#' error occurs. The \code{output_dir} parameter is passed to downstream
#' functions to control where CSV files are written.
#'
#' @importFrom future plan multisession sequential
#' @export


getDataOs <- function(session, exp_name, df_os, output_dir= NULL) {
    plan(multisession)
    on.exit(plan(sequential), add = TRUE)

    getDataByOsUriVariable(session, exp_name, df_os, output_dir = output_dir)
    }
