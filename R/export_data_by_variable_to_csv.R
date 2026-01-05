#' Export Data by Variable to CSV Files
#'
#' This function exports scientific data grouped by variable into separate CSV
#' files. Each variable is written to its own CSV file, named after the variable.
#' The function also returns the exported data as a list of data frames.
#'
#' @param var_exp A data frame containing variable metadata, with at least the
#'   columns \code{uri} and \code{name}.
#' @param data A data frame containing scientific data, with at least the
#'   columns \code{variable}, \code{target}, \code{value}, and \code{date}.
#' @param output_dir Optional character string specifying the directory where
#'   CSV files will be written. If \code{NULL}, no files are created.
#'
#' @return
#' A named list of data frames, where each element corresponds to the filtered
#' data of a single variable.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Splits the input data by variable URI.
#'   \item Removes rows with missing \code{target}, \code{value}, or \code{date}.
#'   \item Builds a clean data frame for each variable with targets, values, and dates.
#'   \item Saves each variable-specific data frame to a CSV file named
#'   \code{"<variable_name>_data.csv"} when \code{output_dir} is provided.
#' }
#'
#' Variables are named using the metadata provided in \code{var_exp}. If no name
#' is found for a variable URI, the last segment of the URI is used instead.
#'
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom readr write_csv
#' @importFrom glue glue
#' @export


export_data_by_variable_to_csv <- function(var_exp, data, output_dir) {
  if (nrow(var_exp) == 0 || nrow(data) == 0) {
    warning("No variable metadata or data provided.")
    return(NULL)
  }

  uri_to_name <- setNames(var_exp$name, var_exp$uri)

  data_by_var <- split(data, data$variable)
  dataframes <- list()

  for (var_uri in names(data_by_var)) {
    var_data <- data_by_var[[var_uri]]
    var_name <- uri_to_name[[var_uri]] %||% tail(strsplit(var_uri, "/")[[1]], 1)

    # Nettoyage : garder uniquement les lignes complètes
    var_data <- var_data %>%
      filter(!is.na(target), !is.na(value), !is.na(date))

    # Sauter si aucune donnée valide
    if (nrow(var_data) == 0) next

    df <- tibble(
      URI = var_data$target,
      !!var_name := var_data$value,
      Date = var_data$date
    )

    dataframes[[paste0("df_", var_name)]] <- df

    if (!is.null(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(df, file.path(output_dir, paste0(var_name, "_data.csv")))
      message(glue::glue("✅ Data for '{var_name}' saved to {file.path( output_dir ,paste0(var_name, '_data.csv'))}"))
    }
  }

  return(dataframes)
}
