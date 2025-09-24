#' Export data by variable to CSV files
#'
#' Cette fonction exporte les données scientifiques groupées par variable dans des fichiers CSV séparés.
#' Chaque variable aura son propre fichier CSV nommé d'après la variable.
#'
#' @param var_exp DataFrame contenant la metadata des variables, avec au moins les colonnes `URI` et `Name`.
#' @param data DataFrame contenant les données scientifiques, avec au moins les colonnes `variable`, `target`, `value` et `date`.
#' @param csv_filepath Chemin du dossier où les fichiers CSV seront enregistrés. Par défaut : "/home/abidri/Bureau/test_R/csv_groups".
#'
#' @return Une liste de DataFrames, chaque élément correspondant aux données filtrées d'une variable.
#'
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom readr write_csv
#' @importFrom glue glue
#' @export
export_data_by_variable_to_csv <- function(var_exp, data) {
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

    if (!is.null(csv_filepath)) {
      dir.create(csv_filepath, recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(df, file.path(csv_filepath, paste0(var_name, "_data.csv")))
      message(glue::glue("✅ Data for '{var_name}' saved to {file.path(csv_filepath, paste0(var_name, '_data.csv'))}"))
    }
  }

  return(dataframes)
}
