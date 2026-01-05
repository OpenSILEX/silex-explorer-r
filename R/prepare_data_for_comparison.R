#' Prepare tidy data for comparison between two groups
#'
#' This function extracts scientific objects belonging to two groups,
#' retrieves their measured variables via the API, adds group and factor
#' information, and combines everything into a tidy (long-format)
#' `data.frame` ready for visualization or statistical analysis.
#'
#' @param group_dict Named list (dictionary) where:
#'   \itemize{
#'     \item names correspond to group identifiers;
#'     \item each element is a `data.frame` containing the scientific objects
#'       belonging to that group.
#'   }
#' @param session Session object containing the authentication token and
#'   GraphQL endpoint URL required for API queries.
#' @param experiment_name Name or identifier of the experiment in the API.
#' @param group1_id Identifier of the first group (must match a name in
#'   `group_dict`).
#' @param group2_id Identifier of the second group (must match a name in
#'   `group_dict`).
#' @param factor Name of the experimental factor used to define the groups
#'   and on which the comparison is performed (e.g. `"genotype"`,
#'   `"treatment"`).
#'
#' @return A tidy `data.frame` combining data from both groups, with the
#'   following columns:
#'   \describe{
#'     \item{Date}{Measurement date}
#'     \item{URI}{Scientific object identifier}
#'     \item{Groupe}{Group label constructed from the factor and its value}
#'     \item{factor}{Value of the factor used for the comparison}
#'     \item{Variable}{Name of the measured variable}
#'     \item{Valeur}{Numeric value of the measurement}
#'   }
#'
#' @details
#' The function assumes that:
#' \itemize{
#'   \item each group contains a single unique value for the comparison factor levels;
#'   \item the same set of variables is available for both groups;
#'   \item the retrieved data include `Date` and `URI` columns.
#' }
#'
#' @export
prepare_data_for_comparison <- function(group_dict, session, experiment_name, group1_id, group2_id, factor) {

  # Extraire les objets scientifiques des deux groupes
  df_os1 <- extract_group_objects(group_dict, group1_id)
  df_os2 <- extract_group_objects(group_dict, group2_id)

  # Récupérer les données des variables pour chaque groupe
  df_var1 <- get_data_by_os_uri_variable(session, experiment_name, df_os1)
  df_var2 <- get_data_by_os_uri_variable(session, experiment_name, df_os2)

  # Ajouter l'information de groupe et de facteur à chaque variable
  for (var_name in names(df_var1)) {
    df_var1[[var_name]]$Groupe <- paste0(tools::toTitleCase(factor), " ", df_os1[[factor]][1])
    df_var1[[var_name]][[factor]] <- df_os1[[factor]][1]

    df_var2[[var_name]]$Groupe <- paste0(tools::toTitleCase(factor), " ", df_os2[[factor]][1])
    df_var2[[var_name]][[factor]] <- df_os2[[factor]][1]
  }

  # Fusionner les données dans un format tidy
  df_list <- list()
  for (var_name in names(df_var1)) {
    clean_var <- sub("^df_", "", var_name)

    temp1 <- df_var1[[var_name]] %>%
      dplyr::mutate(Variable = clean_var) %>%
      dplyr::select(Date, URI, Groupe, all_of(factor), Variable, !!sym(clean_var)) %>%
      dplyr::rename(Valeur = !!sym(clean_var))

    temp2 <- df_var2[[var_name]] %>%
      dplyr::mutate(Variable = clean_var) %>%
      dplyr::select(Date, URI, Groupe, all_of(factor), Variable, !!sym(clean_var)) %>%
      dplyr::rename(Valeur = !!sym(clean_var))

    df_list <- append(df_list, list(temp1, temp2))
  }

  df_final <- bind_rows(df_list)
  return(df_final)
}
