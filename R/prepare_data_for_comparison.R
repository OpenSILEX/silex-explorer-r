#' Préparer les données tidy pour comparaison entre deux groupes
#'
#' Cette fonction extrait les données de deux groupes d’objets scientifiques,
#' récupère leurs mesures, y ajoute les informations de groupe et de facteur,
#' puis retourne un `data.frame` en format long (`tidy`) prêt pour visualisation ou analyse.
#'
#' @param group_dict Liste nommée de groupes (`data.frame`), généralement issue d’un split par facteur.
#' @param session Objet de session avec token et URL GraphQL pour les requêtes.
#' @param experiment_name Nom ou identifiant de l’expérience dans l’API.
#' @param group1_id Identifiant du premier groupe (nom dans `group_dict`).
#' @param group2_id Identifiant du second groupe.
#' @param factor Nom de la colonne utilisée pour définir les groupes (ex. `"genotype"`).
#'
#' @return Un `data.frame` tidy combinant les données des deux groupes, avec colonnes :
#' `Date`, `URI`, `Groupe`, `factor`, `Variable`, `Valeur`.
#'
#' @export
prepare_data_for_comparison <- function(group_dict, session, experiment_name, group1_id, group2_id, factor) {

  # Extraire les objets scientifiques des deux groupes
  df_os1 <- extract_group_objects(group_dict, group1_id, paste0("group_", group1_id, ".csv"))
  df_os2 <- extract_group_objects(group_dict, group2_id, paste0("group_", group2_id, ".csv"))

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
      mutate(Variable = clean_var) %>%
      select(Date, URI, Groupe, all_of(factor), Variable, !!sym(clean_var)) %>%
      rename(Valeur = !!sym(clean_var))

    temp2 <- df_var2[[var_name]] %>%
      mutate(Variable = clean_var) %>%
      select(Date, URI, Groupe, all_of(factor), Variable, !!sym(clean_var)) %>%
      rename(Valeur = !!sym(clean_var))

    df_list <- append(df_list, list(temp1, temp2))
  }

  df_final <- bind_rows(df_list)
  return(df_final)
}
