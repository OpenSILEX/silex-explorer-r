#' Extraire un groupe d'objets scientifiques par ID
#'
#' Cette fonction extrait un sous-ensemble de données (`data.frame`) à partir d'une liste (dictionnaire)
#' indexée par identifiants de groupe. Elle peut aussi sauvegarder le groupe extrait dans un fichier `.csv`.
#'
#' @param group_dict Une liste nommée contenant des objets `data.frame`, typiquement issue d’un split par groupe.
#' @param group_id Le nom du groupe à extraire (doit correspondre à un nom de `group_dict`).
#' @param output_filename (optionnel) Chemin vers un fichier `.csv` où sauvegarder les données du groupe extrait.
#' Si `NULL`, aucune sauvegarde n'est effectuée.
#'
#' @return Le `data.frame` correspondant au groupe extrait.
#'
#' @export
extract_group_objects <- function(group_dict, group_id, output_filename = NULL) {
  # Vérifie si le group_id existe dans les noms du dictionnaire
  if (group_id %in% names(group_dict)) {
    group_df <- group_dict[[group_id]]

    if (!is.null(output_filename)) {
      write.csv(group_df, output_filename, row.names = FALSE)
      message(sprintf("✅ Group '%s' saved to %s", group_id, output_filename))
    } else {
      message(sprintf("ℹ️ Group '%s' extracted. No file was saved.", group_id))
    }

    return(group_df)
  } else {
    stop(sprintf("❌ Invalid group ID: '%s'. Available IDs are: %s",
                 group_id, paste(names(group_dict), collapse = ", ")))
  }
}
