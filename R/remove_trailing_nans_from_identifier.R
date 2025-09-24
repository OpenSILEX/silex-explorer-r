#' Nettoyer un identifiant de groupe
#'
#' Supprime les suffixes \code{"_nan"} (non sensibles à la casse) à la fin d’un identifiant de groupe.
#'
#' @param group_identifier Chaîne de caractères. Identifiant du groupe à nettoyer.
#'
#' @return Une chaîne de caractères correspondant à l’identifiant nettoyé. Retourne une chaîne vide si tout a été supprimé.
#'
#' @examples
#' remove_trailing_nans_from_identifier("WD_ZM1289_nan")  # "WD_ZM1289"
#' remove_trailing_nans_from_identifier("WW_123_nan_nan") # "WW_123"
#'
#' @export

remove_trailing_nans_from_identifier <- function(group_identifier) {
  # Séparer l'identifiant en parties
  parts <- unlist(strsplit(group_identifier, "_"))

  # Supprimer les 'NaN' (insensible à la casse) à la fin
  while (length(parts) > 0 && tolower(tail(parts, 1)) == "nan") {
    parts <- parts[-length(parts)]
  }

  # Rejoindre les parties restantes
  cleaned_identifier <- paste(parts, collapse = "_")

  # Retourner une chaîne vide si tout a été supprimé
  return(cleaned_identifier)
}
