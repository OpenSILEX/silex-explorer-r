#' Clean a Group Identifier
#'
#' Removes trailing \code{"_nan"} suffixes (case-insensitive) at the end of a
#' group identifier.
#'
#' @param group_identifier Character string. Group identifier to be cleaned.
#'
#' @return A character string corresponding to the cleaned identifier.
#'   Returns an empty string if everything has been removed.
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
