#' Get Names from URI
#'
#' Retrieves the name(s) associated with a given URI from the mapping.
#'
#' @param uri Character string. The URI to look up.
#'
#' @return Character vector of name(s) associated with the URI.
#'
#' @export
#'

getNamesFromUri <- function(uri) {
  if (is.null(.pkg_env$uri_name)) {
    stop("uri_name dataframe not initialized. Call initUriName() first.")
  }

  # Find matching names for the given URI
  matching_names <- .pkg_env$uri_name$name[.pkg_env$uri_name$uri == uri]

  if (length(matching_names) == 0) {
    warning(paste("No name found for URI:", uri))
    return(character(0))
  }

  if (length(matching_names) > 1) {
    warning(paste("Multiple names found for URI:", uri,
                  "Returning all matches"))
  }

  return(matching_names)
}
