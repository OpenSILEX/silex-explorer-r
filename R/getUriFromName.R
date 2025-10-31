#' Get URIs from Name
#'
#' Retrieves the URI(s) associated with a given name from the mapping.
#'
#' @param name Character string. The name to look up.
#'
#' @return Character vector of URI(s) associated with the name.
#'
#' @export
#'

getUrisFromName <- function(name) {
  if (is.null(.pkg_env$uri_name)) {
    stop("uri_name dataframe not initialized. Call initUriName() first.")
  }

  # Find matching URIs for the given name
  name_clean <- trimws(tolower(name))
  names_clean <- trimws(tolower(.pkg_env$uri_name$name))

  matching_uris <- .pkg_env$uri_name$uri[names_clean == name_clean]

  if (length(matching_uris) == 0) {
    warning(paste("No URI found for name:", name))
    return(character(0))
  }

  if (length(matching_uris) > 1) {
    warning(paste("Multiple URIs found for name:", name,
                  "Returning all matches"))
  }

  return(matching_uris)
}
