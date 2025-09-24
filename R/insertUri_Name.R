#' Insert URI-Name Mapping
#'
#' Adds a new URI-name pair to the mapping if the URI doesn't already exist.
#'
#' @param uri Character string. The URI to add.
#' @param name Character string. The name associated with the URI.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' initUriName()
#' insertUri_Name("http://example.com/exp1", "Experiment 1")
#' }
insertUri_Name <- function(uri, name) {
  if (is.null(.pkg_env$uri_name)) {
    stop("uri_name dataframe not initialized. Call initUriName() first.")
  }

  if (!uri %in% .pkg_env$uri_name$uri) {
    new_entry <- data.frame(
      uri = uri,
      name = name,
      stringsAsFactors = FALSE
    )

    .pkg_env$uri_name <- rbind(.pkg_env$uri_name, new_entry)

    utils::write.csv(.pkg_env$uri_name, file = .pkg_env$uri_name_path, row.names = FALSE, quote = FALSE)
    message(sprintf("Added: %s -> %s", uri, name))
  }
}
