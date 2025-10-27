#' Check URI-Name Consistency
#'
#' Checks for inconsistencies in the uri_name mapping:
#' - URIs associated with multiple names
#' - Names associated with multiple URIs
#' Displays warnings but does not modify the data.
#'
#' @export
UriCons <- function() {
  if (is.null(.pkg_env$uri_name)) {
    stop("The uri_name data frame is not initialized. Call initUriName() first.")
  }

  uri_name_df <- .pkg_env$uri_name

  # URIs with multiple names
  uri_counts <- uri_name_df %>%
    dplyr::group_by(.data$uri) %>%
    dplyr::summarise(n_names = dplyr::n_distinct(.data$name), .groups = "drop") %>%
    dplyr::filter(n_names > 1)

  if (nrow(uri_counts) > 0) {
    for (u in uri_counts$uri) {
      names_for_uri <- uri_name_df %>%
        dplyr::filter(.data$uri == u) %>%
        dplyr::pull(.data$name) %>%
        unique()
      warning(sprintf("⚠️ Inconsistency: URI '%s' is associated with multiple names: %s.",
                      u, paste(names_for_uri, collapse = ", ")))
    }
  }

  # Names with multiple URIs
  name_counts <- uri_name_df %>%
    dplyr::group_by(.data$name) %>%
    dplyr::summarise(n_uris = dplyr::n_distinct(.data$uri), .groups = "drop") %>%
    dplyr::filter(n_uris > 1)

  if (nrow(name_counts) > 0) {
    for (n in name_counts$name) {
      uris_for_name <- uri_name_df %>%
        dplyr::filter(.data$name == n) %>%
        dplyr::pull(.data$uri) %>%
        unique()
      warning(sprintf("⚠️ Inconsistency: Name '%s' is associated with multiple URIs: %s.",
                      n, paste(uris_for_name, collapse = ", ")))
    }
  }

  if (nrow(uri_counts) == 0 && nrow(name_counts) == 0) {
    message("No inconsistencies found in the URI-Name mappings.")
  }
}
