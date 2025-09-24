#' Check URI-Name Consistency
#'
#' Displays duplicate names in the URI-name mapping without modifying them.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' initUriName()
#' UriCons()
#' }
UriCons <- function() {
  if (is.null(.pkg_env$uri_name)) {
    stop("The uri_name data frame is not initialized. Call initUriName() first.")
  }

  name_duplicates <- .pkg_env$uri_name %>%
    dplyr::group_by(.data$name) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(.data$name)

  if (nrow(name_duplicates) > 0) {
    message("Duplicate names detected:")

    duplicate_groups <- name_duplicates %>%
      dplyr::group_by(.data$name) %>%
      dplyr::group_split()

    for (group in duplicate_groups) {
      message("\nDuplicate entries for name: ", group$name[1])
      for (i in seq_len(nrow(group))) {
        cat(sprintf("%d. URI: %s, Name: %s\n", i, group$uri[i], group$name[i]))
      }
    }
  } else {
    message("No duplicate names found in the URI-Name mappings.")
  }
}
