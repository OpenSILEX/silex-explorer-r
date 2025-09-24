#' Initialize URI-Name Mapping
#'
#' This function initializes the URI-Name mapping system by loading an existing
#' CSV file or creating a new one if it doesn't exist.
#'
#' @param csvFile Character string. Path to the CSV file for URI-name mappings.
#' Default is "uri_name.csv".
#'
#' @return A data frame containing the URI-name mappings.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize with default file
#' initUriName()
#'
#' # Initialize with custom file
#' initUriName("my_uri_mappings.csv")
#' }
initUriName <- function(csvFile = "uri_name.csv") {
  .pkg_env$uri_name_path <- csvFile

  if (file.exists(csvFile)) {
    .pkg_env$uri_name <- utils::read.csv(csvFile, stringsAsFactors = FALSE)
    message(sprintf("Loaded CSV file: %s", csvFile))
  } else {
    .pkg_env$uri_name <- data.frame(uri = character(), name = character(), stringsAsFactors = FALSE)
    utils::write.csv(.pkg_env$uri_name, csvFile, row.names = FALSE)
    message(sprintf("Created new CSV file: %s", csvFile))
  }

  return(.pkg_env$uri_name)
}
