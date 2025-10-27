#' Insert multiple URI-name pairs into the uri_name mapping.
#'
#' This function inserts multiple URI-name pairs at once into the uri_name mapping
#' (stored in `.pkg_env$uri_name`). It ensures that no duplicate URI-name pairs are inserted.
#' It also checks for consistency (URIs with multiple names or names with multiple URIs)
#' after insertion.
#'
#' @param uri_name_pairs A DataFrame containing 'uri' and 'name' columns with the new URI-name pairs to be added.
#'
#' @export
insertUri_Name <- function(uri_name_pairs) {
  # Check if the uri_name mapping is initialized
  if (is.null(.pkg_env$uri_name)) {
    stop("uri_name dataframe not initialized. Call initUriName() first.")
  }

  # Validate that the input is a DataFrame with 'uri' and 'name' columns
  if (!is.data.frame(uri_name_pairs) || !all(c("uri", "name") %in% colnames(uri_name_pairs))) {
    stop("Invalid input: uri_name_pairs must be a DataFrame with 'uri' and 'name' columns.")
  }

  # Combine existing and new data
  combined_data <- dplyr::bind_rows(.pkg_env$uri_name, uri_name_pairs)

  # Remove duplicate URI-name pairs (keep first)
  combined_data <- dplyr::distinct(combined_data, uri, name, .keep_all = TRUE)

  # Identify which entries are new (not already in the existing uri_name)
  existing_keys <- paste(.pkg_env$uri_name$uri, .pkg_env$uri_name$name)
  combined_keys <- paste(combined_data$uri, combined_data$name)
  new_entries <- combined_data[!combined_keys %in% existing_keys, ]

  # Append only unique new entries
  if (nrow(new_entries) > 0) {
    # Update the uri_name mapping with the new unique entries
    .pkg_env$uri_name <- dplyr::bind_rows(.pkg_env$uri_name, new_entries)

    # Save for persistence
    utils::write.csv(.pkg_env$uri_name, file = .pkg_env$uri_name_path, row.names = FALSE, quote = FALSE)

    # Count the number of new entries added
    added_count <- nrow(new_entries)
    message(sprintf("Added %d new URI-name pairs.", added_count))
  } else {
    message("No new URI-name pairs to add.")
  }

  # Check consistency once after all entries are added
  UriCons()
}
