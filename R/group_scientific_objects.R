#' Group Scientific Objects by Similar Attributes
#'
#' This function groups scientific objects in a data frame by their attribute values,
#' excluding 'uri' and 'label' columns, and creates group identifiers.
#' It also optionally saves a summary CSV file listing groups and their sizes.
#'
#' @param df A data frame containing scientific objects data.
#' @param output_filename Optional character string specifying the path to save the group summary CSV.
#'
#' @return A named list where each element is a data frame corresponding to a group of objects.
#'
#' @details
#' - Columns with identical values for all rows are excluded from grouping.
#' - Empty strings and NA values are replaced with 'NaN' for grouping.
#' - Group identifiers have trailing '_NaN' suffixes removed.
#' - Groups with only 'NaN' values are labeled as 'NaN_group'.
#'
#' @importFrom utils write.csv
#' @export

group_scientific_objects <- function(df, output_filename = NULL) {
  # Exclude 'uri' and 'label' columns from comparison
  columns_to_compare <- setdiff(colnames(df), c("uri", "label"))

  # Exclude columns where all values are the same
  columns_to_compare <- columns_to_compare[sapply(df[columns_to_compare], function(x) length(unique(x))) > 1]

  # Convert all values in the relevant columns to strings, replacing "" and NA with 'NaN'
  df[columns_to_compare] <- lapply(df[columns_to_compare], function(x) {
    x[is.na(x) | x == ""] <- "NaN"
    as.character(x)
  })

  # Create a unique identifier for each combination of values in the columns to compare
  df$group_identifier <- apply(df[columns_to_compare], 1, function(row) paste(row, collapse = "_"))

  # Clean identifiers by removing trailing NaNs
  df$group_identifier <- sapply(df$group_identifier, remove_trailing_nans_from_identifier)

  # Assign default identifier if group identifier is empty
  df$group_identifier[df$group_identifier == ""] <- "NaN_group"

  # Create a dictionary to store groups and their corresponding sub-data frames
  group_dict <- list()

  # Iterate over each unique group identifier
  unique_groups <- unique(df$group_identifier)
  for (group in unique_groups) {
    group_df <- df[df$group_identifier == group, , drop = FALSE]
    group_dict[[group]] <- group_df[, !colnames(group_df) %in% "group_identifier"]
  }

  # Prepare summary of group sizes
  group_data <- list()
  for (group in names(group_dict)) {
    group_data[[length(group_data) + 1]] <- c(group, nrow(group_dict[[group]]))
  }

  group_summary_df <- do.call(rbind, group_data)
  colnames(group_summary_df) <- c("Group", "Number of Elements")

  # Save summary to CSV if filename provided
  if (!is.null(output_filename)) {
    write.csv(group_summary_df, output_filename, row.names = FALSE)
    cat(paste("Group summary has been saved to", output_filename, "\n"))
  }

  # Return the dictionary of groups
  return(group_dict)
}
