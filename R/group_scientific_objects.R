#' Group Scientific Objects by Attribute Similarity
#'
#' This function groups scientific objects (rows of a data frame) according to
#' identical combinations of their attribute values.
#' Two objects belong to the same group if they share the same values across all
#' relevant attributes after normalization.
#'
#' The columns \code{uri} and \code{name} are excluded from the grouping process,
#' as they are assumed to be identifiers rather than discriminative attributes.
#'
#' @param df A data frame where each row represents a scientific object and
#'   columns represent object attributes.
#' @param output_dir Optional character string specifying a directory where a CSV
#'   summary of group sizes will be saved. If \code{NULL}, no file is written.
#'
#' @return
#' A named list of data frames.
#' Each element of the list corresponds to one group of similar objects.
#' The names of the list elements are the group identifiers.
#'
#' @details
#' The grouping process follows these steps:
#' \itemize{
#'   \item Columns \code{uri} and \code{name} are excluded from comparison.
#'   \item Columns with identical values for all rows are ignored, as they do not
#'   contribute to distinguishing objects.
#'   \item Missing values (\code{NA}) and empty strings are replaced with the
#'   string \code{"NaN"}.
#'   \item A group identifier is created by concatenating attribute values with
#'   underscores.
#'   \item Trailing \code{"_NaN"} suffixes at the end of an identifier are removed
#'   using \code{remove_trailing_nans_from_identifier()}.
#'   \item Objects whose attributes are all missing are assigned to a default
#'   group named \code{"NaN_group"}.
#' }
#'
#' If \code{output_dir} is provided, a CSV file named
#' \code{"group_summary.csv"} is created, listing each group identifier and the
#' number of objects it contains.
#'
#' @examples
#' \dontrun{
#' groups <- group_scientific_objects(df, output_dir = "results/")
#' names(groups)           # group identifiers
#' nrow(groups[[1]])       # number of objects in the first group
#' }
#'
#' @importFrom utils write.csv
#' @export


group_scientific_objects <- function(df, output_dir = NULL) {
  # Exclude 'uri' and 'label' columns from comparison
  columns_to_compare <- setdiff(colnames(df), c("uri", "name"))

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

  # Save summary to CSV if output_dir provided
  save_to_csv <- !is.null(output_dir)

  if (save_to_csv) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
    }
    csv_path <- file.path(output_dir, "group_summary.csv")


    tryCatch({
      write.csv(group_summary_df, csv_path, row.names = FALSE)
      message("OK: Group summary has been saved to  -> ", csv_path)
    }, error = function(e) {
      warning("WARNING: Failed to save groups : ", e$message)
    })
  }


  # Return the dictionary of groups
  return(group_dict)
}
