#' Extract a Group of Scientific Objects by Group ID
#'
#' This function extracts a subset of data (a \code{data.frame}) from a named
#' list of grouped objects, typically produced by a grouping or splitting
#' procedure. The extracted group can optionally be saved to a CSV file.
#'
#' @param group_dict A named list containing \code{data.frame} objects, usually
#'   resulting from a grouping or splitting operation.
#' @param group_id Character string specifying the group identifier to extract.
#'   It must match one of the names in \code{group_dict}.
#' @param output_dir Optional character string specifying a directory where the
#'   extracted group will be saved as a \code{.csv} file. If \code{NULL}, no file
#'   is written.
#'
#' @return
#' A \code{data.frame} corresponding to the extracted group.
#'
#' @details
#' If \code{output_dir} is provided, the function creates the directory if it does
#' not exist and saves the extracted group to a CSV file named
#' \code{"group_<group_id>.csv"}.
#'
#' @examples
#' \dontrun{
#' group_of_os <- extract_group_objects(
#'   group_summary,
#'   group_id = "WD_ZM4351_SeedLot_Zea mays",
#'   output_dir = "temp_files"
#' )
#' }
#'
#' @importFrom utils write.csv
#' @export

extract_group_objects <- function(group_dict, group_id, output_dir = NULL) {
  # Vérifie si le group_id existe dans les noms du dictionnaire
  if (group_id %in% names(group_dict)) {
    group_df <- group_dict[[group_id]]

    # Save summary to CSV if filename provided
    save_to_csv <- !is.null(output_dir)

    if (save_to_csv) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
      }
      csv_path <- file.path(output_dir, paste0("group_",group_id, ".csv"))


      tryCatch({
        write.csv(group_df, csv_path, row.names = FALSE)
        message(sprintf("✅ Group '%s' saved to %s", group_id,  csv_path))
      }, error = function(e) {
        warning("WARNING: Failed to save group : ", e$message)
      })
    }

    return(group_df)
  } else {
    stop(sprintf("❌ Invalid group ID: '%s'. Available IDs are: %s",
                 group_id, paste(names(group_dict), collapse = ", ")))
  }
}
