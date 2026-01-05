#' Compare two groups of scientific objects based on a given factor
#'
#' This function retrieves measurement data for two predefined groups of
#' scientific objects, prepares a tidy data table, and then displays
#' comparison plots including individual trajectories and group-level
#' mean ± standard deviation over time.
#'
#' @param session Session object providing access to the data API
#'   (authentication token and endpoint).
#' @param group_dict Named list (dictionary) containing the groups as
#'   `data.frame`s of scientific objects. Group identifiers must correspond
#'   to the names of the list.
#' @param experiment_id Name or identifier of the experiment of interest.
#' @param group1_id Identifier of the first group to compare
#'   (must exist in `group_dict`).
#' @param group2_id Identifier of the second group to compare
#'   (must exist in `group_dict`).
#' @param factor Name of the experimental factor used to discriminate the
#'   groups and on which the comparison is based
#'   (e.g. `"genotype"`, `"treatment"`).
#' @param output_dir Optional path to a directory where generated comparison
#'   plots will be saved. If NULL, plots are only displayed.
#'
#' @return A tidy `data.frame` combining data from both groups, ready for
#'   downstream analysis and visualization.
#'
#' @export
compare_groups_by_factor_level <- function(session,
                                           group_dict,
                                           experiment_id,
                                           group1_id,
                                           group2_id,
                                           factor,
                                           output_dir = NULL) {

  # ---- Check group IDs ----
  if (!(group1_id %in% names(group_dict))) {
    stop("group1_id '", group1_id, "' not found in group_dict")
  }
  if (!(group2_id %in% names(group_dict))) {
    stop("group2_id '", group2_id, "' not found in group_dict")
  }

  # ---- Extract scientific objects ----
  df_os1 <- extract_group_objects(group_dict, group1_id)
  df_os2 <- extract_group_objects(group_dict, group2_id)

  # ---- Check that groups are not empty ----
  if (nrow(df_os1) == 0) stop("Group1 data is empty")
  if (nrow(df_os2) == 0) stop("Group2 data is empty")

  # ---- Check that factor exists in group data ----
  if (!(factor %in% colnames(df_os1))) {
    stop("Factor '", factor, "' not found in group1 data")
  }
  if (!(factor %in% colnames(df_os2))) {
    stop("Factor '", factor, "' not found in group2 data")
  }

  # ---- Prepare tidy data ----
  df_compare <- prepare_data_for_comparison(
    group_dict = group_dict,
    session = session,
    experiment_name = experiment_id,
    group1_id = group1_id,
    group2_id = group2_id,
    factor = factor
  )

  # ---- Plot comparisons ----
  plot_comparison(df_compare, factor = factor, output_dir = output_dir)

  return(df_compare)
}
