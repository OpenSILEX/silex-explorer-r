#' Compare deux groupes d'objets scientifiques selon un facteur donné
#'
#' Cette fonction récupère les données pour deux groupes définis,
#' prépare un tableau tidy, puis affiche les courbes de comparaison
#' (trajectoires individuelles et moyennes ± écart-type).
#'
#' @param session Session RSession d’accessibilité aux données (via API).
#' @param group_dict Liste contenant les groupes sous forme de data.frames.
#' @param experiment_id ID ou nom de l’expérience concernée.
#' @param group1_id ID du premier groupe à comparer (doit exister dans `group_dict`).
#' @param group2_id ID du second groupe à comparer.
#' @param factor Le nom du facteur discriminant (ex: "genotype", "treatment").
#'
#' @return Un `data.frame` tidy contenant les données combinées prêtes à être exploitées.
#' @export
compare_groups_by_factor_level <- function(session, group_dict, experiment_id, group1_id, group2_id, factor) {
  df_compare <- prepare_data_for_comparison(group_dict, session, experiment_id, group1_id, group2_id, factor)
  plot_comparison(df_compare, factor)
  return(df_compare)
}
