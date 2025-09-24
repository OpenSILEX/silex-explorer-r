#' Récupère les données scientifiques pour une expérience donnée
#'
#' Cette fonction utilise le plan `future::multisession` pour paralléliser la récupération
#' des données liées à une liste d'objets scientifiques (`df_os`) pour une expérience donnée (`exp_name`).
#' En cas d'erreur, elle capture proprement l'exception et renvoie `NULL`.
#'
#' @param session Une liste contenant les informations de session, incluant l'URL GraphQL et le token d'authentification.
#' @param exp_name Nom ou identifiant de l'expérience.
#' @param df_os DataFrame contenant les objets scientifiques, avec au minimum une colonne `uri`.
#'
#' @return Un `tibble` combiné de toutes les données récupérées ou `NULL` en cas d'erreur.
#'
#' @importFrom future plan multisession sequential
#' @export
get_data_os <- function(session, exp_name, df_os) {
  tryCatch({
    plan(multisession)
    on.exit(plan(sequential), add = TRUE)  # Remise à zéro propre

    get_data_by_os_uri_variable(session, exp_name, df_os)

  }, error = function(e) {
    message("❌ Erreur pendant l'exécution de get_data_os : ", e$message)
    NULL
  })
}
