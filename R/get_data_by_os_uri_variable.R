#' Retrieve data by OS URI and variables
#'
#' Récupère les données scientifiques associées à une liste d'URIs d'objets scientifiques, en traitant les variables d'expérience.
#'
#' @param session Un objet de connexion contenant les informations d'authentification et l'URL GraphQL.
#' @param experience Un vecteur ou liste d'expériences à interroger.
#' @param df_os DataFrame contenant au moins une colonne 'uri' listant les objets scientifiques à interroger.
#' @param ls_var_exp Optionnel. DataFrame des variables par expérience. Si NULL ou vide, la fonction lsVarByExp sera appelée.
#'
#' @return Aucune valeur retournée explicitement, les données sont exportées via export_data_by_variable_to_csv().
#'
#' @importFrom purrr safely
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @export
get_data_by_os_uri_variable <- function(session, experience, df_os, ls_var_exp = NULL) {
  max_ids_per_request <- 40

  if (!"uri" %in% names(df_os)) {
    warning("DataFrame doesn't contain URI column.")
    return(NULL)
  }


  if (is.null(ls_var_exp) || nrow(ls_var_exp) == 0) {
    ls_var_exp <- lsVarByExp(session, experience)
  }

   label_query <- sprintf('
    query {
      Experiment(filter: {label: "%s"}, inferred: true) {
        startDate
      }
    }', experience)

  label_response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      "Content-Type" = "application/json"
    ),
    body = list(query = label_query),
    encode = "json",
    httr::timeout(60)
  )

  httr::stop_for_status(label_response)
  label_result <- httr::content(label_response, as = "parsed")

  if (length(label_result$data$Experiment) == 0) {
    stop("No experiment found with label: ", experience)
  }

  exp_data <- label_result$data$Experiment[[1]]
  start_date <- gsub("-", "_", substr(exp_data$startDate, 1, 10))
  clean_label <- gsub("[^A-Za-z0-9]", "_", experience)
  experiment_id <- paste0("EXP_", clean_label, "_", start_date)


  os_uris <- df_os$uri
  chunks <- chunk_list(os_uris, max_ids_per_request)

  # Utiliser safely pour capter les erreurs dans les appels
  safe_fetch <- purrr::safely(fetch_chunk_data, otherwise = tibble::tibble())

  results <- furrr::future_map(chunks, ~{
    out <- safe_fetch(.x, experiment_id, session)
    if (!is.null(out$error)) {
      message("❌ Error in chunk: ", paste(.x, collapse = ", "))
    }
    out$result
  }, .progress = TRUE)

  all_data <- dplyr::bind_rows(results)

  export_data_by_variable_to_csv(ls_var_exp, all_data)
}

