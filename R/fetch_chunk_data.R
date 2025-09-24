#' Fetch data by chunk using GraphQL query
#'
#' Interroge une API GraphQL pour récupérer les données des objets scientifiques correspondant à un chunk d'URIs.
#'
#' @param chunk Un vecteur de chaînes de caractères représentant les URIs des objets scientifiques à récupérer.
#' @param experience Un vecteur de chaînes de caractères indiquant les expériences concernées par la requête.
#' @param session Un objet de connexion contenant les informations d'authentification et l'URL GraphQL.
#'
#' @return Un data.frame (ou tibble) contenant les données des objets scientifiques récupérées.
#'
#' @examples
#' \dontrun{
#' session <- login("user", "password", "https://example.com", "https://example.com/graphql")
#' fetch_chunk_data(c("uri1", "uri2"), c("exp1"), session)
#' }
#'
#' @importFrom httr POST add_headers content status_code
#' @importFrom tibble tibble
#' @export
fetch_chunk_data <- function(chunk, experience, session) {
  graphql_query <- '
    query ScientificObject($experience: [DataSource!]!, $osUris: [ID]) {
      ScientificObject(
        inferred: true,
        Experience: $experience,
        filter: {_id: $osUris}
      ) {
        data {
          target
          variable
          value
          date
        }
      }
    }'

  body <- list(
    query = graphql_query,
    variables = list(
      experience = experience,
      osUris = chunk
    )
  )

  res <- tryCatch({
    httr::POST(
      url = session$urlGraphql,
      httr::add_headers(Authorization = paste("Bearer", session$token)),
      body = body,
      encode = "json"
    )
  }, error = function(e) {
    message("❌ Request failed for chunk: ", paste(chunk, collapse = ", "))
    message("🧨 Error: ", e$message)
    return(NULL)
  })

  if (is.null(res) || httr::status_code(res) >= 400) {
    warning("⚠️ GraphQL request failed. Status: ", httr::status_code(res))
    return(tibble::tibble())
  }

  result <- httr::content(res, as = "parsed")
  sci_objs <- result$data$ScientificObject

  if (is.null(sci_objs) || length(sci_objs) == 0) {
    warning("🚫 No scientific objects found for chunk: ", paste(chunk, collapse = ", "))
    return(tibble::tibble())
  }

  data_list <- lapply(sci_objs, function(obj) {
    if (!is.null(obj$data) && length(obj$data) > 0) {
      do.call(rbind, lapply(obj$data, function(record) {
        data.frame(
          target   = if (!is.null(record$target)) record$target else NA,
          variable = if (!is.null(record$variable)) record$variable else NA,
          value    = if (!is.null(record$value)) as.numeric(record$value) else NA,
          date     = if (!is.null(record$date)) {
            as.POSIXct(record$date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
          } else {
            as.POSIXct(NA)
          },
          stringsAsFactors = FALSE
        )
      }))
    } else {
      NULL
    }
  })

  data_list <- Filter(Negate(is.null), data_list)

  if (length(data_list) > 0) {
    final_df <- do.call(rbind, data_list)
  } else {
    return(tibble::tibble())
  }

  return(final_df)
}
