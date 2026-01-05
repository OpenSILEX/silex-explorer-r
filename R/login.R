#' Login to OpenSILEX
#'
#' @param id User email
#' @param password Your password
#' @param instance Base instance URL (e.g., http://localhost:8080)
#' @param urlGraphql GraphQL URL endpoint
#'
#' @return A session token (or list) on success
#' @export
#'
#' @examples
#' # Exemple d'utilisation de la fonction login
#' session <- login(id = "ton_email@exemple.com",
#'                  password = "ton_mot_de_passe",
#'                  instance = "http://localhost:8080",
#'                  urlGraphql = "http://localhost:8080/graphql")
#' print(session$token)
login <- function(id, password, instance, urlGraphql, uri_csv = "uri_name.csv") {
  if (!all(is.character(c(id, password, instance, urlGraphql)))) {
    stop("All arguments must be character strings")
  }

  if (any(nchar(c(id, password, instance, urlGraphql)) == 0)) {
    stop("Empty strings are not allowed as arguments")
  }

  tryCatch({
    instance <- gsub("/$", "", instance)
    urlGraphql <- gsub("/$", "", urlGraphql)

    urlRest <- paste0(instance, "/rest")

    body <- list(
      identifier = id,
      password = password
    )

    response <- httr::POST(
      url = paste0(urlRest, "/security/authenticate"),
      body = body,
      encode = "json",
      httr::timeout(60),
      httr::add_headers("Content-Type" = "application/json")
    )

    httr::stop_for_status(response)
    content <- httr::content(response, "parsed")

    if (is.null(content$result$token)) {
      stop("No authentication token received in response")
    }
# Automatically load or initialize the URI-name mapping
if (!exists(".pkg_env", envir = .GlobalEnv)) {
  assign(".pkg_env", new.env(parent = emptyenv()), envir = .GlobalEnv)
}


    # Automatically load or initialize the URI-name mapping
    initUriName(uri_csv)

    structure(
      list(
        token = content$result$token,
        instance = instance,
        urlRest = urlRest,
        urlGraphql = urlGraphql,
        uri_csv = uri_csv
      ),
      class = "opensilex_connection"
    )

  }, error = function(e) {
    stop(paste("Authentication failed:", conditionMessage(e)))
  })
}
