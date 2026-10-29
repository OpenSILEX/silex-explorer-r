#' Fetch Data by Chunk Using a GraphQL Query
#'
#' Queries a GraphQL API to retrieve data associated with scientific objects
#' corresponding to a chunk of URIs.
#'
#' @param chunk A character vector containing the URIs of the scientific objects
#'   to retrieve.
#' @param experience A character vector identifying the experiment(s) associated
#'   with the query.
#' @param session A connection object containing authentication information and
#'   the GraphQL endpoint URL.
#'
#' @return A data frame or tibble containing the retrieved scientific object data.
#'
#' @examples
#' \dontrun{
#' session <- login("user", "password", "https://example.com", "https://example.com/graphql")
#' fetchChunkData(c("uri1", "uri2"), c("exp1"), session)
#' }
#'
#' @importFrom httr POST add_headers content status_code
#' @importFrom tibble tibble
fetchChunkData <- function(chunk, experience, session) {
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

  res <- tryCatch(
    httr::POST(
      url = session$urlGraphql,
      httr::add_headers(
        Authorization = paste("Bearer", session$token)
      ),
      body = body,
      encode = "json"
    ),
    error = function(e) {
      stop(
        "Request failed for chunk [",
        paste(chunk, collapse = ", "),
        "]: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.null(res)) {
    stop(
      "No response received for chunk [",
      paste(chunk, collapse = ", "),
      "].",
      call. = FALSE
    )
  }

  if (httr::status_code(res) >= 400) {
    stop(
      "GraphQL request failed for chunk [",
      paste(chunk, collapse = ", "),
      "]. HTTP status: ",
      httr::status_code(res),
      call. = FALSE
    )
  }

  result <- httr::content(res, as = "parsed")
  sci_objs <- result$data$ScientificObject

  if (is.null(sci_objs) || length(sci_objs) == 0) {
    warning(
      "No scientific objects found for chunk [",
      paste(chunk, collapse = ", "),
      "].",
      call. = FALSE
    )
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
