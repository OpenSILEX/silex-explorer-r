#' Retrieve environmental data associated with a device
#'
#' This function retrieves environmental measurements associated with a given
#' device URI. Results can be filtered by date range and by variable URIs.
#' Data can optionally be exported as CSV files, with one file per variable.
#'
#' @param session An opensilex_connection object.
#' @param device_uri Character, URI of the device.
#' @param date_beginning Character or NULL, start date filter
#'   (YYYY-MM-DD or ISO format).
#' @param date_end Character or NULL, end date filter
#'   (YYYY-MM-DD or ISO format).
#' @param variable_uris Character vector or NULL, URIs of variables to filter.
#' @param output_dir Character or NULL, directory where CSV files will be saved
#'   (one CSV per variable).
#'
#' @return A named list of tibbles. Each list element corresponds to one
#'   variable URI and contains:
#'   \itemize{
#'     \item Date: measurement timestamp
#'     \item Value: measured value
#'   }
#'
#' @examples
#' \dontrun{
#' env_data <- lsEnvDataByDevice(
#'   session = session,
#'   device_uri = "http://phenome.inrae.fr/m3p/id/device/aria_hr1_p",
#'   variable_uris = c(
#'     "http://phenome.inrae.fr/m3p/id/variable/ev000035",
#'     "http://phenome.inrae.fr/m3p/id/variable/ev000034"
#'   ),
#'   date_beginning = "2017-04-16",
#'   date_end = "2017-04-18",
#'   output_dir = "device_data"
#' )
#' }
#'
#' @export
lsEnvDataByDevice <- function(session,
                              device_uri,
                              date_beginning = NULL,
                              date_end = NULL,
                              variable_uris = NULL,
                              output_dir = NULL) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  save_to_csv <- !is.null(output_dir)

  if (save_to_csv && !dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE
    )
  }

  `%||%` <- function(a, b) {
    if (!is.null(a)) a else b
  }

  extract_uri_id <- function(uri) {
    uri <- sub("/+$", "", uri)

    gsub(
      "[^A-Za-z0-9_-]+",
      "_",
      basename(uri)
    )
  }

  #----------------------------------------------------------
  # Build date filter
  #----------------------------------------------------------

  date_filter <- list()

  if (!is.null(date_beginning)) {
    date_filter$gte <- if (grepl("T", date_beginning, fixed = TRUE)) {
      date_beginning
    } else {
      paste0(date_beginning, "T00:00:00.000Z")
    }
  }

  if (!is.null(date_end)) {
    date_filter$lte <- if (grepl("T", date_end, fixed = TRUE)) {
      date_end
    } else {
      paste0(date_end, "T23:59:59.999Z")
    }
  }

  #----------------------------------------------------------
  # Build GraphQL filter
  #----------------------------------------------------------

  filter_input <- list(
    provenance = list(
      provWasAssociatedWith = list(
        uri = device_uri
      )
    )
  )

  operators <- list()

  if (length(date_filter) > 0) {
    operators$date <- date_filter
  }

  if (!is.null(variable_uris) && length(variable_uris) > 0) {
    variable_uris <- unique(as.character(variable_uris))

    operators$variable <- list(
      `in` = as.list(variable_uris)
    )
  }

  if (length(operators) > 0) {
    filter_input$`_operators` <- operators
  }

  #----------------------------------------------------------
  # GraphQL query
  #----------------------------------------------------------

  query_data <- '
    query GetEnvironmentalDataByDevice(
      $filter: FilterFindManyDataInput,
      $page: Int,
      $perPage: Int
    ) {
      Data_pagination(
        filter: $filter,
        page: $page,
        perPage: $perPage
      ) {
        items {
          variable
          value
          date
        }
        pageInfo {
          hasNextPage
        }
      }
    }
  '

  per_page <- 10000
  page <- 1
  data_items <- list()

  #----------------------------------------------------------
  # Retrieve all pages
  #----------------------------------------------------------

  repeat {

    response <- httr::POST(
      url = session$urlGraphql,
      body = list(
        query = query_data,
        variables = list(
          filter = filter_input,
          page = page,
          perPage = per_page
        )
      ),
      encode = "json",
      httr::add_headers(
        Authorization = paste("Bearer", session$token)
      )
    )

    if (httr::http_error(response)) {
      cat(
        httr::content(
          response,
          as = "text",
          encoding = "UTF-8"
        ),
        "\n"
      )

      httr::stop_for_status(response)
    }

    result <- httr::content(
      response,
      as = "parsed",
      encoding = "UTF-8"
    )

    if (!is.null(result$errors) && length(result$errors) > 0) {
      error_messages <- purrr::map_chr(
        result$errors,
        function(error) {
          error$message %||% "Unknown GraphQL error"
        }
      )

      stop(
        "GraphQL query failed: ",
        paste(error_messages, collapse = " | ")
      )
    }

    pagination <- result$data$Data_pagination

    if (is.null(pagination)) {
      stop("The GraphQL response does not contain Data_pagination.")
    }

    page_items <- pagination$items %||% list()
    page_info <- pagination$pageInfo

    if (is.null(page_info)) {
      stop("The GraphQL response does not contain pageInfo.")
    }

    if (length(page_items) > 0) {
      data_items <- c(
        data_items,
        page_items
      )
    }

    message(
      "Page ",
      page,
      " retrieved: ",
      length(page_items),
      " record(s), ",
      length(data_items),
      " record(s) in total"
    )

    if (!isTRUE(page_info$hasNextPage)) {
      break
    }

    page <- page + 1
  }

  if (length(data_items) == 0) {
    warning("No environmental data found for the given parameters.")
    return(list())
  }

  #----------------------------------------------------------
  # Process retrieved data
  #----------------------------------------------------------

  df_all <- purrr::map_dfr(
    data_items,
    function(item) {
      tibble::tibble(
        VariableURI = item$variable %||% NA_character_,
        Date = item$date %||% NA_character_,
        Value = item$value %||% NA
      )
    }
  ) |>
    dplyr::filter(
      !is.na(.data$VariableURI)
    ) |>
    dplyr::arrange(
      .data$VariableURI,
      .data$Date
    )

  if (nrow(df_all) == 0) {
    warning("No environmental data available after processing.")
    return(list())
  }

  #----------------------------------------------------------
  # Split data by variable
  #----------------------------------------------------------

  split_data <- split(
    df_all,
    df_all$VariableURI
  )

  device_id <- extract_uri_id(device_uri)
  results <- list()

  for (variable_uri in names(split_data)) {

    variable_data <- split_data[[variable_uri]] |>
      dplyr::select(
        .data$Date,
        .data$Value
      )

    variable_id <- extract_uri_id(variable_uri)

    results[[variable_id]] <- variable_data

    #--------------------------------------------------------
    # Export CSV
    #--------------------------------------------------------

    if (save_to_csv) {

      csv_path <- file.path(
        output_dir,
        paste0(
          device_id,
          "_",
          variable_id,
          "_data.csv"
        )
      )

      utils::write.csv(
        variable_data,
        csv_path,
        row.names = FALSE,
        na = ""
      )

      message(
        "Saved ",
        nrow(variable_data),
        " record(s) for variable '",
        variable_id,
        "' -> ",
        csv_path
      )
    }
  }

  results
}
