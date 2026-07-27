#' Retrieve devices associated with a facility
#'
#' This function retrieves all devices associated with a given facility using
#' pagination. Results can optionally be exported to a CSV file.
#'
#' @param session An opensilex_connection object.
#' @param facility_label Character, label of the facility.
#' @param page_size Integer, number of devices retrieved per page.
#' @param output_dir Character or NULL, directory where the CSV file will be saved.
#'
#' @return A tibble containing:
#' \itemize{
#'   \item URI: device URI
#'   \item type: device RDF type
#'   \item Name: device name
#' }
#'
#' @examples
#' \dontrun{
#' # Retrieve devices associated with a facility
#' devices <- lsDevicesByFacility(
#'   session = session,
#'   facility_label = "greenhouse 1"
#' )
#'
#' print(devices)
#'
#' # Retrieve devices and export the result
#' devices <- lsDevicesByFacility(
#'   session = session,
#'   facility_label = "greenhouse 1",
#'   page_size = 20,
#'   output_dir = "outputs"
#' )
#' }
#'
#' @export
lsDevicesByFacility <- function(session,
                                facility_label,
                                page_size = 20,
                                output_dir = NULL) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  #----------------------------------------------------------
  # Retrieve facility URI
  #----------------------------------------------------------

  facility_uri <- getUrisFromName(facility_label)

  if (length(facility_uri) == 0) {
    stop("No URI found for facility: ", facility_label)
  }

  if (length(facility_uri) > 1) {
    warning("Multiple URIs found, the first one will be used by default")
    facility_uri <- facility_uri[1]
  }

  #----------------------------------------------------------
  # Prepare output directory
  #----------------------------------------------------------

  save_to_csv <- !is.null(output_dir)

  if (save_to_csv && !dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE
    )
  }

  #----------------------------------------------------------
  # REST endpoint
  #----------------------------------------------------------

  url <- paste0(
    sub("/+$", "", session$instance),
    "/rest/core/devices"
  )

  current_page <- 0
  has_next_page <- TRUE
  devices <- list()

  #----------------------------------------------------------
  # Pagination
  #----------------------------------------------------------

  while (has_next_page) {

    response <- httr::GET(
      url = url,
      query = list(
        facility = facility_uri,
        page = current_page,
        pageSize = page_size
      ),
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

    page_devices <- result$result

    if (!is.null(page_devices) && length(page_devices) > 0) {
      devices <- c(devices, page_devices)
    }

    has_next_page <- isTRUE(
      result$metadata$pagination$hasNextPage
    )

    message(
      "Page ",
      current_page,
      " retrieved: ",
      length(page_devices),
      " device(s)"
    )

    current_page <- current_page + 1
  }

  #----------------------------------------------------------
  # No devices found
  #----------------------------------------------------------

  if (length(devices) == 0) {
    warning("No devices found for the given facility.")

    return(
      tibble::tibble(
        URI = character(),
        type = character(),
        Name = character()
      )
    )
  }

  #----------------------------------------------------------
  # Convert results to tibble
  #----------------------------------------------------------

  devices_df <- purrr::map_dfr(devices, function(device) {
    tibble::tibble(
      URI = device$uri,
      type = device$rdf_type_name,
      Name = device$name
    )
  })

  #----------------------------------------------------------
  # Export CSV
  #----------------------------------------------------------

  if (save_to_csv) {

    facility_name <- gsub(
      "[^A-Za-z0-9_-]",
      "_",
      facility_label
    )

    csv_path <- file.path(
      output_dir,
      paste0(facility_name, "_devices.csv")
    )

    utils::write.csv(
      devices_df,
      csv_path,
      row.names = FALSE
    )

    message("Devices saved to: ", csv_path)
  }

  #----------------------------------------------------------
  # Insert devices into URI-name table
  #----------------------------------------------------------

  if (nrow(devices_df) > 0) {

    uri_name_pairs <- devices_df |>
      dplyr::transmute(
        uri = URI,
        name = Name
      )

    insertUri_Name(uri_name_pairs)
  }

  return(devices_df)
}
