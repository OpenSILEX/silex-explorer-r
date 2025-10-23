#' Retrieve Scientific Objects by Experiment Label
#'
#' This function fetches scientific objects from an OpenSILEX session by experiment label,
#' allowing filtering by object type, factor levels, and germplasm information.
#'
#' @param session An `opensilex_connection` object.
#' @param experiment_label Character string. Label of the experiment to retrieve scientific objects from.
#' @param obj_type Optional character string specifying the type of scientific object to filter (exact match).
#' @param factor_levels Optional character vector of factor-level filters. Each element should follow the format `"FactorName.Level"`.
#' Multiple factors can be provided.
#' @param germplasm_type Optional character string specifying the germplasm type to filter (case-insensitive).
#' @param germplasm_name Optional character string specifying the germplasm name to filter (case-insensitive).
#' Requires `germplasm_type` to be specified.
#' @param output_dir Optional character string specifying a directory path where the output CSV file will be saved.
#' If `NULL`, results are only displayed.
#' @param verbose Logical, whether to print progress messages.
#' @param check_consistency Logical, whether to check URI-label consistency.
#' @return A data frame containing scientific objects matching the specified filters.
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'session' is already created using the login() function
#'
#' # Retrieve scientific objects for an experiment
#' objects <- lsOsByExp(session, experiment_label = "ZA17")
#' print(objects)
#'
#' # Filter by object type
#' objects_type <- lsOsByExp(session, experiment_label = "ZA17", obj_type = "Plant")
#' print(objects_type)
#'
#' # Filter by a factor level
#' objects_factor <- lsOsByExp(session, experiment_label = "ZA17", factor_levels = c("Irrigation.WD"))
#' print(objects_factor)
#'
#' # Filter by germplasm type and name
#' objects_germplasm <- lsOsByExp(session, experiment_label = "ZA17",
#'                                germplasm_type = "Variety", germplasm_name = "testVariety")
#' print(objects_germplasm)
#' }
lsOsByExp <- function(session, experiment_label,
                      obj_type = NULL,
                      factor_levels = NULL,
                      germplasm_type = NULL,
                      germplasm_name = NULL,
                      output_dir = NULL,
                      verbose = TRUE,
                      check_consistency = TRUE) {

  if (!inherits(session, "opensilex_connection")) {
    stop("'session' must be an opensilex_connection object")
  }

  if (is.null(experiment_label) || !is.character(experiment_label)) {
    stop("'experiment_label' must be a non-null character string.")
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Step 1: Get experiment start date
  if (verbose) message("Fetching start date for experiment label: ", experiment_label)

  label_query <- sprintf('
    query {
      Experiment(filter: {label: "%s"}, inferred: true) {
        startDate
      }
    }', experiment_label)

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
    stop("No experiment found with label: ", experiment_label)
  }

  exp_data <- label_result$data$Experiment[[1]]
  start_date <- gsub("-", "_", substr(exp_data$startDate, 1, 10))
  clean_label <- gsub("[^A-Za-z0-9]", "_", experiment_label)
  experiment_uri <- paste0("EXP_", clean_label, "_", start_date)

  if (verbose) message("Constructed experiment URI: ", experiment_uri)

  # Step 2: Query scientific objects
  query <- sprintf('
  query {
    ScientificObject(Experience: %s, inferred: true) {
      _id
      label
      _type
      hasFactorLevel {
        label
        hasFactor { label }
      }
      hasGermplasm {
        _type(inferred: true)
        label
        fromSpecies { label }
        fromVariety { label }
        fromAccession { label }
      }
    }
  }', experiment_uri)

  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      "Content-Type" = "application/json"
    ),
    body = list(query = query),
    encode = "json",
    httr::timeout(120)
  )

  httr::stop_for_status(response)
  result <- httr::content(response, as = "parsed")

  if (is.null(result$data$ScientificObject) || length(result$data$ScientificObject) == 0) {
    warning("No scientific objects found for the specified experiment.")
    return(data.frame())
  }

  objects <- purrr::map(result$data$ScientificObject, function(obj) {
    base <- list(
      uri = obj[["_id"]],
      label = obj$label,
      type = if (!is.null(obj[["_type"]])) paste(obj[["_type"]], collapse = "; ") else ""
    )

    # Factor levels
    factors <- list()
    if (!is.null(obj$hasFactorLevel)) {
      for (fl in obj$hasFactorLevel) {
        if (!is.null(fl$hasFactor[[1]]$label)) {
          factor_name <- fl$hasFactor[[1]]$label
          factors[[factor_name]] <- fl$label
        }
      }
    }

    # Germplasm
    germplasm <- list()
    if (!is.null(obj$hasGermplasm)) {
      for (i in seq_along(obj$hasGermplasm)) {
        germ <- obj$hasGermplasm[[i]]
        germplasm[[paste0("germplasm_label_", i)]] <- germ$label
        germ_type <- if (!is.null(germ[["_type"]][[1]])) sub(".*[#/]", "", germ[["_type"]][[1]]) else NA
        germplasm[[paste0("germplasm_type_", i)]] <- germ_type

        germplasm[[paste0("species_", i)]] <-
          if (!is.null(germ$fromSpecies) && length(germ$fromSpecies) > 0 && !is.null(germ$fromSpecies[[1]]$label)) {
            germ$fromSpecies[[1]]$label
          } else {
            ""
          }

        germplasm[[paste0("variety_", i)]] <-
          if (!is.null(germ$fromVariety) && length(germ$fromVariety) > 0 && !is.null(germ$fromVariety[[1]]$label)) {
            germ$fromVariety[[1]]$label
          } else {
            ""
          }

        germplasm[[paste0("accession_", i)]] <-
          if (!is.null(germ$fromAccession) && length(germ$fromAccession) > 0 && !is.null(germ$fromAccession[[1]]$label)) {
            germ$fromAccession[[1]]$label
          } else {
            ""
          }
      }
    }

    c(base, factors, germplasm)
  })

  # Harmonize data
  all_cols <- unique(unlist(lapply(objects, names)))
  df <- do.call(rbind.data.frame,
                lapply(objects, function(row) {
                  row[setdiff(all_cols, names(row))] <- NA
                  as.data.frame(as.list(row[all_cols]), stringsAsFactors = FALSE)
                }))
  df[is.na(df)] <- ""

  # Filtering by object type
  if (!is.null(obj_type)) {
    df <- df[df$type == obj_type, ]
  }


  # Filtering by factor levels (improved for factor names with dots)
  if (!is.null(factor_levels)) {
    # Start with a logical vector of TRUE (i.e., no filtering applied yet)
    filter_condition <- rep(TRUE, nrow(df))

    for (fl in factor_levels) {
      parts <- unlist(strsplit(fl, "\\."))
      if (length(parts) >= 2) {
        level <- tail(parts, 1)
        factor_name <- paste(parts[-length(parts)], collapse = ".")

        # Check if the factor exists in the data
        if (factor_name %in% names(df)) {
          # Check if the level exists in the factor values
          if (level %in% unique(df[[factor_name]])) {
            # Update filter_condition to include only rows that match the current condition
            filter_condition <- filter_condition & (df[[factor_name]] == level)
          } else {
            warning("Level '", level, "' for factor '", factor_name, "' does not exist in the data.")
            filter_condition <- rep(FALSE, nrow(df))  # Return an empty dataframe
            break  # Stop further filtering since there's no matching level
          }
        } else {
          warning("Factor '", factor_name, "' does not exist in the data.")
          filter_condition <- rep(FALSE, nrow(df))  # Return an empty dataframe
          break  # Stop further filtering since there's no matching factor
        }
      }
    }

    # Apply the final filter condition
    df <- df[filter_condition, ]
  }


  # Advanced filtering by germplasm type and optionally germplasm name
  if (!is.null(germplasm_type)) {

    # Get germplasm_type and germplasm_label columns (case-insensitive)
    type_cols <- grep("^germplasm_type", names(df), value = TRUE, ignore.case = TRUE)
    label_cols <- grep("^germplasm_label", names(df), value = TRUE, ignore.case = TRUE)

    if (!is.null(germplasm_name)) {
      # Initialize an empty logical filter
      match_filter <- rep(FALSE, nrow(df))

      # --- CASE 1: Match via germplasm_type_X / germplasm_label_X pairs ---
      for (type_col in type_cols) {
        suffix <- sub("^germplasm_type", "", type_col, ignore.case = TRUE)
        label_col <- paste0("germplasm_label", suffix)

        if (label_col %in% names(df)) {
          type_match <- tolower(df[[type_col]]) == tolower(germplasm_type)
          label_match <- tolower(df[[label_col]]) == tolower(germplasm_name)
          match_filter <- match_filter | (type_match & label_match)
        }
      }

      # --- CASE 2: Match via columns starting with the germplasm_type itself ---
      type_specific_cols <- grep(paste0("^", germplasm_type), names(df),
                                 value = TRUE, ignore.case = TRUE)

      if (length(type_specific_cols) > 0) {
        type_specific_match <- apply(df[type_specific_cols], 1, function(row) {
          any(tolower(row) == tolower(germplasm_name))
        })
        match_filter <- match_filter | type_specific_match
      }

      # Apply combined filter
      df <- df[match_filter, , drop = FALSE]

    } else {
      # --- CASE: Filter only by germplasm_type ---
      if (length(type_cols) > 0) {
        type_filter <- apply(df[type_cols], 1, function(row) {
          any(tolower(row) == tolower(germplasm_type))
        })
        df <- df[type_filter, , drop = FALSE]
      } else {
        warning("No germplasm_type columns found in the data.")
        df <- df[FALSE, , drop = FALSE]
      }
    }

  } else if (!is.null(germplasm_name)) {
    # --- CASE: germplasm_name provided without germplasm_type ---
    warning("You have provided a germplasm_name, but you must also specify a germplasm_type for filtering.")
    df <- df[FALSE, , drop = FALSE]
  }



  # URI-Name consistency check (if applicable)
  if (exists("uri_name")) {
    purrr::walk2(df$uri, df$label, insertUri_Name)
    if (check_consistency) UriCons()
  }

  # Optional export
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_out <- file.path(output_dir, paste0("scientific_objects_", timestamp, ".csv"))
    write.csv(df, file_out, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) message("Data saved to ", file_out)
  } else if (verbose) {
    message("Displaying results without saving to file.")
  }

  return(df)
}
