#' Retrieve Scientific Objects by Experiment Label
#'
#' This function fetches scientific objects from an OpenSILEX session by experiment label,
#' with filtering options.
#'
#' @param session An opensilex_connection object.
#' @param experiment_label Character string, experiment label.
#' @param obj_type Optional character to filter object types.
#' @param factor_levels Optional vector of factor levels to filter.
#' @param germplasm_type Optional germplasm type filter.
#' @param germplasm_name Optional germplasm name filter.
#' @param output_dir Optional directory to save output CSV.
#' @param verbose Logical, whether to print progress messages.
#' @param check_consistency Logical, whether to check URI-label consistency.
#' @return Data frame of scientific objects matching criteria.
#' @export
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
    df <- df[grepl(obj_type, df$type, ignore.case = TRUE), ]
  }

  # Filtering by factor levels (improved for factor names with dots)
  if (!is.null(factor_levels)) {
    for (fl in factor_levels) {
      parts <- unlist(strsplit(fl, "\\."))
      if (length(parts) >= 2) {
        level <- tail(parts, 1)
        factor_name <- paste(parts[-length(parts)], collapse = ".")
        if (factor_name %in% names(df)) {
          df <- df[df[[factor_name]] == level, ]
        } else {
          warning("Factor '", factor_name, "' not found in data.")
        }
      }
    }
  }
  

  # Advanced filtering by germplasm type and optionally germplasm name
  if (!is.null(germplasm_type)) {
    if (!is.null(germplasm_name)) {
      # Step 1: Filter by germplasm type and germplasm name in any relevant columns
      name_cols <- grep("^(germplasm_label_|Germplasm_)", names(df), value = TRUE)
      type_cols <- grep(paste0("^", germplasm_type), names(df), value = TRUE)
      relevant_cols <- unique(c(name_cols, type_cols))

      if (length(relevant_cols) > 0) {
        name_filter <- apply(df[relevant_cols], 1, function(row) germplasm_name %in% row)
        df <- df[name_filter, ]
      }
    } else {
      # Step 2: Filter only by germplasm type in all germplasm_type columns
      type_cols <- grep("^germplasm_type_", names(df), value = TRUE)
      if (length(type_cols) > 0) {
        type_filter <- apply(df[type_cols], 1, function(row) germplasm_type %in% row)
        df <- df[type_filter, ]
      }
    }
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