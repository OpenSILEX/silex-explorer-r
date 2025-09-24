library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
login <- function(id, password, instance, urlGraphql) {
  # Remove trailing slash from instance URL if present
  instance <- gsub("/$", "", instance)

  # Construct REST URL
  urlRest <- paste0(instance, "/rest")

  # Prepare request body
  body <- list(
    identifier = id,
    password = password
  )

  # Make authentication request
  response <- httr::POST(paste0(urlRest, "/security/authenticate"),
                         body = body, encode = "json")

  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    token <- httr::content(response)$result$token
    # Return a list with all required outputs
    return(list(
      token = token,
      instance = instance,
      urlRest = urlRest,
      urlGraphql = urlGraphql
    ))
  } else {
    stop("Authentication failed. Please check your credentials and instance details.")
  }
}
lsExp <- function(session, species = NULL, projet = NULL, date = NULL) {
  # Define the GraphQL query
  query <- 'query MyQuery {
    Experiment(inferred: true) {
      _id
      label
      startDate
      endDate
      hasSpecies {
        label
      }
      hasProject {
        label
      }
      usesFacility {
        label
      }
    }
  }'

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$Experiment) && length(result$data$Experiment) > 0) {
      # Process the result into a data frame
      experiments <- lapply(result$data$Experiment, function(exp) {
        data.frame(
          id = ifelse(!is.null(exp$`_id`), exp$`_id`, NA),
          label = ifelse(!is.null(exp$label), exp$label, NA),
          start_date = ifelse(!is.null(exp$startDate), exp$startDate, NA),
          end_date = ifelse(!is.null(exp$endDate), exp$endDate, NA),
          species = ifelse(!is.null(exp$hasSpecies) && length(exp$hasSpecies) > 0, exp$hasSpecies[[1]]$label, NA),
          project = ifelse(!is.null(exp$hasProject) && length(exp$hasProject) > 0, exp$hasProject[[1]]$label, NA),
          facilities = ifelse(!is.null(exp$usesFacility) && length(exp$usesFacility) > 0,
                              paste(sapply(exp$usesFacility, function(f) f$label), collapse = ", "),
                              NA),
          stringsAsFactors = FALSE
        )
      })

      # Combine all experiment data into a single data frame
      experiments_df <- do.call(rbind, experiments)

      # Apply filters
      if (!is.null(species)) {
        experiments_df <- experiments_df[experiments_df$species == species, ]
      }

      if (!is.null(projet)) {
        experiments_df <- experiments_df[experiments_df$project == projet, ]
      }

      if (!is.null(date)) {
        experiments_df <- experiments_df[as.Date(experiments_df$start_date) <= as.Date(date) &
                                           (is.na(experiments_df$end_date) | as.Date(experiments_df$end_date) >= as.Date(date)), ]
      }

      # Save the result as a CSV file
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      output_file <- file.path(output_dir, "experiments.csv")

      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      write.csv(experiments_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(experiments_df)
    } else {
      warning("No experiments found.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}
##############################################################"
######################################################"
lsFactorsByExp <- function(session, experiment_uri) {
  # Define the GraphQL query with the experiment URI as a filter
  query <- sprintf('query MyQuery {
    Experiment(inferred: true, filter: {_id: "%s"}) {
      studyEffectOf {
        _id
        label
      }
    }
  }', experiment_uri)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$Experiment) && length(result$data$Experiment) > 0 &&
        !is.null(result$data$Experiment[[1]]$studyEffectOf)) {
      # Process the result into a data frame
      factors <- lapply(result$data$Experiment[[1]]$studyEffectOf, function(factor) {
        data.frame(
          id = ifelse(!is.null(factor$`_id`), factor$`_id`, NA),
          label = ifelse(!is.null(factor$label), factor$label, NA),
          stringsAsFactors = FALSE
        )
      })

      factors_df <- do.call(rbind, factors)

      # Define output directory and file path
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      output_file <- file.path(output_dir, "factors.csv")

      # Save the result as a CSV file
      write.csv(factors_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(factors_df)
    } else {
      warning("No factors found for the specified experiment.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}
###############################"

lsFlByExp <- function(session, experiment_uri) {
  # Step 1: Retrieve the factors for the specified experiment using the `lsFactorsByExp` function
  factors_df <- lsFactorsByExp(session, experiment_uri)

  if (nrow(factors_df) == 0) {
    warning("No factors found for the specified experiment.")
    return(data.frame())
  }

  # Initialize an empty list to store factor levels
  factor_levels <- list()

  # Step 2: Iterate over each factor to fetch associated levels
  for (i in seq_len(nrow(factors_df))) {
    factor_id <- factors_df$id[i]
    factor_label <- factors_df$label[i]

    # Define the GraphQL query to fetch levels for a specific factor
    query <- sprintf('query MyQuery {
      Factor(filter: {_id: "%s"}) {
        hasLevels {
          _id
          label
        }
      }
    }', factor_id)

    # Make the POST request to the GraphQL API
    response <- httr::POST(
      url = session$urlGraphql,
      httr::add_headers(Authorization = paste("Bearer", session$token)),
      body = list(query = query),
      encode = "json"
    )

    # Check if the request was successful
    if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
      result <- httr::content(response, as = "parsed")

      if (!is.null(result$data$Factor) && length(result$data$Factor) > 0 &&
          !is.null(result$data$Factor[[1]]$hasLevels)) {
        levels <- lapply(result$data$Factor[[1]]$hasLevels, function(level) {
          data.frame(
            `Factor Label` = factor_label,
            `Factor URI` = factor_id,
            `Level Label` = ifelse(!is.null(level$label), level$label, NA),
            `Level ID` = ifelse(!is.null(level$`_id`), level$`_id`, NA),
            stringsAsFactors = FALSE
          )
        })

        # Append levels to the main list
        factor_levels <- c(factor_levels, levels)
      }
    } else {
      warning("Failed to fetch levels for factor: ", factor_label)
    }
  }

  # Combine all factor level data frames into one
  factor_levels_df <- do.call(rbind, factor_levels)

  # Define the output directory and file path
  output_dir <- "/home/rebahi/Bureau/final/csv file"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, "factor_levels.csv")

  # Save the data as a CSV file
  write.csv(factor_levels_df, file = output_file, row.names = FALSE)

  message("Data saved to ", output_file)

  return(factor_levels_df)
}
###########################################################"

lsOsByExp <- function(session, experiment_uri, obj_type = NULL,
                      factor_level_uri = NULL, germplasm_uri = NULL) {
  # Define the GraphQL query with the experiment_uri inserted without quotes
  query <- sprintf('
  query {
    ScientificObject(Experience: %s, inferred: true) {
      _id
      label
      type
      hasFactorLevel {
        label
        hasFactor {
          label
        }
      }
      hasGermplasm {
        fromSpecies {
          _id
          label
        }
        fromVariety {
          _id
          label
        }
        fromAccession {
          _id
          label
        }
        label
        type
      }
    }
  }', experiment_uri)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$ScientificObject) && length(result$data$ScientificObject) > 0) {
      # Process the result into a data frame
      objects <- lapply(result$data$ScientificObject, function(obj) {
        # Extract species information
        species <- if (!is.null(obj$hasGermplasm$fromSpecies)) {
          sapply(obj$hasGermplasm$fromSpecies, function(sp) sp$label)
        } else {
          NA
        }
        # Extract variety information
        varieties <- if (!is.null(obj$hasGermplasm$fromVariety)) {
          sapply(obj$hasGermplasm$fromVariety, function(var) var$label)
        } else {
          NA
        }
        # Extract accession information
        accessions <- if (!is.null(obj$hasGermplasm$fromAccession)) {
          sapply(obj$hasGermplasm$fromAccession, function(acc) acc$label)
        } else {
          NA
        }
        # Create a data frame for the object
        data.frame(
          id = ifelse(!is.null(obj$`_id`), obj$`_id`, NA),
          label = ifelse(!is.null(obj$label), obj$label, NA),
          type = ifelse(!is.null(obj$type), obj$type, NA),
          factor_level = ifelse(!is.null(obj$hasFactorLevel) && length(obj$hasFactorLevel) > 0,
                                paste(sapply(obj$hasFactorLevel, function(fl) fl$label), collapse = ", "),
                                NA),
          factor = ifelse(!is.null(obj$hasFactorLevel) && length(obj$hasFactorLevel) > 0,
                          paste(sapply(obj$hasFactorLevel, function(fl) fl$hasFactor$label), collapse = ", "),
                          NA),
          species = ifelse(length(species) > 0, paste(species, collapse = ", "), NA),
          variety = ifelse(length(varieties) > 0, paste(varieties, collapse = ", "), NA),
          accession = ifelse(length(accessions) > 0, paste(accessions, collapse = ", "), NA),
          stringsAsFactors = FALSE
        )
      })

      # Combine all object data into a single data frame
      objects_df <- do.call(rbind, objects)

      # Apply filters
      if (!is.null(obj_type)) {
        objects_df <- objects_df[objects_df$type == obj_type, ]
      }

      if (!is.null(factor_level_uri)) {
        objects_df <- objects_df[grepl(factor_level_uri, objects_df$factor_level), ]
      }

      if (!is.null(germplasm_uri)) {
        objects_df <- objects_df[grepl(germplasm_uri, objects_df$species) |
                                   grepl(germplasm_uri, objects_df$variety) |
                                   grepl(germplasm_uri, objects_df$accession), ]
      }

      # Save the result as a CSV file
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      output_file <- file.path(output_dir, "scientific_objects.csv")

      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      write.csv(objects_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(objects_df)
    } else {
      warning("No scientific objects found.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}

##############################################################################""#
lsOsTypeByExp <- function(session, experiment_uri) {
  # Define the GraphQL query to fetch scientific objects by experiment
  query <- sprintf('
  query {
    ScientificObject(Experience: %s, inferred: true) {
      _id
      label
      type
    }
  }', experiment_uri)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$ScientificObject) && length(result$data$ScientificObject) > 0) {
      # Extract types of scientific objects
      objects <- lapply(result$data$ScientificObject, function(obj) {
        data.frame(
          id = ifelse(!is.null(obj$`_id`), obj$`_id`, NA),
          label = ifelse(!is.null(obj$label), obj$label, NA),
          type = ifelse(!is.null(obj$type), obj$type, NA),
          stringsAsFactors = FALSE
        )
      })

      # Combine all object data into a single data frame
      objects_df <- do.call(rbind, objects)

      # Save the result as a CSV file
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      output_file <- file.path(output_dir, "scientific_objects_types.csv")

      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      write.csv(objects_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(objects_df)
    } else {
      warning("No scientific objects found for the given experiment.")
      return(data.frame(
        id = character(),
        label = character(),
        type = character(),
        stringsAsFactors = FALSE
      ))
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}

#####################################################################################
lsVarByExp <- function(session, experiment_uri, output_dir = "/home/rebahi/Bureau/final/csv file", verbose = TRUE) {
  # Define the API endpoint
  endpoint <- "/core/variables"

  # Construct the full URL
  url <- paste0(session$urlRest, endpoint)

  # Set up query parameters
  query_params <- list(
    experiments = experiment_uri,
    withAssociatedData = "false",  # Ensure that associated data is not fetched
    order_by = "uri=asc",
    page = 0,
    page_size = 20
  )

  # Make the GET request to the REST API
  response <- httr::GET(
    url = url,
    httr::add_headers(
      Authorization = paste("Bearer", session$token),
      Accept = "application/json"
    ),
    query = query_params
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed", type = "application/json")

    # Debugging: Print the API response structure
    if (verbose) print(result$result)

    # Ensure there is data in the result
    if (length(result$result) > 0) {
      # Extract the variable names and entities
      variables <- lapply(result$result, function(var) {
        data.frame(
          name = ifelse(!is.null(var$name), var$name, NA),
          entity = ifelse(!is.null(var$hasEntity) && !is.null(var$hasEntity$label), var$hasEntity$label, NA),
          stringsAsFactors = FALSE
        )
      })

      # Combine the data frames into one
      variables_df <- do.call(rbind, variables)

      # Ensure output directory exists
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
      }

      # Save the result as a CSV file
      output_file <- file.path(output_dir, "variables_with_entities.csv")
      write.csv(variables_df, file = output_file, row.names = FALSE)

      if (verbose) message("Data saved to ", output_file)

      return(variables_df)
    } else {
      warning("No variables found for the specified experiment.")
      return(data.frame())
    }
  } else {
    stop("REST API request failed with status code ", httr::status_code(response))
  }
}



####################################################################################"
####################"

lsFlByFactors <- function(session, factor_id, output_dir = "/home/rebahi/Bureau/final/csv file") {
  # Define the GraphQL query to fetch levels for a specific factor
  query <- sprintf('query MyQuery {
    FactorLevel(inferred: true, filter: {hasFactor: "%s"}) {
      _id
      label
    }
  }', factor_id)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    # Check if the result contains valid data
    if (!is.null(result$data$FactorLevel) && length(result$data$FactorLevel) > 0) {
      # Process the levels into a data frame
      factor_levels_df <- do.call(rbind, lapply(result$data$FactorLevel, function(level) {
        data.frame(
          `Factor URI` = factor_id,
          `Level Label` = ifelse(!is.null(level$label), level$label, NA),
          `Level ID` = ifelse(!is.null(level$`_id`), level$`_id`, NA),
          stringsAsFactors = FALSE
        )
      }))

      # Ensure the output directory exists
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      # Define the output file path
      output_file <- file.path(output_dir, "factor_levels_by_factor.csv")

      # Save the data as a CSV file
      write.csv(factor_levels_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(factor_levels_df)
    } else {
      warning("No factor levels found for the specified factor ID.")
      return(data.frame())
    }
  } else {
    stop("Failed to fetch factor levels. Status code: ", httr::status_code(response))
  }
}

#' List Experiments by Facility and Save to CSV
#'
#' @param session List containing authentication and URL information
#' @param output_file Path to the CSV file to save the output
#'
#' @return A data frame where each row contains a facility and its corresponding experiment label
#' @export
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv
lsExperimentsByFacility <- function(session, output_file = "/home/rebahi/Bureau/final/csv file") {
  query <- '
  query MyQuery {
    Experiment(inferred: true) {
      usesFacility {
        label
      }
      label
    }
  }'

  url <- session$urlGraphql
  response <- httr::POST(url,
                         httr::add_headers(Authorization = paste("Bearer", session$token)),
                         body = list(query = query),
                         encode = "json")

  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    content <- httr::content(response, "text")
    json <- jsonlite::fromJSON(content, flatten = TRUE)

    if (!is.null(json$data$Experiment) && length(json$data$Experiment) > 0) {
      experiments <- json$data$Experiment

      # Create a data frame to store experiments by facility
      experiments_by_facility <- data.frame(Facility = character(0), Experiment = character(0), stringsAsFactors = FALSE)

      # Group experiments by facility
      for (i in seq_len(nrow(experiments))) {
        facility <- experiments$usesFacility[[i]]$label
        experiment <- experiments$label[i]

        if (is.null(facility) || length(facility) == 0) {
          facility <- "No Facility"
        } else {
          facility <- facility[1]  # Take the first facility if there are multiple
        }

        # Add to the data frame
        experiments_by_facility <- rbind(experiments_by_facility, data.frame(Facility = facility, Experiment = experiment, stringsAsFactors = FALSE))
      }

      # Write the data frame to a CSV file
      utils::write.csv(experiments_by_facility, file = output_file, row.names = FALSE)

      return(experiments_by_facility)
    } else {
      stop("No experiments found to save to CSV.")
    }
  } else {
    stop("GraphQL request for experiments by facility failed. Please try again later.")
  }
}
#' List Facilities and Save as CSV
#'
#' @param session List containing authentication and URL information
#' @param output_dir Directory where the CSV file will be saved
#'
#' @return A data frame of facilities with columns: id, label
#' @export
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% select rename
#' @importFrom utils write.csv
lsFacility <- function(session, output_dir = "/home/rebahi/Bureau/final/csv file") {
  query <- '
  query MyQuery {
    Facility(inferred: true) {
      _id
      label
      type
    }
  }'

  url <- session$urlGraphql
  response <- httr::POST(url,
                         httr::add_headers(Authorization = paste("Bearer", session$token)),
                         body = list(query = query),
                         encode = "json")

  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    content <- httr::content(response, "text")
    json <- jsonlite::fromJSON(content, flatten = TRUE)

    if (!is.null(json$data$Facility) && length(json$data$Facility) > 0) {
      facilities <- json$data$Facility %>%
        dplyr::select(.data$`_id`, .data$label) %>%
        dplyr::rename(id = .data$`_id`)

      # Save the data frame to a CSV file
      if (nrow(facilities) > 0) {
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        output_file <- file.path(output_dir, "facilities.csv")
        write.csv(facilities, file = output_file, row.names = FALSE)
        message("Facility data saved to ", output_file)

        # Optional: Print the table using knitr if knitr is available
        if (requireNamespace("knitr", quietly = TRUE)) {
          print(knitr::kable(facilities, format = "pipe", align = "c"))
        }

        return(facilities)
      } else {
        return(data.frame(id = character(), label = character()))  # Return an empty data frame if no valid facilities are found
      }
    } else {
      return(data.frame(id = character(), label = character()))  # Return an empty data frame if no facilities are found
    }
  } else {
    stop("GraphQL request for facility data failed. Please try again later.")
  }
}

lsVarByFacility <- function(session, facility_uri) {
  # Define the GraphQL query to fetch all variables and their targets
  query <- 'query MyQuery {
    Data_findMany {
      variable
      target
    }
  }'

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(
      Authorization = paste("Bearer", session$token) # Add the authorization header
    ),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$Data_findMany) && length(result$data$Data_findMany) > 0) {
      # Filter the data to only include variables for the specified facility_uri
      filtered_data <- lapply(result$data$Data_findMany, function(data_item) {
        if (data_item$target == facility_uri) {
          return(data.frame(
            variable = ifelse(!is.null(data_item$variable), data_item$variable, NA),
            target = data_item$target,
            stringsAsFactors = FALSE
          ))
        }
        NULL # Return NULL if the target does not match facility_uri
      })

      # Remove NULL entries from the list
      filtered_data <- filtered_data[!sapply(filtered_data, is.null)]

      # Combine the filtered data into a data frame
      variables_df <- do.call(rbind, filtered_data)

      # Define output directory and file path
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      output_file <- file.path(output_dir, "variables_by_facility.csv")

      # Save the result as a CSV file
      write.csv(variables_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(variables_df)
    } else {
      warning("No variables found for the specified facility.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}

#' List Data by Scientific Object for an Experiment and Save as CSV
#'
#' @param session List containing authentication and URL information
#' @param experiment_uri URI of the experiment
#' @param os_label Label of the scientific object to fetch data for
#' @param output_dir Directory where the CSV file will be saved
#'
#' @return A data frame containing the scientific object data
#' @export
#'
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% mutate select arrange rename
#' @importFrom tidyr unnest
#' @importFrom utils write.csv
lsDataByOS <- function(session, experiment_uri, os_label, output_dir = "/home/rebahi/Bureau/final/csv file") {

  # Define the GraphQL query
  query <- sprintf('
  query {
    ScientificObject(Experience: %s, inferred: true, filter: {label: "%s"}) {
      data {
        variable
        value
        date
      }
    }
  }', experiment_uri, os_label)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$ScientificObject) && length(result$data$ScientificObject) > 0) {
      # Process the result into a data frame
      data <- lapply(result$data$ScientificObject, function(obj) {
        if (!is.null(obj$data) && length(obj$data) > 0) {
          do.call(rbind, lapply(obj$data, function(record) {
            data.frame(
              variable = record$variable,
              value = as.numeric(record$value),
              date = as.POSIXct(record$date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
              stringsAsFactors = FALSE
            )
          }))
        } else {
          NULL
        }
      })

      # Combine all data into a single data frame
      data_df <- do.call(rbind, data)

      # Save the result as a CSV file
      if (!is.null(data_df) && nrow(data_df) > 0) {
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        output_file <- file.path(output_dir, "data_by_scientific_object.csv")
        write.csv(data_df, file = output_file, row.names = FALSE)

        message("Data saved to ", output_file)
        return(data_df)
      } else {
        warning("No data found for the specified scientific object.")
        return(data.frame())
      }
    } else {
      warning("No scientific objects found.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}
lsDataByVar <- function(session, variable_name, output_dir = "/home/rebahi/Bureau/final/csv file") {
  # Define the GraphQL query
  query <- sprintf('
  query {
    Data_findMany {
      target
      value
      uri
      variable
    }
  }')

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(content, flatten = TRUE)
    data <- json$data$Data_findMany

    if (!is.null(data) && length(data) > 0) {
      # Process the result into a data frame
      result <- data %>%
        dplyr::filter(variable == variable_name) %>%
        dplyr::rename(
          target_uri = target,
          variable_uri = variable
        ) %>%
        dplyr::select(uri, variable_uri, target_uri, value)

      # Save the result as a CSV file
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      output_file <- file.path(output_dir, "data_by_variable.csv")
      write.csv(result, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)
      return(result)
    } else {
      warning("No data found for the specified variable.")
      return(data.frame(uri = character(), variable_uri = character(), target_uri = character(), value = numeric()))
    }
  } else {
    stop("GraphQL request for variable data failed with status code: ", httr::status_code(response))
  }
}

lsDeviceByFacility <- function(session, facility_uri) {
  # Define the base URL for the REST service
  base_url <- paste0(session$urlRest, "/core/devices")

  # Set query parameters
  query_params <- list(
    facility = facility_uri, # Filter by facility
    page = 0,                # Start with the first page
    page_size = 100          # Adjust page size as needed
  )

  # Make the GET request to the REST service
  response <- httr::GET(
    url = base_url,
    httr::add_headers(
      Authorization = paste("Bearer", session$token), # Add the authorization header
      `Accept-Language` = "en"                       # Specify the language
    ),
    query = query_params
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result) && length(result) > 0) {
      # Process the result into a data frame
      devices <- lapply(result, function(device) {
        data.frame(
          uri = ifelse(!is.null(device$uri), device$uri, NA),
          rdf_type = ifelse(!is.null(device$rdf_type), device$rdf_type, NA),
          rdf_type_name = ifelse(!is.null(device$rdf_type_name), device$rdf_type_name, NA),
          name = ifelse(!is.null(device$name), device$name, NA),
          brand = ifelse(!is.null(device$brand), device$brand, NA),
          constructor_model = ifelse(!is.null(device$constructor_model), device$constructor_model, NA),
          serial_number = ifelse(!is.null(device$serial_number), device$serial_number, NA),
          person_in_charge = ifelse(!is.null(device$person_in_charge), device$person_in_charge, NA),
          start_up = ifelse(!is.null(device$start_up), device$start_up, NA),
          removal = ifelse(!is.null(device$removal), device$removal, NA),
          description = ifelse(!is.null(device$description), device$description, NA),
          publication_date = ifelse(!is.null(device$publication_date), device$publication_date, NA),
          last_updated_date = ifelse(!is.null(device$last_updated_date), device$last_updated_date, NA),
          stringsAsFactors = FALSE
        )
      })

      devices_df <- do.call(rbind, devices)

      # Define output directory and file path
      output_dir <- "/home/rebahi/Bureau/final/csv file"
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      output_file <- file.path(output_dir, "devices_by_facility.csv")

      # Save the result as a CSV file
      write.csv(devices_df, file = output_file, row.names = FALSE)

      message("Data saved to ", output_file)

      return(devices_df)
    } else {
      warning("No devices found for the specified facility.")
      return(data.frame())
    }
  } else {
    stop("REST request failed with status code ", httr::status_code(response))
  }
}
getDataByFacility <- function(session, target_filter, output_dir = NULL) {
  # GraphQL query to fetch data
  query <- 'query MyQuery {
    Data_findMany {
      target
      value
      uri
      variable
      offset
    }
  }'

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    if (!is.null(result$data$Data_findMany) && length(result$data$Data_findMany) > 0) {
      # Extract and filter data
      data <- lapply(result$data$Data_findMany, function(item) {
        data.frame(
          uri = ifelse(!is.null(item$uri), item$uri, NA),
          variable = ifelse(!is.null(item$variable), item$variable, NA),
          target = ifelse(!is.null(item$target), item$target, NA),
          value = ifelse(!is.null(item$value), item$value, NA),
          offset = ifelse(!is.null(item$offset), item$offset, NA),
          stringsAsFactors = FALSE
        )
      })

      data_df <- do.call(rbind, data)

      # Filter data by the target
      filtered_data <- data_df %>% filter(target == target_filter)

      if (nrow(filtered_data) == 0) {
        warning("No data found for the specified target.")
        return(data.frame())
      }

      # Output directory and file path
      if (is.null(output_dir)) {
        output_dir <- "/home/rebahi/Bureau/final/csv file"
      }
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      output_file <- file.path(output_dir, paste0("data_by_facility_", gsub("[^A-Za-z0-9]", "_", target_filter), ".csv"))

      # Save the filtered data to a CSV file
      write.csv(filtered_data[, c("uri", "variable", "target", "value")], file = output_file, row.names = FALSE)
      message("Filtered data saved to ", output_file)

      return(filtered_data)
    } else {
      warning("No data found in the API response.")
      return(data.frame())
    }
  } else {
    stop("GraphQL request failed with status code ", httr::status_code(response))
  }
}
#######################
#' List Movements by Scientific Object
#'
#' @param session List containing session information from login()
#' @param experiment_id Character string containing the experiment identifier
#' @param object_uri Character string containing the scientific object URI
#' @param output_dir Optional directory path for CSV output. If NULL, no file is written
#' @param debug Logical, if TRUE prints debug information
#'
#' @return A data frame containing movement information
#' @export
lsMoveByOs <- function(session, experiment_id, scientific_object_id) {
  # Define the GraphQL query with the experiment_id as an enum (no quotes)
  query <- sprintf('query MyQuery {
    ScientificObject(
      Experience: %s
      inferred: true
      filter: {_id: "%s"}
    ) {
      _id
      label
      positions {
        hasBeginning(source: %s) {
          inXSDDateTimeStamp
        }
        hasEnd(source: %s) {
          inXSDDateTimeStamp
        }
        position {
          targetPositions {
            target
          }
        }
        to {
          label
          withinSite {
            label
          }
        }
        isInstant
        from {
          label
          withinSite {
            label
          }
        }
        comment
        _id
      }
    }
  }', experiment_id, scientific_object_id, experiment_id, experiment_id)

  # Make the POST request to the GraphQL API
  response <- httr::POST(
    url = session$urlGraphql,
    httr::add_headers(Authorization = paste("Bearer", session$token)),
    body = list(query = query),
    encode = "json"
  )

  # Print raw response for debugging
  print(httr::content(response, "text", encoding = "UTF-8"))

  # Check if the request was successful
  if (httr::status_code(response) >= 200 && httr::status_code(response) < 300) {
    result <- httr::content(response, as = "parsed")

    # Check if the ScientificObject is available
    if (!is.null(result$data$ScientificObject) && length(result$data$ScientificObject) > 0) {
      scientific_object <- result$data$ScientificObject[[1]]
      positions <- scientific_object$positions

      if (!is.null(positions) && length(positions) > 0) {
        # Process positions into a data frame
        moves <- lapply(positions, function(pos) {
          data.frame(
            scientific_object_id = ifelse(!is.null(scientific_object$`_id`), scientific_object$`_id`, NA),
            scientific_object_label = ifelse(!is.null(scientific_object$label), scientific_object$label, NA),
            position_id = ifelse(!is.null(pos$`_id`), pos$`_id`, NA),
            has_beginning = ifelse(!is.null(pos$hasBeginning$inXSDDateTimeStamp), pos$hasBeginning$inXSDDateTimeStamp, NA),
            has_end = ifelse(!is.null(pos$hasEnd$inXSDDateTimeStamp), pos$hasEnd$inXSDDateTimeStamp, NA),
            is_instant = ifelse(!is.null(pos$isInstant), pos$isInstant, NA),
            from_label = ifelse(!is.null(pos$from$label), pos$from$label, NA),
            from_within_site = ifelse(!is.null(pos$from$withinSite$label), pos$from$withinSite$label, NA),
            to_label = ifelse(!is.null(pos$to$label), pos$to$label, NA),
            to_within_site = ifelse(!is.null(pos$to$withinSite$label), pos$to$withinSite$label, NA),
            target_positions = ifelse(!is.null(pos$position$targetPositions) && length(pos$position$targetPositions) > 0,
                                      paste(sapply(pos$position$targetPositions, function(t) t$target), collapse = ", "),
                                      NA),
            comment = ifelse(!is.null(pos$comment), pos$comment, NA),
            stringsAsFactors = FALSE
          )
        })

        # Combine all move data into a single data frame
        moves_df <- do.call(rbind, moves)

        # Save the result as a CSV file
        output_dir <- "/home/rebahi/Bureau/final/csv file"
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }

        output_file <- file.path(output_dir, "moves.csv")

        write.csv(moves_df, file = output_file, row.names = FALSE)

        message("Data saved to ", output_file)

        return(moves_df)
      } else {
        warning("No positions found for the specified scientific object.")
        return(data.frame())
      }
    } else {
      warning("No scientific object found for the specified ID.")
      return(data.frame())
    }
  } else {
    # Log the error message if the response fails
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    stop("GraphQL request failed with status code ", httr::status_code(response),
         ". Message: ", error_message)
  }
}



tryCatch({
  # Login
  session <- login(
    id = "admin@opensilex.org",
    password = "phisc1.61",
    instance = "http://138.102.159.36:8084",
    urlGraphql = "http://opensilex.org/graphql")

  print(session)
  all_experiments <- lsExp(session)
  result <- lsOsByExp(session, experiment_uri="SweetPotatoViruses_2018_2018_07_01")
  factors_df <- lsFactorsByExp(session, experiment_uri = "http://opensilex.test/id/experiment/sweetpotatoviruses_2018")
  variables <- lsVarByExp(session, experiment_uri = "http://opensilex.test/id/experiment/sweetpotatoviruses_2018")
  factor_levels <-  lsFlByExp(session, experiment_uri = "http://opensilex.test/id/experiment/sweetpotatoviruses_2018")
  factor_levels_by_factor <-  lsFlByFactors(session, factor_id="http://opensilex.test/id/factor/sweetpotatoviruses_2018.viral_disease")
  facility <- lsFacility(session, output_dir = "/home/rebahi/Bureau/final/csv file")
  varbyfacility <- lsVarByFacility(session, facility_uri="http://opensilex.test/id/scientific-object/so-dic09d11g05")
  databyos <- lsDataByOS(session, experiment_uri="SweetPotatoViruses_2018_2018_07_01", os_label="SP004008", output_dir = "/home/rebahi/Bureau/final/csv file")
  result <- lsDataByVar(session, variable_name ="http://opensilex.test/id/variable/plant_height_abovereferencefromsideview_centimeter")
  devicebyfacility <- lsDeviceByFacility(session, facility_uri="http://opensilex.test/id/scientific-object/so-dic09d11g05")

  result <- lsOsTypeByExp(session, experiment_uri = "SweetPotatoViruses_2018_2018_07_01")

  result <- getDataByFacility(session, target_filter = "http://opensilex.test/id/scientific-object/so-dic09d11g05")
  moves <- lsMoveByOs(
    session = session,
    experiment_id = "exp_test_2022_09_01",
    scientific_object_id = "http://opensilex.test/id/scientific-object/so-objet_1"
  )


  })
