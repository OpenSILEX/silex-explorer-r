#' Retrieve Data by Variable from an Experiment
#'
#' Retrieves data associated with specific variables from a given experiment in OpenSILEX.
#' The function can filter by scientific object type and variable names, and optionally
#' export results as CSV files (one per variable).
#'
#' @param session An opensilex_connection object obtained from \code{login()}.
#' @param experiment_label Character, experiment label.
#' @param obj_type_label Character, scientific object type label.
#' @param variable_names Character vector or NULL. List of variable names to filter on (optional).
#' @param output_dir Character string or NULL. Directory path to save CSV files (if NULL, files are not saved).
#'
#' @return A named list of data frames, where each element corresponds to a variable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve and save data by variable for experiment "ZA17" and object type "Plant"
#' data_by_var <- lsDataByVar(session, "ZA17", "Plant",
#'                            output_dir = "your_output_directory_here")
#'
#' # Retrieve data without saving to CSV
#' data_by_var <- lsDataByVar(session, "ZA17", "Plant")
#' print(data_by_var)
#'
#' # Retrieve and save data by variable for experiment "MAU17-PG" and object type "Plot"
#' data_by_var <- lsDataByVar(session, "MAU17-PG", "Plot",
#'                            output_dir = "your_output_directory_here")

#' # Retrieve data for specific variables
#' variable_names <- c("BERRY_DO280", "BER_K_HPLC")
#' data_by_var <- lsDataByVar(session, "MAU17-PG", "Plot", variable_names,
#'                            output_dir = "your_output_directory_here")
#' }
#'
lsDataByVar <- function(session,
                        experiment_label,
                        obj_type_label,
                        variable_names = NULL,
                        output_dir = NULL) {
  #----------------------------------------------------------
  # 1️⃣ Retrieve the experiment ID and Object_type name
  #----------------------------------------------------------
  experiment_id <- get_experiment_id(experiment_label, session)

  obj_type <- getUrisFromName(obj_type_label)
  if(length(obj_type) > 1) {
    warning("Multiple URIs found, the first one will be used by default")
    obj_type <- obj_type[1]
  }

  # 2️⃣ Check/create output directory if CSV export is requested
  #----------------------------------------------------------
  save_to_csv <- !is.null(output_dir)

  if (save_to_csv) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
    }
  }
  #----------------------------------------------------------
  # 3️⃣ Retrieve data using GraphQL query
  #----------------------------------------------------------
  query_data <- '
     query GetScientificObjects($experience: [DataSource!]!, $objType: String!) {
       ScientificObject(inferred: true,
       Experience: $experience,
       filter: {
         type: $objType
       }) {
         label
         type: _type(inferred: true)
         data {
           target
           variable
           value
           date
         }
       }
     }
   '
  response <- POST(
    url = session$urlGraphql,
    add_headers(
      Authorization = paste("Bearer", session$token),
      "Content-Type" = "application/json"
    ),
    body = list(query = query_data, variables = list(
      experience = list(experiment_id),
      objType = obj_type
    )),
    encode = "json"
  )

  result <- httr::content(response, as = "parsed")

  if (length(result$data$ScientificObject) == 0) {
    stop("ERROR: No data retrieved for experiment: ", experiment_label)
  }

  all_data <- result$data$ScientificObject
  #----------------------------------------------------------
  # 4️⃣ Merge and clean data
  #----------------------------------------------------------
  if (length(all_data) > 0) {

    # Use map_dfr to apply a function to each element of all_data and combine results
    all_data_df <- purrr::map_dfr(all_data, function(x) {

      # Check if x$data is not empty
      if (!is.null(x$data) && length(x$data) > 0) {

        # Convert x$data into a data frame
        df <- as.data.frame(do.call(rbind, lapply(x$data, function(d) {

          # Handle missing data
          target_val <- ifelse(!is.null(d$target), d$target, NA)
          variable_val <- ifelse(!is.null(d$variable), d$variable, NA)
          value_val <- ifelse(!is.null(d$value), as.character(d$value), NA)
          date_val <- ifelse(!is.null(d$date), as.character(d$date), NA)

          # Create data frame with validated values, excluding 'label'
          data.frame(
            target = target_val,
            variable = variable_val,
            value = value_val,
            date = date_val
          )
        })))

        # Return the data frame for this scientific object
        return(df)

      } else {
        return(NULL)  # Return NULL if x$data is empty or NULL
      }
    })
  }
  if (nrow(all_data_df) > 0) {
    # If all_data_df is not empty, select relevant columns
    all_data_df <- all_data_df %>%
      dplyr::select(target, variable, value, date)

    #----------------------------------------------------------
    # 5️⃣ Filter by variable_names (if provided)
    #----------------------------------------------------------
    split_data <- split(all_data_df, all_data_df$variable)

    variable_uris <- NULL  # Initialize variable_uris
    if (!is.null(variable_names)) {
      # Retrieve URIs for each variable name
      variable_uris <- sapply(variable_names, function(var_name) {
        uris <- getUrisFromName(var_name)
        if (length(uris) > 1) {
          warning(paste("Multiple URIs found for variable:", var_name, "the first one will be used"))
          return(uris[1])
        } else if (length(uris) == 0) {
          warning(paste("No URI found for variable:", var_name))
          return(NULL)
        }
        return(uris[1])
      })
      variable_uris <- variable_uris[!is.null(variable_uris)]  # Remove NULLs

      if (length(variable_uris) == 0) {
        warning("WARNING: No valid URIs found for the provided variable names.")
        return(list())  # Return empty list if no valid URIs
      }
    }

    if (length(variable_uris) > 0) {
      split_data <- split_data[names(split_data) %in% variable_uris]
      if (length(split_data) == 0) {
        warning("WARNING: None of the provided variable URIs were found in the data.")
        return(list())  # Return empty list if no match
      }
    }

    #----------------------------------------------------------
    # 6️⃣ Export CSV (optional) + return list of dataframes
    #----------------------------------------------------------
    results <- list()

    for (var_uri in names(split_data)) {
      df <- split_data[[var_uri]] %>%
        dplyr::select(uri = target, value, date)

      results[[var_uri]] <- df

      # Save to CSV if output_dir is provided
      if (save_to_csv) {
        var_name <- getNamesFromUri(var_uri)
        if(length(var_name) > 1) {
          warning("Multiple URIs found, the first one will be used by default")
          var_name <- var_name[1]
        }
        name_clean <- gsub("[^A-Za-z0-9_-]", "_", var_name)

        # Replace spaces with underscores
        var_name <- gsub("\\s+", "_", name_clean)
        csv_path <- file.path(output_dir, paste0(var_name, "_data.csv"))

        tryCatch({
          write.csv(df, csv_path, row.names = FALSE)
          message("OK: Saved data for variable '", var_name, "' -> ", csv_path)
        }, error = function(e) {
          warning("WARNING: Failed to save ", var_name, ": ", e$message)
        })
      }
    }

    return(results)  # Return a list of dataframes, one per variable
  } else {
    # If all_data_df is empty
    message("ERROR: No available data.")
    return(list())  # Return empty list
  }
}
