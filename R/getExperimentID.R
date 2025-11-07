#' Retrieve the Experiment ID by Experiment Name
#'
#' This function fetches the experiment URI based on the experiment's name,
#' performs a GraphQL query to retrieve the experiment details (label and start date),
#' and then constructs a unique experiment ID in the format `EXP_<clean_label>_<formatted_date>`.
#'
#' @param experiment_name Character string. The name of the experiment to retrieve the ID for.
#' @param session A list containing session details of authentification
#' @return A character string containing the formatted experiment ID.
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming 'session' is a valid session object with necessary details
#' # Call the function with an experiment name
#' experiment_id <- get_experiment_id("Experiment_A", session)
#' print(experiment_id)  # Output: "EXP_Experiment_A_2017_03_30"
#' }
#'
#' @seealso `getUrisFromName` for fetching the URI from the experiment name.
get_experiment_id <- function(experiment_name, session) {
  tryCatch({
    # Get URI based on the experiment name using the provided function
    experiment_uri <- getUrisFromName(experiment_name)

    # GraphQL query to fetch the label and startDate of the experiment
    graphql_query <- '
      query MyQuery($id: [ID]) {
        Experiment(filter: {_id: $id}) {
          label
          startDate
        }
      }
    '

    # Perform the POST request to the GraphQL API
    response <- POST(
      session$urlGraphql,
      body = list(query = graphql_query, variables = list(id = list(experiment_uri))),
      encode = "json",
      httr::add_headers(Authorization = paste("Bearer", session$token))
    )

    # Check if the response was successful
    if (status_code(response) != 200) {
      stop("❌ Request failed with status: ", status_code(response))
    }

    # Parse the response content into a JSON format
    json_response <- content(response, "text")
    json_data <- jsonlite::fromJSON(json_response)

    # Check for errors in the GraphQL response
    if (!is.null(json_data$errors)) {
      stop("❌ GraphQL error: ", json_data$errors[[1]]$message)
    }

    # Retrieve the experiment data
    experiment_data <- json_data$data$Experiment

    if (length(experiment_data) == 0) {
      stop("❌ No experiment found for the provided URI")
    }

    # Extract the label and startDate from the experiment data
    label <- experiment_data$label[1]
    start_date <- experiment_data$startDate[1]

    # Format the start date to replace hyphens with underscores
    formatted_date <- gsub("-", "_", substr(start_date, 1, 10))

    # Clean the label by replacing non-alphanumeric characters with underscores
    clean_label <- gsub("[^A-Za-z0-9]", "_", label)

    # Construct the final experiment ID
    experiment_id <- paste("EXP", clean_label, formatted_date, sep = "_")

    # Return the final experiment ID
    return(experiment_id)

  }, error = function(e) {
    stop("❌ Unable to retrieve experiment ID: ", conditionMessage(e))
  })
}
