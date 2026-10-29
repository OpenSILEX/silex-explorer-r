# Example script to test getDataExp function from the SilexExplorerR package
# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://your_instance_url",
  urlGraphql = "http://your_graphql_url"
)

# -------------------------------------------------------------------------
# Example 1: Retrieve all data from an experiment and save to CSV
# -------------------------------------------------------------------------


ls_data <- getDataExp(
  session,
  experiment_uri = "http://your_experiment_uri",
  output_dir = "your_output_directory_here"
)

# -------------------------------------------------------------------------
# Example 2: Retrieve all data without saving to CSV
# -------------------------------------------------------------------------
ls_data_no_csv <- getDataExp(
  session,
  experiment_uri = "http://your_experiment_uri"
)

print(ls_data_no_csv)

# -------------------------------------------------------------------------
# Example 3: Retrieve data for specific variables and save to CSV
# -------------------------------------------------------------------------
variable_names <- c("supply_voltage_measurement_volt")

ls_data_filtered <- getDataExp(
  session,
  experiment_uri = "http://your_experiment_uri",
  variable_names = variable_names,
  output_dir = "your_output_directory_here"
)

print(ls_data_filtered)
