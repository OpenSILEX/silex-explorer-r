# Example script to test getDataByVariable function from the SilexExplorerR package

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
# Example 1: Retrieve data for all variables for an experiment and save to CSV
# -------------------------------------------------------------------------
data_by_var <- getDataByVariable(
  session,
  experiment_label = "ZA17",
  obj_type_label = "Plant",
  output_dir = "your_output_directory_here"
)
print(data_by_var)

# -------------------------------------------------------------------------
# Example 2: Retrieve data by variable for an experiment (without saving)
# -------------------------------------------------------------------------
data_by_var_no_csv <- getDataByVariable(
  session,
  experiment_label = "ZA17",
  obj_type_label = "Plant"
)
print(data_by_var_no_csv)


# -------------------------------------------------------------------------
# Example 3: Retrieve data for specific variable names and save to CSV
# -------------------------------------------------------------------------
variable_names <- c("BERRY_DO280", "BER_K_HPLC")

data_by_var_filtered <- getDataByVariable(
  session,
  experiment_label = "MAU17-PG",
  obj_type_label = "Plot",
  variable_names = variable_names,
  output_dir = "your_output_directory_here")
print(data_by_var_filtered)


