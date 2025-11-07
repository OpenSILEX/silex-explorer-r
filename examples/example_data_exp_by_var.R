# Example script to test lsDataByVar function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://138.102.159.36:8084",
  urlGraphql = "http://138.102.159.37/graphql"
)

# -------------------------------------------------------------------------
# Example 1: Retrieve data for all variables for an experiment and save to CSV
# -------------------------------------------------------------------------
data_by_var <- lsDataByVar(
  session,
  experiment_label = "ZA17",
  obj_type_label = "Plant",
  output_dir = "your_output_directory_here"
)
print(data_by_var)

# -------------------------------------------------------------------------
# Example 2: Retrieve data by variable for an experiment (without saving)
# -------------------------------------------------------------------------
data_by_var_no_csv <- lsDataByVar(
  session,
  experiment_label = "ZA17",
  obj_type_label = "Plant"
)
print(data_by_var_no_csv)


# -------------------------------------------------------------------------
# Example 3: Retrieve data for specific variable names and save to CSV
# -------------------------------------------------------------------------
variable_names <- c("BERRY_DO280", "BER_K_HPLC")

data_by_var_filtered <- lsDataByVar(
  session,
  experiment_label = "MAU17-PG",
  obj_type_label = "Plot",
  variable_names = variable_names,
  output_dir = "/home/abidri/Bureau/packageR/silex-explorer-r/temp_files"
)
print(data_by_var_filtered)


