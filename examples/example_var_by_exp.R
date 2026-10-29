# Example script to test lsVarByExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(id = "your_email@example.com",
                 password = "your_password",
                 instance = "http://your_instance_url",
                 urlGraphql = "http://your_graphql_url")

# Example 1: Retrieve variables for an experiment
variables_by_exp <- lsVarByExp(session, experiment_label = "experiment_001")
print(variables_by_exp)

# Example 2: Retrieve variables for an experiment and save to a CSV file
variables_by_exp_with_csv <- lsVarByExp(session, experiment_label = "experiment_002", output_dir = "your_output_directory_here")
print(variables_by_exp_with_csv)

