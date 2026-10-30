# Example script to test lsOsTypeByExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(id = "your_email@example.com",
                 password = "your_password",
                 instance = "http://your_instance_url",
                 urlGraphql = "http://your_graphql_url")

# Example: Get object types from experiment and print result
os_types <- lsOsTypeByExp(session, experiment_label = "SweetPotatoViruses_2018")
print(os_types)

# Optionally save to a CSV
os_types_csv <- lsOsTypeByExp(session, experiment_label = "SweetPotatoViruses_2018", output_dir = "your_output_directory_here")
print(os_types_csv)
