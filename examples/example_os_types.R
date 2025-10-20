# Example script to test lsOsTypeByExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(id = "your_email@example.com",
                 password = "your_password",
                 instance = "http://138.102.159.36:8084",
                 urlGraphql = "http://138.102.159.37/graphql")

# Example: Get object types from experiment and print result
os_types <- lsOsTypeByExp(session, experiment_label = "SweetPotatoViruses_2018")
print(os_types)

# Optionally save to a CSV
os_types_csv <- lsOsTypeByExp(session, experiment_label = "SweetPotatoViruses_2018", output_dir = "/chemin/vers/dossier")
print(os_types_csv)
