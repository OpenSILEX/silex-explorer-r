# Example script to test lsExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(id = "your_email@example.com",
                 password = "your_password",
                 instance = "http://138.102.159.36:8084",
                 urlGraphql = "http://138.102.159.37/graphql")

# Example 1: List all experiments and save to a CSV file
all_experiments <- lsExp(session, output_dir = "/chemin")
print(all_experiments)

# Example 2: List experiments active on a specific date (2025-01-10)
active_experiments <- lsExp(session, date = "2025-01-10")
print(active_experiments)

# Example 3: List experiments filtered by project (EPPN2020)
expe_by_project <- lsExp(session, projet = "EPPN2020")
print(expe_by_project)

# Example 4: List experiments filtered by species (Zea mays)
expe_by_species <- lsExp(session, Species = "Zea mays")
print(expe_by_species)

# Example 5: List experiments filtered by both species and project
expe_by_species_project <- lsExp(session, date = "2017-05-18", projet = "French plant phenomic network (FPPN)", Species = "Zea mays")
print(expe_by_species_project)
