# Example script to test lsFacilityByExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(id = "your_email@example.com",
                 password = "your_password",
                 instance = "your_instance_url_here",
                 urlGraphql = "your_graphql_url_here")

# Example 1: List all facilities used in experiments
all_facilities <- lsFacilityByExp(session)
print(all_facilities)

# Example 2: List facilities used in a specific experiment (e.g., ZA17)
facilities_by_exp <- lsFacilityByExp(session, exp_label = "ZA17")
print(facilities_by_exp)

# Example 3: List facilities and save results to a CSV file
facilities_by_exp <- lsFacilityByExp(session,
                                     exp_label = "ZA17",
                                     output_dir = "your_output_directory_here")
print(facilities_by_exp)
