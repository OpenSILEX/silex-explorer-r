# Example script to test lsOsByExp function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://138.102.159.36:8084",
  urlGraphql = "http://138.102.159.37/graphql"
)

# Example 1: Retrieve all scientific objects from an experiment
scientific_objects_by_exp <- lsOsByExp(session, experiment_label = "ZA17")
print(scientific_objects_by_exp)

# Example 2: Retrieve objects filtered by object type
scientific_objects_by_type <- lsOsByExp(session, experiment_label = "ZA17", obj_type = "Plant")
print(scientific_objects_by_type)

# Example 3: Retrieve objects filtered by a single factor level
scientific_objects_by_factor <- lsOsByExp(session, experiment_label = "ZA17", factor_levels = c("Irrigation.WD"))
print(scientific_objects_by_factor)

# Example 4: Retrieve objects filtered by multiple factor levels
scientific_objects_by_list_factors <- lsOsByExp(
  session,
  experiment_label = "ZA17",
  factor_levels = c("Irrigation.WD", "testfactor.WS")
)
print(scientific_objects_by_list_factors)

# Example 5: Retrieve objects filtered by germplasm type only
scientific_objects_by_germplasm_type <- lsOsByExp(session, experiment_label = "ZA17", germplasm_type = "Accession")
print(scientific_objects_by_germplasm_type)

# Example 6: Retrieve objects filtered by germplasm type and name
scientific_objects_by_germplasm <- lsOsByExp(
  session,
  experiment_label = "ZA17",
  germplasm_type = "Variety",
  germplasm_name = "testVariety"
)
print(scientific_objects_by_germplasm)
