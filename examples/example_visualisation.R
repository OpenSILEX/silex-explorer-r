# ============================================================================
# Example script: Visualization and comparison of scientific object groups
# from the SilexExplorerR package
# ============================================================================

# Load the package
library(SilexExplorerR)

# -------------------------------------------------------------------------
# Log in to OpenSILEX
# -------------------------------------------------------------------------
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://your_instance_url",
  urlGraphql = "http://your_graphql_url"
)

# -------------------------------------------------------------------------
# Example 1: Retrieve scientific objects for an experiment
# -------------------------------------------------------------------------
# Get all scientific objects of type "Plant" for experiment ZA17
scientific_objects <- lsOsByExp(
  session,
  experiment_label = "ZA17",
  obj_type = "Plant",
  output_dir = "your_output_directory_here"
)

print(scientific_objects)

# -------------------------------------------------------------------------
# Example 2: Group scientific objects
# -------------------------------------------------------------------------
# Group objects based on available metadata (e.g. genotype, irrigation, etc.)
group_summary <- groupScientificObjects(
  scientific_objects,
  output_dir = "your_output_directory_here"
)

print(names(group_summary))

# -------------------------------------------------------------------------
# Example 3: Extract scientific objects from a single group
# -------------------------------------------------------------------------
# Select one group by its identifier
group_of_os <- extractGroupObjects(
  group_summary,
  group_id = "WW_ZM4351_SeedLot_Zea mays",
  output_dir = "your_output_directory_here"
)

print(group_of_os)

# -------------------------------------------------------------------------
# Example 4: Retrieve measurement data for one group
# -------------------------------------------------------------------------
# Retrieve all variable measurements for the selected group
group_data <- getDataOs(
  session,
  exp_name = "ZA17",
  df_os = group_of_os,
  output_dir = "your_output_directory_here"
)

print(group_data)

# -------------------------------------------------------------------------
# Example 5: Compare two groups according to a given factor
# -------------------------------------------------------------------------
# Compare two irrigation treatments and generate visualizations
df_result <- compareGroupsByFactorLevel(
  session = session,
  group_dict = group_summary,
  experiment_id = "ZA17",
  group1_id = "WD_ZM4379_SeedLot_Zea mays",
  group2_id = "WW_ZM4379_SeedLot_Zea mays",
  factor = "Irrigation",
  output_dir = "your_output_directory_here"
)

print(df_result)

# -------------------------------------------------------------------------
# Notes:
# - If output_dir is provided, plots are saved as PNG files.
# - If output_dir is NULL, plots are only displayed.
# - The returned data frame (df_result) can be reused for custom analyses.
# -------------------------------------------------------------------------
