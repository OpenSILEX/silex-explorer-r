# Example script to test lsEnvDataByFacility function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# ----------------------------------------------------------
# Log in to OpenSILEX
# ----------------------------------------------------------
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://138.102.159.36:8084",
  urlGraphql = "http://138.102.159.37/graphql"
)

# ----------------------------------------------------------
# Example 1: Retrieve all environmental data for a facility
# ----------------------------------------------------------
env_data_all <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1"
)
print(env_data_all)

# ----------------------------------------------------------
# Example 2: Retrieve environmental data for a specific variable
# ----------------------------------------------------------
variable_names <- c("air_temperature_thermocouple_degreeCelsius")

env_data_var <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  variable_names = variable_names
)
print(env_data_var)

# ----------------------------------------------------------
# Example 3: Retrieve environmental data with a start date filter
# ----------------------------------------------------------
env_data_start <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  date_beginning = "2017-04-16"
)
print(env_data_start)

# ----------------------------------------------------------
# Example 4: Retrieve environmental data with an end date filter (ISO format)
# ----------------------------------------------------------
env_data_end <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  date_end = "2017-04-16T20:00:00.000Z"
)
print(env_data_end)

# ----------------------------------------------------------
# Example 5: Retrieve environmental data with both start and end dates
# ----------------------------------------------------------
env_data_beg_end <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  date_beginning = "2017-04-16T20:00:00.000Z",
  date_end = "2017-04-16T22:00:00.000Z"
)
print(env_data_beg_end)

# ----------------------------------------------------------
# Example 6: Retrieve environmental data for a variable with a date range
# ----------------------------------------------------------
env_data_var_range <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  variable_names = c("air_temperature_thermocouple_degreeCelsius"),
  date_beginning = "2017-04-16",
  date_end = "2017-04-18"
)
print(env_data_var_range)

# ----------------------------------------------------------
# Example 7: Retrieve environmental data and export CSV files
# ----------------------------------------------------------
env_data_csv <- lsEnvDataByFacility(
  session,
  facility_label = "greenhouse 1",
  variable_names = c("air_temperature_thermocouple_degreeCelsius"),
  output_dir = "/path/to/save"
)
print(env_data_csv)
