# Example script to test lsVarByFacility function from the SilexExplorerR package

# Load the necessary package
library(SilexExplorerR)

# Log in to OpenSILEX
session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://your_instance_url",
  urlGraphql = "http://your_graphql_url"
)

# Example 1: Retrieve variables for a facility
variables_by_facility <- lsVarByFacility(session, facility_label = "greenhouse 1")
print(variables_by_facility)

# Example 2: Retrieve variables for a facility and save to a CSV file
variables_by_facility_csv <- lsVarByFacility(
  session,
  facility_label = "greenhouse 1",
  output_dir = "your_output_directory_here"
  )
print(variables_by_facility_csv)

# Example 3: Retrieve variables for a facility with a start date filter
variables_by_facility_beg <- lsVarByFacility(
  session,
  facility_label = "greenhouse 1",
  date_beginning = "2017-04-16"
)
print(variables_by_facility_beg)

# Example 4: Retrieve variables for a facility with an end date filter
variables_by_facility_end <- lsVarByFacility(
  session,
  facility_label = "greenhouse 1",
  date_end = "2017-04-16T20:00:00.000Z"
)
print(variables_by_facility_end)

# Example 5: Retrieve variables for a facility with both start and end dates
variables_by_facility_beg_end <- lsVarByFacility(
  session,
  facility_label = "greenhouse 1",
  date_beginning = "2017-04-16T20:00:00.000Z",
  date_end = "2017-04-16T22:00:00.000Z"
)
print(variables_by_facility_beg_end)
