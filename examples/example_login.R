# Example: Log in to an OpenSILEX instance

library(SilexExplorerR)

# Specify connection information
id <- "your_email@example.com"
password <- "your_password"
instance <- "http://your_instance_url"
urlGraphql <- "http://your_graphql_url"

# Log in
session <- login(
  id = id,
  password = password,
  instance = instance,
  urlGraphql = urlGraphql
)

# Check the connection object
print(session)
