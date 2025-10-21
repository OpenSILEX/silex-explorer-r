<div align="center">

# SilexExplorerR
###  R Interface for OpenSILEX Platform

[![R](https://img.shields.io/badge/R-%3E%3D%204.0.0-blue?style=flat-square&logo=r)](https://www.r-project.org/)
[![OpenSILEX](https://img.shields.io/badge/OpenSILEX-Platform-green?style=flat-square)](https://opensilex.org/)
[![Documentation](https://img.shields.io/badge/docs-latest-brightgreen.svg?style=flat-square)](https://opensilex.org/documentation)
[![GitHub](https://img.shields.io/badge/GitHub-Repository-181717?style=flat-square&logo=github)](https://github.com/OpenSILEX/silex-explorer-r)

[Documentation](https://opensilex.org/documentation) • 
[Installation Guide](#️-installation) • 
[Quick Start](#-quick-start) • 
[Examples](./examples) • 
[Contributing](#-contributing)
---

</div>

## 📚 Overview

**SilexExplorerR** is an R interface designed for seamless interaction with OpenSILEX platforms, enabling researchers to efficiently manage and analyze scientific experimental data. This package provides comprehensive tools for:

- 🧪 Experimental data management
- 📊 Data visualization and analysis
- 🌱 Phenotypic data processing
- 🌡️ Environmental data collection
- 📈 Time-series analysis
- 🔍 Advanced filtering capabilities

## 🎯 Key Features
## **Features**
- **User Authentication**:   
  The `login` function allows secure authentication to an OpenSILEX instance and facilitates seamless interaction with its REST and GraphQL services by:  
  - Providing a token-based authentication mechanism.  
  - Configuring and generating REST and GraphQL endpoints dynamically.  
  - Preparing headers for secure and efficient API communication.  
  - Supporting custom ports and flexible server setups.  
 
- **Experiment Retrieval and Filtering**:   
  The `lsExp` function allows you to retrieve and filter experiments from an OpenSILEX instance using a GraphQL query. Key features include:  
  - Filtering experiments by species_uri, project_uri, date,species_name or project name.    
  - Returning results as a pandas DataFrame. 
  - Exporting filtered results to a CSV file for further analysis.  

- **Scientific Object Types Retrieval**:    
  The `lsOsByExp` function retrieves all scientific object types associated with a specific experiment from an OpenSILEX instance. Key features include:   
  - Filtering scientific object types by experiment URI.  
  - Returning results, including URIs and namesas,as a pandas DataFrame.  
  - Exporting results, to a CSV file.  

- **Factors and Factor Levels Retrieval** :   
  The `lsFlByFactor` function retrieves all factors and their associated levels for a given experiment. Key features include:  
  - Retrieving factors and their levels using GraphQL queries.  
  - Storing the retrieved data, including factor URIs and level URIs, in a CSV file.  
  - Returning the results as a pandas DataFrame for further analysis or processing.  

- **Variables Retrieval by Experiment**:   
  The `lsVarByExp` function retrieves all variables for a given experiment. Key features include:  
  - Fetching variables and their associated metadata (e.g., entity, characteristic, method, and unit) for a specified experiment.  
  - Saving the retrieved data to a CSV file for further analysis.  
  - Returning the variables as a pandas DataFrame.   

- **Scientific Objects Retrieval by Experiment**:   
  The `lsOsByExp` function retrieves scientific objects and their associated details for a given experiment and object type. Key features include:  
  - Dynamic filtering for factor levels and germplasm (both by uri and name).  
  - Supporting GraphQL queries to fetch detailed information about the scientific objects, including factors and germplasm data.  
  - Saving the retrieved data to a CSV file for further analysis.  
  - Returning the data as a pandas DataFrame, with optional filters applied for factor levels and germplasm.

- **Data Retrieval by Variable**:   
  The `lsDataByVar` function retrieves data associated with scientific objects for a specified experiment and object type. Key features include:  
  - Supports optional filtering by factor levels and germplasm.  
  - Extracts data such as target, variable, value, and date for each scientific object.  
  - Organizes the extracted data by variable and exports each variable's data to separate CSV files.  
  - Returns a dictionary where keys are variable names and values are DataFrames containing the associated data.  
  - The data can be filtered based on a provided list of variables (e.g., sensor readings, measurements).  
  - Saves the CSV files in the `temp_files` directory.

- **Environmental Variables by Facility**:   
  The `lsVarByFacility` function retrieves detailed environmental variable information linked to a facility. Key features include:  
  - Allows filtering by a specific date range (optional).  
  - Fetches a list of unique variables associated with the facility for the given date range.  
  - Retrieves detailed information for each variable (e.g., entity, characteristic, method, unit).  
  - Saves the variable details into a CSV file (default: `facility_env_var.csv`).  
  - Returns the variable details as a DataFrame. 

- **Retrieve and Export Environmental Data by Facility**:   
  The `lsEnvironmentalDataByFacility` function fetches environmental data for a specific facility within a given date range. Key features include:  
  - Retrieves environmental data and exports it to CSV files, organized by variable.  
  - Allows filtering by a list of environmental variables (optional). If none provided, variables are fetched using `get_variable_by_facility`.  
  - Supports flexible date filtering (defaulting to today's date if no dates are provided).  
  - The data is saved as separate CSV files for each variable, with a customizable prefix.  
  - Returns a dictionary where keys are variable names and values are corresponding DataFrames. 

- **Retrieve and Export Devices by Facility**:   
  The `lsDeviceByFacility` function fetches devices associated with a specific facility using pagination. Key features include:  
  - Retrieves devices data for a given facility with pagination support.  
  - Each device is represented with its URI, type, and name. 
  - Saves the retrieved devices data to a CSV file.  
  - Returns a list of dictionaries containing device details.  

- **Retrieve Measured Data by Device**:   
  The `lsDatabyDevice` function retrieves measured data associated with a specific device and exports it to a CSV file. Key features include:  
  - Fetches measured data for a device based on a specified date range (optional).  
  - Data includes device URI, target, value, variable, and measurement date.  
  - Saves the data to a CSV file, with a customizable filename.  
  - Returns the data as a Pandas DataFrame.  

- **Retrieve and Export Moves for a Scientific Object**:  
  The `lsMoveByOs` function retrieves the movement history of a scientific object and exports it to a CSV file. Key features include:  
  - Retrieves moves based on the object's URI, experiment, and optional date range.  
  - The moves include information about the "from" and "to" locations and the start and end times of each move.  
  - Generates a CSV file with columns: From, To, HasBeginning, and HasEnd.  
  - Returns a list of moves, each containing the relevant details.  

### 🔐 Authentication System
```R
session <- login(
    id = "admin@opensilex.org",
    password = Sys.getenv("OPENSILEX_PWD"),
    instance = "http://localhost:8080",
    urlGraphql = "http://localhost:4000/graphql"
)
```
- Secure token-based authentication
- Dynamic endpoint configuration
- Session management
- Custom port support

### 🧪 Experiment Management
```R
# List all experiments
experiments <- lsExp(session)

# List experiments active on a specific date
active_experiments <- lsExp(session, date = "2025-01-10")
print(active_experiments)
 
```
- Comprehensive experiment listing
- Advanced filtering options
- Data frame output format
- Integrated facility mapping

### 📊 Data Analytics
```R
# Retrieve and analyze data
data <- lsDataByOs(session, object_uri = "object_uri") %>%
    group_by(variable) %>%
    summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n_observations = n()
    )
```
- Statistical analysis tools
- Time-series processing
- Data aggregation
- Visualization support

## ⚙️ Installation

### Prerequisites
```R
# Check R version (requires >= 4.0.0)
R.version.string

# Install required system dependencies
system("gcc --version")  # Ensure build tools are available
```

### Package Installation
```R
# Install devtools
if (!require("devtools")) install.packages("devtools")

# Install SilexExplorerR
devtools::install_git(
    "https://forgemia.inra.fr/OpenSILEX/opensilex-graphql/package-r.git",
    dependencies = TRUE,
    build_vignettes = TRUE
)
```

### Core Dependencies
```R
# Install core packages
install.packages(c(
    "httr",      # HTTP client
    "jsonlite",  # JSON parsing
    "dplyr",     # Data manipulation
    "tidyr",     # Data tidying
    "lubridate", # Date handling
    "ggplot2",   # Visualization
    "testthat"   # Testing
))
```

## 🚀 Quick Start

### Basic Workflow
```R
library(SilexExplorerR)
library(dplyr)
library(ggplot2)

# 1. Initialize session
session <- login(
    id = Sys.getenv("OPENSILEX_USER"),
    password = Sys.getenv("OPENSILEX_PWD"),
    instance = "http://localhost:8080",
    urlGraphql = "http://localhost:4000/graphql"
)

# 2. Retrieve experiments
experiments <- lsExp(session)

```

## 📂 Project Structure

```plaintext
opensilex-r-package/
├── R/                       # Source code
│   ├── auth/               # Authentication
│   │   └── login.R
│   ├── experiment/         # Experiment management
│   │   ├── lsExp.R
│   │   ├── lsOsByExp.R
│   │   └── lsVarByExp.R
│   ├── facility/           # Facility operations
│   │   ├── lsExpByFacility.R
│   │   └── lsDeviceByFacility.R
│   ├── scientific_object/  # Object management
│   │   ├── lsMoveByOs.R
│   │   └── lsDataByOs.R
│   └── utils/             # Utility functions
├── tests/                  # Unit tests
├── vignettes/             # Extended documentation
├── examples/              # Usage examples
├── man/                   # Function documentation
├── DESCRIPTION            # Package metadata
├── NAMESPACE             # Export declarations
└── README.md             # This file
```

## 📖 Function Reference

### Authentication
| Function | Description | Parameters |
|----------|-------------|------------|
| `login()` | Initialize session | `id`, `password`, `instance`, `urlGraphql` |

### Experiment Management
| Function | Description | Parameters |
|----------|-------------|------------|
| `lsExp()` | List experiments | `session` |
| `lsOsByExp()` | List objects | `session`, `experiment_uri` |
| `lsVarByExp()` | List variables | `session`, `experiment_uri` |

### Facility Operations
| Function | Description | Parameters |
|----------|-------------|------------|
| `lsDeviceByFacility()` | List facility devices | `session`, `facility_uri` |



### Development Guidelines
- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Add unit tests for new features
- Update documentation
- Maintain backward compatibility

## 📄 License

This project is licensed under the MIT License. See [LICENSE](LICENSE) file.

## 🌟 Citation

```bibtex
@software{SilexExplorerR2024,
    title = {SilexExplorerR: Advanced R Interface for OpenSILEX Platform},
    author = {OpenSILEX Team},
    year = {2024},
    url = {https://forgemia.inra.fr/OpenSILEX/opensilex-graphql}
}
```
## 🤝 Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch:
```bash
git checkout -b feature/amazing-feature
```
3. Make your changes
4. Run tests:
```R
devtools::test()
```
5. Submit a Pull Request




---

<div align="center">

Made with ❤️ by the OpenSILEX Team

[↑ Back to Top](#SilexExplorerR)

</div>
