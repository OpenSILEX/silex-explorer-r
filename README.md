<div align="center">

# SilexExplorerR

### R Interface for OpenSILEX

[![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue?style=flat-square&logo=r)](https://www.r-project.org/)
[![OpenSILEX](https://img.shields.io/badge/OpenSILEX-Platform-green?style=flat-square)](https://opensilex.org/)
[![GitHub](https://img.shields.io/badge/GitHub-Repository-181717?style=flat-square&logo=github)](https://github.com/OpenSILEX/silex-explorer-r)

[Installation](#installation) •
[Quick Start](#quick-start) •
[Contributing](#contributing)

---

</div>

## 📚 Overview

**SilexExplorerR** is an R package designed to facilitate access to, exploration, and visualization of data from OpenSILEX instances.

It provides functions to authenticate to an OpenSILEX instance and retrieve experiments, scientific objects, variables, facilities, devices, movements, environmental data, and experimental data.

The package relies on **SilexExplorer**, a GraphQL API for accessing and querying OpenSILEX data, while also interacting with OpenSILEX REST services when required.

SilexExplorerR also provides URI-name mapping to simplify data exploration using labels, optional CSV export, and visualization tools for exploring and comparing scientific data.

## 🎯 Key Features

- **User Authentication**:   
  The `login` function allows secure authentication to an OpenSILEX instance and facilitates seamless interaction with its REST and GraphQL services by:  
  - Providing a token-based authentication mechanism.  
  - Configuring and generating REST and GraphQL endpoints dynamically.  
  - Preparing headers for secure and efficient API communication.  
  - Supporting custom ports and flexible server setups.  
 
- **Experiment Exploration**:   
  The `lsExp` function allows you to retrieve and filter experiments from an OpenSILEX instance using a GraphQL query. Key features include:  
  - Filtering experiments by species, project. 
  - Exporting filtered results to a CSV file for further analysis.  

- **Scientific Object Exploration**:    
  Retrieve scientific object types and scientific objects associated with experiments using `lsOsTypeByExp()` and `lsOsByExp()`.

- **Variables and Experimental Data**  
  Explore variables associated with experiments and retrieve experimental data using functions such as `lsVarByExp()`, `getDataExp()`, and `getDataByVariable()`.

- **Facilities, Devices, and Environmental Data**  
  Explore facilities, devices, environmental variables, and environmental measurements using functions such as `lsFacilityByExp()`, `lsDevicesByFacility()`, `lsVarByFacility()`, `lsEnvDataByFacility()`, and `lsEnvDataByDevice()`.

- **Scientific Object Movements**  
  Retrieve movement information associated with scientific objects using `lsMoveByOs()`.

- **URI-Name Mapping**  
  Manage mappings between OpenSILEX URIs and human-readable labels to simplify data exploration and filtering. 
  
- **Data Visualization**  
  Explore and compare scientific data using the visualization and grouping utilities provided by the package.

- **CSV Export**  
  Optionally export retrieved data to CSV files for further analysis and reuse.

## ⚙️ Installation

### Prerequisites
SilexExplorerR requires **R >= 4.1.0**.

### Install from the INRAE forge


```r
install.packages("remotes")

remotes::install_git(
  "https://forge.inrae.fr/OpenSILEX/opensilex-graphql/silex-explorer-r.git"
)
```

### Install from GitHub
```r
install.packages("remotes")

remotes::install_github(
  "OpenSILEX/silex-explorer-r"
)
```

Once installed, load the package with:

```r
library(SilexExplorerR)
```

## 🚀 Quick Start

### 1. Connect to OpenSILEX

Use `login()` to authenticate to an OpenSILEX instance and configure access to the SilexExplorer GraphQL API.

```r
library(SilexExplorerR)

session <- login(
  id = "your_email@example.com",
  password = "your_password",
  instance = "http://your_opensilex_instance",
  urlGraphql = "http://your_silexexplorer_graphql_endpoint"
)
```
### 2. Explore Experiments

Retrieve the experiments available in the connected OpenSILEX instance:

```r
experiments <- lsExp(session)
```

You can also filter experiments by species, project, or date.

> For more detailed workflows and usage examples, see the [`examples`](./examples) directory

> ⚠️  **Important:**  

> For all functions where you filter using names or labels, the corresponding elements **must be loaded in advance** into the `uri_name` table.  
> 
> For example, if you want to filter the scientific objects of a specific experiment with label `"ZA17"` and object type `"Plant"`, you would normally write:  
> 
> ```r
> scientific_objects_by_exp_type <- lsOsByExp(
>     session, 
>     experiment_label = "ZA17", 
>     obj_type = "Plant"
> )
> ```  
> 
> Before doing this, you must first load the experiment and the object types to populate the `uri_name` table:  
> 
> ```r
> # Load all experiments
> all_experiments <- lsExp(session)
> 
> # Load object types for the specific experiment
> os_types <- lsOsTypeByExp(session, experiment_label = "ZA17")
> ```  
> 
> This ensures that all names and URIs required for filtering are available for the functions to work correctly.

## 📂 Project Structure

```text
silex-explorer-r/
├── R/              # R source code
├── man/            # Generated function documentation
├── examples/       # Usage examples
├── vignettes/      # Extended documentation
├── renv/           # Development environment configuration
├── DESCRIPTION     # Package metadata and dependencies
├── NAMESPACE       # Exports and imports
├── renv.lock       # Reproducible development dependencies
└── README.md       # Package overview and usage guide
```

The `man/` documentation and `NAMESPACE` file are generated from the Roxygen documentation contained in the source files.

## 🤝 Contributing

Contributions are welcome.

To contribute:

1. Fork or clone the repository.
2. Create a dedicated branch for your changes.
3. Implement and document your changes.
4. Check that the package builds correctly.
5. Submit a merge request or pull request.

Please keep function naming, documentation, and coding conventions consistent with the existing package.


---

<div align="center">

Developed by the OpenSILEX Team

[↑ Back to Top](#silexexplorerr)

</div>
