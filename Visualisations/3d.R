library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(htmlwidgets)

create_3d_plot <- function(height_file_path, biomass_file_path, output_html_path = NULL, save_plot = TRUE) {
  # Check if files exist
  if (!file.exists(height_file_path) || !file.exists(biomass_file_path)) {
    stop("One or both input files do not exist. Please check the file paths.")
  }

  # Read height and biomass data
  height_data <- read.csv(height_file_path, stringsAsFactors = FALSE)
  biomass_data <- read.csv(biomass_file_path, stringsAsFactors = FALSE)

  # Convert date columns
  height_data$date <- as.Date(height_data$date)
  biomass_data$date <- as.Date(biomass_data$date)

  # Rename value columns
  height_data <- height_data %>% rename(height = value)
  biomass_data <- biomass_data %>% rename(biomass = value)

  # Ensure label column exists before merging
  if (!"label" %in% colnames(height_data) || !"label" %in% colnames(biomass_data)) {
    stop("The expected 'label' column is missing in one of the input files.")
  }

  # Merge datasets
  merged_data <- inner_join(height_data, biomass_data, by = c("label", "date")) %>% drop_na(height, biomass)

  # Create irrigation treatment variable
  merged_data$irrigation <- case_when(
    grepl("irrigation.ww", merged_data$label) ~ "well-watered",
    grepl("irrigation.wd", merged_data$label) ~ "water-deficit",
    TRUE ~ "unknown"
  )

  # Create the 3D plot
  fig <- plot_ly(data = merged_data,
                 x = ~date,
                 y = ~height,
                 z = ~biomass,
                 color = ~irrigation,
                 type = 'scatter3d',
                 mode = 'markers',
                 marker = list(size = 6, opacity = 0.8, symbol = 'circle'))

  fig <- fig %>% layout(title = "3D Plot of Plant Growth Data",
                        scene = list(
                          xaxis = list(title = "Date"),
                          yaxis = list(title = "Height (mm)"),
                          zaxis = list(title = "Biomass (g)")
                        ))

  # Save plot if requested
  if (save_plot) {
    if (is.null(output_html_path)) {
      output_html_path <- "plant_3d_plot.html"
    }
    htmlwidgets::saveWidget(fig, output_html_path, selfcontained = TRUE)
    message(paste("Plot saved to:", output_html_path))
  }

  return(fig)
}

# Example usage
my_3d_plot <- create_3d_plot(
  "/home/rebahi/Bureau/final/csv_files/plant_height_imageanalysis_millimetre_data.csv",
  "/home/rebahi/Bureau/final/csv_files/plant_biomass_imageanalysis_grams_data.csv",
  "/home/rebahi/Bureau/final/csv_files/plant_growth_3d_plot.html"
)
my_3d_plot
