# Load required libraries
library(tidyverse)
library(corrplot)
library(reshape2)
library(ggplot2)

# Function to create correlation heatmap for a single file
create_correlation_heatmap <- function(file_path, output_prefix = NULL) {
  # Extract file name for labeling
  file_name <- basename(file_path)

  # Read the CSV file
  data <- read.csv(file_path)

  # Remove any non-numeric columns for correlation calculation
  numeric_data <- data %>% select_if(is.numeric)

  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

  # Create basic correlation plot
  png(paste0(ifelse(is.null(output_prefix), "", paste0(output_prefix, "_")),
             "correlation_", gsub(".csv", "", file_name), ".png"),
      width = 1000, height = 800, res = 100)
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           title = paste("Correlation Heatmap -", file_name),
           mar = c(0, 0, 2, 0))
  dev.off()

  # Create ggplot2 heatmap (more customizable)
  cor_melted <- melt(cor_matrix)

  ggplot_heatmap <- ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    labs(title = paste("Correlation Heatmap -", file_name),
         x = "", y = "")

  # Save ggplot heatmap
  ggsave(paste0(ifelse(is.null(output_prefix), "", paste0(output_prefix, "_")),
                "ggplot_correlation_", gsub(".csv", "", file_name), ".png"),
         ggplot_heatmap, width = 10, height = 8)

  return(list(cor_matrix = cor_matrix, plot = ggplot_heatmap))
}

# Function to create combined correlation analysis for multiple files
compare_correlations <- function(file_paths, output_prefix = NULL) {
  # Create list to store correlation matrices
  cor_matrices <- list()
  file_names <- basename(file_paths)

  # Calculate correlation for each file
  for (i in 1:length(file_paths)) {
    data <- read.csv(file_paths[i])
    numeric_data <- data %>% select_if(is.numeric)
    cor_matrices[[file_names[i]]] <- cor(numeric_data, use = "pairwise.complete.obs")
  }

  # Find common column names across all datasets
  common_cols <- Reduce(intersect, lapply(cor_matrices, function(x) colnames(x)))

  if (length(common_cols) < 2) {
    warning("Not enough common numeric columns found across datasets for comparison")
    return(NULL)
  }

  # Filter correlation matrices to only include common columns
  cor_matrices_filtered <- lapply(cor_matrices, function(x) {
    x[common_cols, common_cols]
  })

  # Create a dataframe for differences between pairs of correlation matrices
  if (length(cor_matrices_filtered) > 1) {
    for (i in 1:(length(cor_matrices_filtered) - 1)) {
      for (j in (i + 1):length(cor_matrices_filtered)) {
        diff_matrix <- cor_matrices_filtered[[i]] - cor_matrices_filtered[[j]]

        diff_melted <- melt(diff_matrix)

        diff_plot <- ggplot(data = diff_melted, aes(x = Var1, y = Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                               midpoint = 0, limit = c(-1, 1), space = "Lab",
                               name = "Correlation\nDifference") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                plot.title = element_text(hjust = 0.5)) +
          coord_fixed() +
          geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
          labs(title = paste("Correlation Difference:",
                             names(cor_matrices_filtered)[i], "vs",
                             names(cor_matrices_filtered)[j]),
               x = "", y = "")

        ggsave(paste0(ifelse(is.null(output_prefix), "", paste0(output_prefix, "_")),
                      "correlation_diff_",
                      gsub(".csv", "", names(cor_matrices_filtered)[i]), "_vs_",
                      gsub(".csv", "", names(cor_matrices_filtered)[j]), ".png"),
               diff_plot, width = 10, height = 8)
      }
    }
  }

  return(cor_matrices_filtered)
}

# File paths
file_paths <- c(
  "/home/rebahi/Bureau/final/csv_file/v002_data.csv",
  "/home/rebahi/Bureau/final/csv_file/v003_data.csv",
  "/home/rebahi/Bureau/final/csv_file/v004_data.csv"
)

# Output directory for results
output_dir <- "/home/rebahi/Bureau/final/correlation_results/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Create individual heatmaps
results <- list()
for (file_path in file_paths) {
  results[[basename(file_path)]] <- create_correlation_heatmap(file_path, paste0(output_dir, "individual"))
}

# Create comparison between files
comparison_results <- compare_correlations(file_paths, paste0(output_dir, "comparison"))

# Generate a combined report
capture.output({
  cat("Correlation Analysis Report\n")
  cat("==========================\n\n")

  cat("Individual Files Analyzed:\n")
  for (file_path in file_paths) {
    cat("- ", basename(file_path), "\n")
  }

  cat("\nCorrelation Summary:\n")
  for (file_name in names(results)) {
    cat("\nFile: ", file_name, "\n")
    # Find top 5 positive correlations
    cor_melted <- melt(results[[file_name]]$cor_matrix)
    # Remove self-correlations
    cor_melted <- cor_melted[cor_melted$Var1 != cor_melted$Var2,]
    # Sort and get top correlations
    top_pos <- head(cor_melted[order(cor_melted$value, decreasing = TRUE),], 5)
    top_neg <- head(cor_melted[order(cor_melted$value),], 5)

    cat("  Top positive correlations:\n")
    for (i in 1:nrow(top_pos)) {
      cat("  - ", as.character(top_pos$Var1[i]), " & ",
          as.character(top_pos$Var2[i]), ": ",
          round(top_pos$value[i], 3), "\n")
    }

    cat("  Top negative correlations:\n")
    for (i in 1:nrow(top_neg)) {
      cat("  - ", as.character(top_neg$Var1[i]), " & ",
          as.character(top_neg$Var2[i]), ": ",
          round(top_neg$value[i], 3), "\n")
    }
  }
}, file = paste0(output_dir, "correlation_analysis_report.txt"))

cat("Correlation analysis complete. Results saved to:", output_dir, "\n")
