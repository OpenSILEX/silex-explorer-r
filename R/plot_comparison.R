#' Visualize variable-wise comparisons between two groups
#'
#' For each variable, this function displays two plots side by side:
#' \enumerate{
#'   \item Individual time-series curves for each scientific object (with points).
#'   \item Group-level mean trajectories with standard deviation (± SD) over time.
#' }
#'
#' @param df A tidy `data.frame` as returned by
#'   `prepare_data_for_comparison()`, containing measurements from two groups
#'   and a column identifying the comparison factor.
#' @param factor Name of the experimental factor used to define and label the
#'   groups being compared (e.g. `"genotype"`, `"treatment"`). This name is used
#'   only for plot labeling and legends.
#' @param output_dir Optional path to a directory where generated comparison
#'   plots will be saved. If NULL, plots are only displayed.
#'
#' @export
plot_comparison <- function(df, factor, output_dir = NULL) {

  # Convert columns to proper type
  df <- df %>%
    dplyr::mutate(
      Date = as.POSIXct(Date),
      Valeur = as.numeric(Valeur)
    ) %>%
    dplyr::arrange(Date)

  # Create output directory if needed
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Unique groups and palette
  groupes <- unique(df$Groupe)
  palette_colors <- scales::hue_pal()(length(groupes))
  names(palette_colors) <- groupes

  # Loop over variables
  variables <- unique(df$Variable)

  for (var in variables) {

    # Filter data for current variable
    df_var <- df %>%
      dplyr::filter(
        Variable == var,
        !is.na(Date),
        !is.na(Valeur),
        !is.na(URI)
      )

    # ---- Individual trajectories ----
    p1 <- ggplot2::ggplot(df_var, ggplot2::aes(x = Date, y = Valeur, group = URI, color = Groupe)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point(size = 0.8, alpha = 0.4) +
      ggplot2::scale_color_manual(values = palette_colors) +
      ggplot2::labs(
        title = paste("Individual trajectories -", var),
        x = "Date", y = var, color = factor
      ) +
      ggplot2::scale_x_datetime(date_labels = "%Y-%m-%d") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    # ---- Group mean ± SD ----
    df_summary <- df_var %>%
      dplyr::group_by(Date, Groupe) %>%
      dplyr::summarise(
        Mean = mean(Valeur, na.rm = TRUE),
        SD = stats::sd(Valeur, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(Mean), !is.na(SD))

    p2 <- ggplot2::ggplot(df_summary, ggplot2::aes(x = Date, y = Mean, color = Groupe, fill = Groupe)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.3, color = NA) +
      ggplot2::scale_color_manual(values = palette_colors) +
      ggplot2::scale_fill_manual(values = palette_colors) +
      ggplot2::labs(
        title = paste("Mean ± SD -", var),
        x = "Date", y = var, color = factor, fill = factor
      ) +
      ggplot2::scale_x_datetime(date_labels = "%Y-%m-%d") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    # ---- Combine and save/display ----
    combined_plot <- p1 + p2 + patchwork::plot_layout(ncol = 2)

    if (!is.null(output_dir)) {
      file_name <- paste0("comparaison_FL_", factor, "_", var, ".png")
      file_path <- file.path(output_dir, file_name)

      ggplot2::ggsave(
        filename = file_path,
        plot = combined_plot,
        width = 12,
        height = 6,
        dpi = 300
      )
    }

    # Display plot
    print(combined_plot)
  }
}
