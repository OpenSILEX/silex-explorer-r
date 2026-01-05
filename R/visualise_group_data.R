#' Visualize Grouped Scientific Data by Variable
#'
#' This function generates time-series plots for each variable of a group of scientific objects.
#' Each object (identified by its URI) is displayed in a consistent color across all plots.
#' The individual plots are then combined into a single figure using the \pkg{patchwork} package.
#'
#' If an output directory is provided, the combined visualization is automatically saved as a PDF
#' file named \code{"visualisation_group_<group_id>.pdf"} in the specified directory. The PDF size
#' is dynamically adjusted based on the number of variables to preserve readability.
#'
#' @param df_variables A named list of \code{tibble} objects. Each element represents a variable and must contain at least the columns \code{URI}, the variable values, and \code{Date}.
#' @param group_id Character string identifying the group being visualized. This is used to name the output PDF.
#' @param output_dir Optional character string specifying the directory where the visualization PDF will be saved.
#'   If \code{NULL} or empty, the visualization is displayed but not saved.
#'
#' @return
#' No object is returned. The combined visualization is displayed on screen and optionally saved as a PDF.
#'
#' @details
#' The function performs the following:
#' \itemize{
#'   \item Creates one time-series plot per variable.
#'   \item Colors observations consistently by scientific object URI across all plots.
#'   \item Chooses between points or lines based on data density and completeness.
#'   \item Collects a shared legend at the bottom of the figure.
#'   \item Combines all plots using \code{patchwork} into a single layout.
#'   \item Saves the visualization as a PDF in \code{output_dir} if provided. The PDF width and height
#'         are automatically computed based on the number of variables.
#' }
#'
#' The output file is saved with the following pattern:
#' \preformatted{
#' output_dir/visualisation_group_<group_id>.pdf
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter arrange pull %>%
#' @importFrom tibble tibble
#' @importFrom lubridate ddays
#' @importFrom patchwork wrap_plots
#' @importFrom scales hue_pal
#' @importFrom tools toTitleCase
#' @importFrom grid unit
#' @importFrom grDevices dev.size
#' @export


visualise_group_data <- function(df_variables,group_id, output_dir = NULL) {
  # Vérifier si df_variables est vide ou contient uniquement des tibbles vides
  if (length(df_variables) == 0 || all(sapply(df_variables, nrow) == 0)) {
    message("ℹ️ Aucun donnée à visualiser pour le groupe '", group_id, "'.")
    return(invisible(NULL))
  }
  plots <- list()
  all_uris <- unique(unlist(lapply(df_variables, function(df) unique(df$URI))))
  palette_colors <- scales::hue_pal()(length(all_uris))
  names(palette_colors) <- all_uris

  for (var_name in names(df_variables)) {
    df <- df_variables[[var_name]]
    variable_name <- gsub("^df_", "", var_name)

    if (!inherits(df$Date, "POSIXct") && !inherits(df$Date, "Date")) {
      df$Date <- as.POSIXct(df$Date, format = "%Y-%m-%d", tz = "UTC")
    }
    df <- df %>% arrange(Date)

    p <- ggplot()

    for (uri in unique(df$URI)) {
      df_uri <- df %>% filter(URI == uri & !is.na(.data[[variable_name]]))
      if (nrow(df_uri) == 0) next

      dates_sorted <- sort(df_uri$Date)
      avg_gap_days <- if (length(dates_sorted) > 1) mean(diff(dates_sorted)) / ddays(1) else Inf
      has_na <- any(is.na(df %>% filter(URI == uri) %>% pull(variable_name)))
      color <- palette_colors[uri]

      if (has_na) {
        p <- p + geom_point(data = df_uri, aes(x = Date, y = .data[[variable_name]]),
                            color = color, size = 1.8, show.legend = FALSE)
      } else if (nrow(df_uri) >= 3 && avg_gap_days < 3) {
        p <- p + geom_line(data = df_uri, aes(x = Date, y = .data[[variable_name]], group = URI),
                           color = color, size = 0.6, show.legend = FALSE)
      } else {
        p <- p + geom_point(data = df_uri, aes(x = Date, y = .data[[variable_name]]),
                            color = color, size = 1.8, show.legend = FALSE)
      }
    }

    legend_df <- data.frame(
      URI = factor(all_uris, levels = all_uris),
      x = as.POSIXct(NA),
      y = NA_real_
    )

    p <- p +
      geom_point(data = legend_df, aes(x = x, y = y, color = URI),
                 shape = 15, size = 4, show.legend = TRUE, na.rm = TRUE) +
      scale_color_manual(values = palette_colors, breaks = all_uris) +
      labs(
        title = tools::toTitleCase(gsub("_", " ", variable_name)),
        x = "Date",
        y = tools::toTitleCase(gsub("_", " ", variable_name)),
        color = "Scientific Object (URI)"
      ) +
      {
        if (inherits(df$Date, "POSIXct")) {
          scale_x_datetime(
            breaks = pretty(df$Date, n = 10),
            date_labels = "%Y-%m-%d",
            expand = expansion(mult = c(0.01, 0.01))
          )
        } else {
          scale_x_date(
            breaks = pretty(df$Date, n = 10),
            date_labels = "%Y-%m-%d",
            expand = expansion(mult = c(0.01, 0.01))
          )
        }
      } +
      scale_y_continuous(breaks = pretty(df[[variable_name]], n = 7), expand = expansion(mult = c(0.05, 0.05))) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        legend.key.height = unit(8, "pt"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        plot.margin = margin(10, 15, 25, 10)
      )

    plots[[length(plots) + 1]] <- p
  }

  n_plots <- length(plots)
  ncol <- 2
  nrow <- ceiling(n_plots / ncol)

  combined <- patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow, guides = "collect") &
    theme(legend.position = "bottom") &
    guides(color = guide_legend(ncol = 2, override.aes = list(shape = 15, size = 4)))

  if (!is.null(output_dir) && output_dir != "") {
    # Create output directory if it does not exist
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Build output file path
    output_file <- file.path(
      output_dir,
      paste0("visualisation_group_", group_id, ".pdf")
    )

    # Determine number of plots and layout
    n_plots <- length(plots)
    ncol <- 2
    nrow <- ceiling(n_plots / ncol)

    # Automatic sizing per plot
    base_width <- 6   # inches per plot
    base_height <- 4  # inches per plot
    width <- base_width * ncol
    height <- base_height * nrow

    # Save PDF
    ggsave(
      filename = output_file,
      plot = combined,
      width = width,
      height = height,
      units = "in",
      dpi = 300,
      limitsize = FALSE
    )

    message("✅ Data visualization saved to: ", output_file)
  }


  print(combined)
}
