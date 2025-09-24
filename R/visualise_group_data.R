#' Visualiser les données groupées par variable
#'
#' Cette fonction crée une série de graphiques temporels (un par variable) pour les objets scientifiques,
#' avec des couleurs spécifiques par URI. Elle combine ensuite les graphiques en une seule figure à l'aide du package `patchwork`.
#' Si un chemin est fourni, la visualisation est enregistrée dans un fichier.
#'
#' @param df_variables Une liste de `tibble`, chaque élément représentant une variable sous forme de `data.frame`
#' avec les colonnes `URI`, le nom de la variable, et `Date`.
#' @param output_file (Optionnel) Chemin vers un fichier image (ex: `"output.png"`). Si `NULL` ou vide, la visualisation n'est pas enregistrée.
#'
#' @return Aucun objet retourné. Affiche la figure combinée à l'écran et éventuellement sauvegarde le fichier.
#'
#' @details
#' - Les graphiques affichent les données dans le temps pour chaque URI, colorées selon une palette cohérente.
#' - Le type de visualisation (points ou ligne) dépend de la densité et complétude des données.
#' - Les légendes sont regroupées et affichées en bas.
#'
#' @import ggplot2
#' @importFrom dplyr filter arrange %>%
#' @importFrom tibble tibble
#' @importFrom lubridate ddays
#' @importFrom patchwork wrap_plots
#' @importFrom readr write_csv
#' @importFrom glue glue
#' @importFrom scales hue_pal
#' @importFrom tools toTitleCase
#' @importFrom grid unit
#' @importFrom graphics dev.size
#' @export
visualise_group_data <- function(df_variables, output_file = NULL) {
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

  if (!is.null(output_file) && output_file != "") {
    plot_size <- dev.size("in")
    ggsave(output_file, combined, width = plot_size[1], height = plot_size[2], dpi = 300, limitsize = FALSE)
    message("✅ Data visualisation saved to: ", output_file)
  } else {
    message("ℹ️ Aucune sauvegarde effectuée (output_file non spécifié).")
  }

  print(combined)
}
