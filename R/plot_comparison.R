#' Visualisation des comparaisons par variable entre deux groupes
#'
#' Affiche pour chaque variable deux graphiques côte à côte :
#' 1. Les courbes individuelles des objets scientifiques (avec points).
#' 2. Les moyennes ± SD par groupe, dans le temps.
#'
#' @param df Un `data.frame` tidy tel que retourné par `prepare_data_for_comparison()`.
#' @param factor Le nom du facteur utilisé pour séparer les groupes (ex: `"genotype"`, `"treatment"`).
#'
#' @export
plot_comparison <- function(df, factor) {
  df <- df %>%
    mutate(
      Date = as.POSIXct(Date),
      Valeur = as.numeric(Valeur)
    ) %>%
    arrange(Date)

  groupes <- unique(df$Groupe)
  palette_colors <- scales::hue_pal()(length(groupes))
  names(palette_colors) <- groupes

  variables <- unique(df$Variable)

  for (var in variables) {
    # 🔎 Filtrage NA
    df_var <- df %>%
      filter(
        Variable == var,
        !is.na(Date),
        !is.na(Valeur),
        !is.na(URI)
      )

    # 📈 Courbes individuelles
    p1 <- ggplot(df_var, aes(x = Date, y = Valeur, group = URI, color = Groupe)) +
      geom_line(alpha = 0.5, na.rm = FALSE) +
      geom_point(size = 0.8, alpha = 0.4, na.rm = FALSE) +
      scale_color_manual(values = palette_colors) +
      labs(
        title = paste("Évolution individuelle -", var),
        x = "Date", y = var, color = factor
      ) +
      scale_x_datetime(date_labels = "%Y-%m-%d") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 📊 Moyenne + SD
    df_summary <- df_var %>%
      group_by(Date, Groupe) %>%
      summarise(
        Mean = mean(Valeur, na.rm = TRUE),
        SD = sd(Valeur, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(Mean), !is.na(SD))

    p2 <- ggplot(df_summary, aes(x = Date, y = Mean, color = Groupe, fill = Groupe)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD), alpha = 0.3, color = NA, na.rm = TRUE) +
      scale_color_manual(values = palette_colors) +
      scale_fill_manual(values = palette_colors) +
      labs(
        title = paste("Moyenne ± SD -", var),
        x = "Date", y = var, color = factor, fill = factor
      ) +
      scale_x_datetime(date_labels = "%Y-%m-%d") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # 🖼 Combiner les deux graphiques
    print(p1 + p2 + patchwork::plot_layout(ncol = 2))
  }
}
