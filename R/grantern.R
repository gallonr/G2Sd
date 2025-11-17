grantern <-
  function(
    x,
    main = "Ternary Diagram",
    show_labels = TRUE,
    point_size = 3,
    point_color = "steelblue",
    show_grid = TRUE
  ) {
    # Vérifier la compatibilité des données
    x <- .grancompat(x)

    # Extraire les données de texture (Gravel, Sand, Mud)
    texture_data <- .texture.sedim(x)

    # Sélectionner uniquement les colonnes nécessaires et gérer les NA
    tern_data <- texture_data |>
      select(samples, Gravel, Sand, Mud) |>
      mutate(
        Gravel = replace_na(Gravel, 0),
        Sand = replace_na(Sand, 0),
        Mud = replace_na(Mud, 0)
      )

    # Vérifier que la somme fait bien 100% (avec tolérance)
    tern_data <- tern_data |>
      mutate(total = Gravel + Sand + Mud) |>
      mutate(
        Gravel = (Gravel / total) * 100,
        Sand = (Sand / total) * 100,
        Mud = (Mud / total) * 100
      ) |>
      select(-total)

    # Fonction pour convertir coordonnées ternaires en coordonnées cartésiennes
    # Gravel = coin inférieur gauche (0,0)
    # Sand = coin inférieur droit (1,0)
    # Mud = coin supérieur (0.5, sqrt(3)/2)
    tern_data <- tern_data |>
      mutate(
        x = (Sand + 0.5 * Mud) / 100,
        y = (sqrt(3) / 2 * Mud) / 100
      )

    # Créer le triangle de base
    triangle <- data.frame(
      x = c(0, 1, 0.5, 0),
      y = c(0, 0, sqrt(3) / 2, 0)
    )

    # Créer le graphique de base
    p <- ggplot(tern_data) +
      geom_polygon(
        data = triangle,
        aes(x = x, y = y),
        fill = NA,
        color = "black",
        linewidth = 1
      ) +
      coord_fixed(xlim = c(-0.15, 1.15), ylim = c(-0.15, sqrt(3)/2 + 0.15)) +
      theme_void() +
      labs(title = main) +
      theme(
        plot.title = element_text(
          size = rel(1.3),
          face = "bold",
          hjust = 0.5
        ),
        plot.margin = margin(20, 20, 20, 20)
      )

    # Ajouter la grille si demandé
    if (show_grid) {
      # Créer les lignes de grille (tous les 10%)
      grid_lines <- data.frame()
      for (i in seq(10, 90, by = 10)) {
        pct <- i / 100
        # Lignes parallèles au côté Gravel-Sand (horizontales)
        grid_lines <- rbind(
          grid_lines,
          data.frame(
            x = c(0.5 * pct, 1 - 0.5 * pct),
            y = c(sqrt(3) / 2 * pct, sqrt(3) / 2 * pct),
            group = paste0("h", i)
          )
        )
        # Lignes parallèles au côté Gravel-Mud
        grid_lines <- rbind(
          grid_lines,
          data.frame(
            x = c(pct, 0.5 + 0.5 * pct),
            y = c(0, sqrt(3) / 2 * (1 - pct)),
            group = paste0("g", i)
          )
        )
        # Lignes parallèles au côté Sand-Mud
        grid_lines <- rbind(
          grid_lines,
          data.frame(
            x = c(1 - pct, 0.5 - 0.5 * pct),
            y = c(0, sqrt(3) / 2 * (1 - pct)),
            group = paste0("s", i)
          )
        )
      }

      p <- p +
        geom_line(
          data = grid_lines,
          aes(x = x, y = y, group = group),
          color = "gray80",
          linewidth = 0.3
        )
    }

    # Ajouter les points
    p <- p +
      geom_point(
        aes(x = x, y = y),
        size = point_size,
        color = point_color,
        alpha = 0.7
      )

    # Ajouter les labels des axes
    p <- p +
      annotate(
        "text",
        x = c(0, 1, 0.5),
        y = c(-0.08, -0.08, sqrt(3) / 2 + 0.08),
        label = c("Gravel (100%)", "Sand (100%)", "Mud (100%)"),
        size = 4.5,
        fontface = "bold"
      )

    # Ajouter les labels des échantillons si demandé
    if (show_labels) {
      p <- p +
        geom_text(
          aes(x = x, y = y, label = samples),
          size = 3,
          hjust = -0.2,
          vjust = -0.2
        )
    }

    # Afficher le graphique
    print(p)

    # Retourner les données de texture silencieusement
    invisible(tern_data)
  }
