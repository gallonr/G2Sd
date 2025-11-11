library(shiny)
library(G2Sd)
library(tidyverse)
library(bslib)
library(patchwork)
library(plotly)

# Function to create a plot without printing it
create_single_plot <- function(data, xc, hist = TRUE, cum = TRUE, main = "") {
  x <- data
  um <- as.numeric(row.names(x))
  meshmin <- 1

  # Replace 0 and negative values
  if (!is.na(pmatch(0, um))) {
    um[pmatch(0, um)] <- meshmin
  }
  um[um <= 0] <- meshmin

  sum.sieve <- sum(x[, xc])
  class.weight <- (x[, xc] * 100) / sum.sieve
  class.weight.cum <- round(cumsum(class.weight), 2)
  weight.gran <- data.frame(
    um = um,
    weight = class.weight,
    weight.cum = class.weight.cum
  )

  # Create base plot
  p <- ggplot(weight.gran) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_line(colour = "gray"),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11, colour = "black"),
      strip.text.x = element_text(size = rel(1.0), face = "bold"),
      axis.ticks = element_line(colour = "black"),
      axis.line = element_line(colour = "black")
    ) +
    labs(title = main)

  # Add layers according to options
  if (hist && cum) {
    p <- p +
      geom_bar(aes(x = um, y = weight), stat = "identity", fill = "darkgray") +
      geom_line(aes(x = um, y = weight.cum), colour = "red", linewidth = 1.5) +
      ylab("Weight (%)") +
      xlab(expression(log[10](Particule ~ size))) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . * 1, name = "Percentage cum.(%)")
      ) +
      suppressWarnings(scale_x_log10())
  } else if (hist) {
    p <- p +
      geom_bar(aes(x = um, y = weight), stat = "identity", fill = "darkgray") +
      ylab("Weight (%)") +
      xlab(expression(log[10](Particule ~ size))) +
      suppressWarnings(scale_x_log10())
  } else if (cum) {
    p <- p +
      geom_line(aes(x = um, y = weight.cum), colour = "red", linewidth = 1.5) +
      ylab("Percentage cum.(%)") +
      xlab(expression(log[10](Particule ~ size))) +
      suppressWarnings(scale_x_log10())
  }

  return(p)
}

# Grain size data validation function
grancompat <- function(x) {
  x <- as.data.frame(x)
  n.sieve <- nrow(x)
  n.sample <- ncol(x)

  if (!is.data.frame(x)) {
    stop("A data.frame is expected.", call. = FALSE)
  }
  if (any(x < 0, na.rm = TRUE)) {
    stop("Negative values detected in the data.", call. = FALSE)
  }
  if (any(x > 300, na.rm = TRUE)) {
    warning(
      "Some values are very high (>300).",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  if (any(apply(x, 2, sum, na.rm = TRUE) == 0)) {
    stop("Some columns have a zero sum.", call. = FALSE)
  }

  return(x)
}

shinyServer(function(input, output, session) {
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================

  # Loaded grain size data
  bdd_gran <- reactiveVal(NULL)

  # ============================================================================
  # DATA LOADING
  # ============================================================================

  # Load example data
  observeEvent(input$load_example, {
    withProgress(message = 'Loading example data...', value = 0, {
      data("granulo", package = "G2Sd", envir = environment())
      incProgress(0.5)

      # Prepare data (remove first size column)
      gran_data <- granulo[, -1]
      gran_data <- round(gran_data, 3)
      gran_data <- grancompat(gran_data)

      incProgress(0.5)
      bdd_gran(gran_data)

      # Update station limits
      updateNumericInput(
        session,
        "to",
        max = ncol(gran_data),
        value = min(3, ncol(gran_data))
      )
      updateNumericInput(session, "from", max = ncol(gran_data), value = 1)
    })

    showNotification(
      "Example data loaded successfully!",
      type = "message",
      duration = 3
    )
  })

  # Load user file
  observeEvent(input$file1, {
    req(input$file1)

    withProgress(message = 'Loading file...', value = 0, {
      tryCatch(
        {
          incProgress(0.3, detail = "Reading file...")

          gran_data <- read.table(
            input$file1$datapath,
            header = TRUE,
            sep = input$separator,
            dec = input$decimal,
            row.names = 1
          )

          incProgress(0.3, detail = "Validating data...")
          gran_data <- round(gran_data, 3)
          gran_data <- grancompat(gran_data)

          incProgress(0.4, detail = "Finalizing...")
          bdd_gran(gran_data)

          # Update station limits
          updateNumericInput(
            session,
            "to",
            max = ncol(gran_data),
            value = min(3, ncol(gran_data))
          )
          updateNumericInput(session, "from", max = ncol(gran_data), value = 1)

          showNotification(
            paste("File loaded:", ncol(gran_data), "samples detected"),
            type = "message",
            duration = 3
          )
        },
        error = function(e) {
          showNotification(
            paste("Loading error:", e$message),
            type = "error",
            duration = 5
          )
          bdd_gran(NULL)
        }
      )
    })
  })

  # ============================================================================
  # DATA STATUS
  # ============================================================================

  output$data_status <- renderUI({
    if (is.null(bdd_gran())) {
      div(
        class = "alert alert-info",
        icon("circle-info"),
        " No data loaded. Use an example file or import your data."
      )
    } else {
      div(
        class = "alert alert-success",
        icon("circle-check"),
        sprintf(
          " %d samples and %d sieves detected",
          ncol(bdd_gran()),
          nrow(bdd_gran())
        )
      )
    }
  })

  # Information about station range
  output$station_range_info <- renderUI({
    req(bdd_gran())
    n_samples <- ncol(bdd_gran())

    # Validation
    from_val <- input$from
    to_val <- input$to

    if (from_val > to_val) {
      div(
        class = "alert alert-warning",
        style = "padding: 5px; font-size: 0.85em;",
        icon("triangle-exclamation"),
        " 'From' must be ≤ 'To'"
      )
    } else if (to_val > n_samples) {
      div(
        class = "alert alert-warning",
        style = "padding: 5px; font-size: 0.85em;",
        icon("triangle-exclamation"),
        sprintf(" Maximum: %d samples", n_samples)
      )
    } else {
      div(
        class = "alert alert-info",
        style = "padding: 5px; font-size: 0.85em;",
        icon("info-circle"),
        sprintf(" %d sample(s) selected", to_val - from_val + 1)
      )
    }
  })

  # ============================================================================
  # DATA SUMMARY
  # ============================================================================

  output$data_summary <- renderUI({
    req(bdd_gran())

    gran <- bdd_gran()

    div(
      fluidRow(
        column(
          6,
          card(
            card_body(
              h4(ncol(gran), style = "color: #3498db;"),
              p("Samples")
            )
          )
        ),
        column(
          6,
          card(
            card_body(
              h4(nrow(gran), style = "color: #3498db;"),
              p("Sieve sizes")
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          card(
            card_body(
              h5("Size range"),
              p(sprintf(
                "%.2f - %.2f µm",
                min(as.numeric(rownames(gran)), na.rm = TRUE),
                max(as.numeric(rownames(gran)), na.rm = TRUE)
              ))
            )
          )
        ),
        column(
          6,
          card(
            card_body(
              h5("Samples"),
              p(paste(head(colnames(gran), 3), collapse = ", "), "...")
            )
          )
        )
      )
    )
  })

  # ============================================================================
  # DATA PREVIEW
  # ============================================================================

  output$contents <- renderTable(
    {
      req(bdd_gran())

      # Display first 10 rows with row names
      head(bdd_gran(), 10)
    },
    rownames = TRUE,
    digits = 2
  )

  # ============================================================================
  # STATISTICAL CALCULATIONS (with cache)
  # ============================================================================

  result_stats <- reactive({
    req(bdd_gran())

    withProgress(message = 'Calculating statistics...', value = 0, {
      incProgress(0.5)
      result <- granstat(bdd_gran(), statistic = input$method)
      incProgress(0.5)
      result
    })
  })

  # ============================================================================
  # STATISTICS DISPLAY
  # ============================================================================

  # Create separate outputs for each table when "All" is selected
  output$stat_statistics <- renderTable(
    {
      req(result_stats(), input$gran == "All")
      result_stats()$stat
    },
    rownames = TRUE,
    digits = 3
  )

  output$stat_indices <- renderTable(
    {
      req(result_stats(), input$gran == "All")
      result_stats()$index
    },
    rownames = TRUE,
    digits = 3
  )

  output$stat_texture <- renderTable(
    {
      req(result_stats(), input$gran == "All")
      result_stats()$sedim$texture
    },
    rownames = TRUE,
    digits = 3
  )

  output$stat_sediment <- renderTable(
    {
      req(result_stats(), input$gran == "All")
      result_stats()$sedim$descript
    },
    rownames = TRUE,
    digits = 3
  )

  output$stat_output <- renderUI({
    req(result_stats())

    result <- result_stats()

    if (input$gran == "All") {
      # Display all tables with proper rendering
      tagList(
        card(
          card_header("Statistics"),
          div(
            style = "overflow-x: auto;",
            tableOutput("stat_statistics")
          )
        ),
        card(
          card_header("Indices"),
          div(
            style = "overflow-x: auto;",
            tableOutput("stat_indices")
          )
        ),
        card(
          card_header("Texture"),
          div(
            style = "overflow-x: auto;",
            tableOutput("stat_texture")
          )
        ),
        card(
          card_header("Sediment"),
          div(
            style = "overflow-x: auto;",
            tableOutput("stat_sediment")
          )
        )
      )
    } else {
      # Display single table
      data_to_show <- switch(
        input$gran,
        "Statistics" = result$stat,
        "Index" = result$index,
        "Texture" = result$sedim$texture,
        "Sedim" = result$sedim$descript
      )
      renderTable(data_to_show, rownames = TRUE, digits = 3)
    }
  })

  # ============================================================================
  # STATISTICS DOWNLOAD
  # ============================================================================

  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$gran == "All") {
        paste0("G2Sd_results_", Sys.Date(), ".zip")
      } else {
        paste0("G2Sd_", input$gran, "_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      req(result_stats())
      result <- result_stats()

      if (input$gran == "All") {
        # Create a ZIP file with all results
        withProgress(message = 'Creating ZIP file...', value = 0, {
          tmpdir <- tempdir()
          setwd(tmpdir)

          files <- c()

          incProgress(0.2, detail = "Statistics...")
          write.csv2(result$stat, "Statistics.csv", row.names = TRUE)
          files <- c(files, "Statistics.csv")

          incProgress(0.2, detail = "Indices...")
          write.csv2(result$index, "Index.csv", row.names = TRUE)
          files <- c(files, "Index.csv")

          incProgress(0.2, detail = "Texture...")
          write.csv2(result$sedim$texture, "Texture.csv", row.names = TRUE)
          files <- c(files, "Texture.csv")

          incProgress(0.2, detail = "Sediment...")
          write.csv2(result$sedim$descript, "Sedim.csv", row.names = TRUE)
          files <- c(files, "Sedim.csv")

          incProgress(0.2, detail = "Compressing...")
          zip(zipfile = file, files = files)
        })
      } else {
        # Save a single file
        data_to_save <- switch(
          input$gran,
          "Statistics" = result$stat,
          "Index" = result$index,
          "Texture" = result$sedim$texture,
          "Sedim" = result$sedim$descript
        )
        write.csv2(data_to_save, file, row.names = TRUE)
      }
    },
    contentType = if (input$gran == "All") "application/zip" else "text/csv"
  )

  # ============================================================================
  # PLOTS - HISTOGRAMS
  # ============================================================================

  output$plot1_container <- renderUI({
    req(bdd_gran())

    # Validate inputs
    from_val <- max(1, min(input$from, ncol(bdd_gran())))
    to_val <- max(1, min(input$to, ncol(bdd_gran())))

    if (from_val > to_val) {
      div(
        class = "alert alert-warning",
        icon("triangle-exclamation"),
        " Please select a valid range (From ≤ To)"
      )
    } else {
      n_plots <- to_val - from_val + 1

      # Dynamic height: if individual histograms, multiply by number
      if (input$hist || n_plots == 1) {
        height <- n_plots * 500 # 500px per plot
      } else {
        # Single comparison plot
        height <- 600
      }

      plotOutput("plot1", width = "100%", height = paste0(height, "px"))
    }
  })

  output$plot1 <- renderPlot({
    req(bdd_gran())

    from_val <- max(1, min(input$from, ncol(bdd_gran())))
    to_val <- max(1, min(input$to, ncol(bdd_gran())))

    if (from_val <= to_val) {
      n_plots <- to_val - from_val + 1

      # If histogram checked OR single sample: individual plots stacked vertically
      if (input$hist || n_plots == 1) {
        # Create a list of plots
        plot_list <- list()

        for (i in from_val:to_val) {
          p <- create_single_plot(
            bdd_gran(),
            xc = i,
            hist = input$hist,
            cum = input$cum,
            main = names(bdd_gran())[i]
          )
          plot_list[[length(plot_list) + 1]] <- p
        }

        # Combine plots vertically with patchwork
        combined_plot <- wrap_plots(plot_list, ncol = 1)
        print(combined_plot)
      } else {
        # If only cumulative curves: single plot with all curves
        granplot(
          bdd_gran(),
          xc = from_val:to_val,
          hist = FALSE,
          cum = TRUE,
          cexname = 1.2
        )
      }
    }
  })

  # Download histograms
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      paste0("G2Sd_histograms_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(bdd_gran())

      from_val <- max(1, min(input$from, ncol(bdd_gran())))
      to_val <- max(1, min(input$to, ncol(bdd_gran())))

      n_plots <- to_val - from_val + 1

      # If histogram checked OR single sample: individual plots stacked vertically
      if (input$hist || n_plots == 1) {
        height <- n_plots * 500 # 500px per plot

        # Create a list of plots
        plot_list <- list()

        for (i in from_val:to_val) {
          p <- create_single_plot(
            bdd_gran(),
            xc = i,
            hist = input$hist,
            cum = input$cum,
            main = names(bdd_gran())[i]
          )
          plot_list[[length(plot_list) + 1]] <- p
        }

        # Combine and save
        combined_plot <- wrap_plots(plot_list, ncol = 1)
        ggsave(file, combined_plot, width = 10, height = n_plots * 5, dpi = 100)
      } else {
        # If only cumulative curves: single plot
        png(file, width = 1000, height = 600, res = 100)

        granplot(
          bdd_gran(),
          xc = from_val:to_val,
          hist = FALSE,
          cum = TRUE,
          cexname = 1.2
        )

        dev.off()
      }
    }
  )

  # ============================================================================
  # PLOTS - DISTRIBUTION
  # ============================================================================

  output$plot2 <- renderPlotly({
    req(bdd_gran())

    # Prepare data according to chosen scale
    if (input$distritype == "fine") {
      Descript <- .sedim.descript(bdd_gran())
      Descript <- Descript |>
        pivot_longer(cols = -samples, names_to = "class", values_to = "value")
      Descript$class <- factor(
        Descript$class,
        c(
          "boulder",
          "vcgravel",
          "cgravel",
          "mgravel",
          "fgravel",
          "vfgravel",
          "vcsand",
          "csand",
          "msand",
          "fsand",
          "vfsand",
          "vcsilt",
          "silt"
        ),
        ordered = TRUE
      )
    } else {
      Descript <- .texture.sedim(bdd_gran())
      Descript <- Descript |>
        pivot_longer(
          cols = -c(samples, texture),
          names_to = "class",
          values_to = "value"
        )
      Descript$class <- factor(
        Descript$class,
        c("Boulder", "Gravel", "Sand", "Mud"),
        ordered = TRUE
      )
    }

    # Create plot directly with plotly
    plot_ly(
      data = Descript,
      x = ~samples,
      y = ~value,
      color = ~class,
      type = "bar",
      text = ~ paste0(
        "<b>Station:</b> ",
        samples,
        "<br>",
        "<b>Class:</b> ",
        class,
        "<br>",
        "<b>Percentage:</b> ",
        round(value, 2),
        "%"
      ),
      hoverinfo = "text",
      textposition = "none",
      colors = "Spectral"
    ) |>
      plotly::layout(
        title = "Grain size distribution",
        xaxis = list(title = "Stations"),
        yaxis = list(title = "Percentage (%)"),
        barmode = "stack",
        hovermode = "closest",
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        legend = list(title = list(text = "Classes")),
        showlegend = TRUE
      )
  })

  # Download distribution
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste0("G2Sd_distribution_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(bdd_gran())

      # Prepare data according to chosen scale
      if (input$distritype == "fine") {
        Descript <- .sedim.descript(bdd_gran())
        Descript <- Descript |>
          pivot_longer(cols = -samples, names_to = "class", values_to = "value")
        Descript$class <- factor(
          Descript$class,
          c(
            "boulder",
            "vcgravel",
            "cgravel",
            "mgravel",
            "fgravel",
            "vfgravel",
            "vcsand",
            "csand",
            "msand",
            "fsand",
            "vfsand",
            "vcsilt",
            "silt"
          ),
          ordered = TRUE
        )
      } else {
        Descript <- .texture.sedim(bdd_gran())
        Descript <- Descript |>
          pivot_longer(
            cols = -c(samples, texture),
            names_to = "class",
            values_to = "value"
          )
        Descript$class <- factor(
          Descript$class,
          c("Boulder", "Gravel", "Sand", "Mud"),
          ordered = TRUE
        )
      }

      # Create plot
      p <- ggplot(Descript, aes(x = samples, y = value, fill = class)) +
        geom_bar(stat = "identity", position = "stack") +
        theme_bw() +
        scale_fill_viridis_d() +
        xlab("Stations") +
        ylab("Percentage") +
        ggtitle("Grain size distribution") +
        guides(fill = guide_legend(title = "Classes")) +
        theme(
          axis.title = element_text(face = "bold", size = 13),
          axis.text = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(face = "bold", size = 11)
        )

      # Sauvegarder comme fichier PNG
      ggsave(file, p, width = 10, height = 6, dpi = 100)
    }
  )
})
