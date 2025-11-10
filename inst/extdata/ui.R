library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)

# Define UI for G2Sd application
shinyUI(page_sidebar(
  title = tagList(
    tags$span(
      style = "display: flex; align-items: center; width: 100%;",
      tags$img(
        src = "G2SdLogo.png",
        height = "70px",
        alt = "G2Sd Logo",
        style = "margin-right: 10px;"
      ),
      tags$span(
        "Grain-size Statistics and Description of Sediment",
        style = "font-size: 1.5em; font-weight: bold;"
      ),
      tags$a(
        icon("github"),
        href = "https://github.com/gallonr/G2Sd",
        target = "_blank",
        style = "color: #333; margin-left: auto; font-size: 1.2em;",
        title = "View on GitHub"
      )
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",
    # primary = "#DB8846"
    # base_font retiré pour éviter le téléchargement de Google Fonts
  ),

  # Sidebar avec les contrôles
  sidebar = sidebar(
    width = 350,

    # Section 1: Chargement des données
    accordion(
      id = "acc_data",
      open = "panel_data",
      accordion_panel(
        title = tagList(icon("database"), "Data"),
        value = "panel_data",

        # Bouton pour charger l'exemple
        actionButton(
          "load_example",
          "Load example data",
          icon = icon("file-import"),
          class = "btn-primary btn-sm w-100",
          style = "margin-bottom: 10px;"
        ),

        p(strong("OR"), style = "text-align: center; margin: 10px 0;"),

        # Upload de fichier
        fileInput(
          'file1',
          'Import a file',
          accept = c('.csv', '.txt', '.CSV', '.TXT'),
          buttonLabel = "Browse...",
          placeholder = "No file"
        ),

        # Paramètres de lecture
        fluidRow(
          column(6, textInput("separator", "Separator:", value = ";")),
          column(6, textInput("decimal", "Decimal:", value = ","))
        ),

        # Statut du chargement
        uiOutput("data_status"),

        helpText(
          icon("info-circle"),
          "Accepted formats: CSV, TXT. ",
          "The file must contain sieve sizes in rows and samples in columns."
        )
      ),

      # Section 2: Méthode statistique
      accordion_panel(
        title = tagList(icon("calculator"), "Statistics"),
        value = "panel_stats",

        selectInput(
          'method',
          'Calculation method:',
          choices = c(
            "Arithmetic" = "arithmetic",
            "Geometric" = "geometric",
            "Folk & Ward" = "folk.ward"
          ),
          selected = "arithmetic"
        ),

        selectInput(
          'gran',
          'Results to display:',
          choices = c(
            'Statistics' = 'Statistics',
            'Indices' = 'Index',
            'Texture' = 'Texture',
            'Sediment' = 'Sedim',
            'All' = 'All'
          ),
          selected = 'Statistics'
        ),

        downloadButton(
          'downloadData',
          'Download results',
          icon = icon("download"),
          class = "btn-primary btn-sm w-100"
        )
      ),

      # Section 3: Graphiques
      accordion_panel(
        title = tagList(icon("chart-bar"), "Plots"),
        value = "panel_plot",

        h6(strong("Histograms")),

        fluidRow(
          column(6, numericInput('from', 'Station from:', value = 1, min = 1)),
          column(6, numericInput('to', 'Station to:', value = 1, min = 1))
        ),

        uiOutput("station_range_info"),

        checkboxInput('hist', 'Show histogram', TRUE),
        checkboxInput('cum', 'Show cumulative curve', TRUE),

        downloadButton(
          'downloadPlot1',
          'Download histograms',
          icon = icon("download"),
          class = "btn-sm btn-outline-primary w-100",
          style = "margin-top: 10px;"
        ),

        hr(),

        h6(strong("Distribution")),

        radioButtons(
          'distritype',
          'Distribution type:',
          choices = c('Fine' = 'fine', 'Coarse' = 'large'),
          selected = 'fine'
        ),

        downloadButton(
          'downloadPlot2',
          'Download distribution',
          icon = icon("download"),
          class = "btn-sm btn-outline-primary w-100"
        )
      )
    ),

    hr(),

    # Footer
    div(
      style = "text-align: center; font-size: 0.85em; margin-top: 10px;",
      div(
        style = "margin-top: 15px;",
        a(
          href = "https://www.intechmer.cnam.fr/intechmer/",
          target = "_blank",
          tags$img(
            src = "cnam_logo.png",
            height = "100px",
            alt = "CNAM Intechmer"
          )
        )
      ),
      div(style = "height: 12px;"),
      p(
        "Contact:",
        a(
          "regis.gallon@lecnam.net",
          href = "mailto:regis.gallon@lecnam.net?subject=[G2Sd]%20Information"
        )
      ),
      p(
        "© 2025 Régis Gallon - CNAM Intechmer"
      )
    )
  ),

  # Main panel avec les onglets
  navset_card_tab(
    id = "main_tabs",

    # Onglet Aperçu
    nav_panel(
      title = tagList(icon("eye"), "Overview"),
      value = "tab_overview",

      card(
        card_header("Data summary"),
        uiOutput("data_summary")
      ),

      card(
        card_header("Table preview"),
        div(
          style = "overflow-x: auto;",
          tableOutput('contents')
        )
      )
    ),

    # Onglet Statistiques
    nav_panel(
      title = tagList(icon("table"), "Statistics"),
      value = "tab_stats",

      card(
        card_header(
          "Statistical results",
          popover(
            icon("circle-info"),
            "Statistics are calculated according to the method selected in the sidebar."
          )
        ),
        div(
          style = "overflow-x: auto;",
          uiOutput('stat_output')
        )
      )
    ),

    # Onglet Histogrammes
    nav_panel(
      title = tagList(icon("chart-column"), "Histograms"),
      value = "tab_hist",

      card(
        card_header("Histograms and cumulative curves"),
        uiOutput("plot1_container")
      )
    ),

    # Onglet Distribution
    nav_panel(
      title = tagList(icon("chart-line"), "Distribution"),
      value = "tab_distrib",

      card(
        card_header("Distribution diagram (interactive)"),
        plotlyOutput("plot2", width = "100%", height = "600px")
      )
    ),

    # Onglet Aide
    nav_panel(
      title = tagList(icon("circle-question"), "Help"),
      value = "tab_help",

      card(
        card_header("User Guide"),

        h4("1. Data Loading"),
        p("You can either:"),
        tags$ul(
          tags$li(
            "Load example data by clicking on ",
            strong("'Load example data'")
          ),
          tags$li("Import your own CSV or TXT file")
        ),

        h5("File Format"),
        p("The file must be structured with:"),
        tags$ul(
          tags$li(
            strong("First column:"),
            " sieve sizes (in µm or mm)"
          ),
          tags$li(
            strong("Following columns:"),
            " passing percentages for each sample"
          ),
          tags$li(strong("First row:"), " sample names")
        ),

        card(
          card_header("Format example"),
          tags$pre(
            "Size;Sample1;Sample2;Sample3\n",
            "2000;0.1;0.2;0.0\n",
            "1000;2.5;3.1;1.8\n",
            "500;15.2;18.5;12.3\n",
            "..."
          )
        ),

        hr(),

        h4("2. Statistical Methods"),

        h5("Arithmetic"),
        p(
          "Calculations based on arithmetic means. Recommended for symmetrical distributions."
        ),

        h5("Geometric"),
        p(
          "Calculations based on geometric means. Recommended for log-normal distributions."
        ),

        h5("Folk & Ward"),
        p(
          "Standard graphical method in sedimentology. Uses percentiles to calculate parameters."
        ),

        hr(),

        h4("3. Available Results"),
        tags$ul(
          tags$li(
            strong("Statistics:"),
            " mean, median, mode, standard deviation"
          ),
          tags$li(strong("Indices:"), " skewness, kurtosis"),
          tags$li(strong("Texture:"), " percentages of sand, silt, clay"),
          tags$li(strong("Sediment:"), " sedimentological classification")
        ),

        hr(),

        h4("4. Plots"),
        p(
          "Histograms show the size distribution for each sample."
        ),
        p(
          "The distribution diagram compares all samples on a single plot."
        ),

        hr(),

        h4("5. Results Export"),
        p(
          "All tables and plots can be downloaded using the dedicated buttons."
        ),

        hr(),

        card(
          card_header(icon("book"), " References"),
          p(
            "For more information about the G2Sd package:",
            tags$ul(
              tags$li(a(
                "CRAN Documentation",
                href = "https://CRAN.R-project.org/package=G2Sd",
                target = "_blank"
              )),
              tags$li(a(
                "GitHub Repository",
                href = "https://github.com/gallonr/G2Sd",
                target = "_blank"
              )),
              tags$li(a(
                "Author's Blog",
                href = "http://regisgallon.wordpress.com/",
                target = "_blank"
              ))
            )
          )
        )
      )
    )
  )
))
