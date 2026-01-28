library(shiny)
library(tidyverse)
library(httr2)
library(ggrepel)
library(officer)
library(flextable)
library(config)

source("functions.R")
source("text.R")

# UI
ui <- fluidPage(
  titlePanel("Stats for MERV Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # textInput(
      #   "api_key",
      #   "HDR API Key:",
      #   value = api_key,
      #   placeholder = "Enter your API key"
      # ),
      p("Select countries and year and click on 'Fetch data' at the bottom"),

      selectInput(
        "main_country",
        "Main Country:",
        choices = country_list,
        selected = "AFG"
      ),

      checkboxGroupInput(
        "comparison_countries",
        "Comparison Countries:",
        choices = country_list,
        selected = c("PAK", "IRN")
      ),

      checkboxGroupInput(
        "years",
        "Years:",
        choices = 2018:2025,
        selected = 2018:2025,
        inline = TRUE
      ),

      actionButton(
        "fetch_data",
        "1. Fetch Data",
        class = "btn-primary",
        style = "width: 100%; margin-top: 10px;"
      )
    ),

    mainPanel(
      width = 9,

      hr(),

      h2("A) Political System"),

      hr(),

      h3("Domestic Political Stability"),

      hr(),

      plotOutput("delib_plot", height = "300px"),

      hr(),

      h3("Voices & accountability, civil society, and media"),

      hr(),

      uiOutput("civic_summary"),

      hr(),

      plotOutput("civic_plot", height = "300px"),

      hr(),

      h2("B) Development baselines"),

      hr(),

      h3("2) Human capital, poverty and inequalities"),

      uiOutput("hdi_summary"),

      plotOutput("hdi_plot", height = "300px"),

      hr(),



      # h3("Situation Analysis"),
      # p("- Guiding questions"),
      #
      # h3("Consequences for the operation"),
      #
      # hr(),

      downloadButton(
        "download_report",
        "2. Download Stats to prepare the MERV",
        class = "btn-success",
        style = "width: 100%; margin-top: 20px;"
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive value to store data
  civic_data <- reactiveVal(NULL)
  delib_data <- reactiveVal(NULL)
  hdi_data <- reactiveVal(NULL)

  # Fetch data when button is clicked
  observeEvent(input$fetch_data, {
    req(input$main_country, input$years, hdr_api_key)

    countries <- c(input$main_country, input$comparison_countries)
    years <- as.numeric(input$years)

    # Fetch deliberative democracy data
    showNotification("Fetching deliberative democracy index data...", type = "message", duration = NULL, id = "fetch_delib")

    delib_df <- get_wb_deliberative_data(countries, years)

    if (!is.null(delib_df) && nrow(delib_df) > 0) {
      delib_data(delib_df)
      removeNotification(id = "fetch_delib")
      showNotification("Deliberative democracy index data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_delib")
      showNotification("Failed to fetch deliberative democracy index data.", type = "warning", duration = 5)
    }

    # Fetch civic space data
    showNotification("Fetching civic space data...", type = "message", duration = NULL, id = "fetch_civic")

    civic_df <- get_wb_civic_data(countries)

    if (!is.null(civic_df) && nrow(civic_df) > 0) {
      civic_data(civic_df)
      removeNotification(id = "fetch_civic")
      showNotification("Civic space data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_civic")
      showNotification("Failed to fetch civic space data.", type = "warning", duration = 5)
    }

    # Show notification
    showNotification("Fetching data on HDI...", type = "message", duration = NULL, id = "fetch")

    # Fetch HDI data
    data <- get_multiple_hdr(
      countries = countries,
      years = years,
      indicators = "hdi",
      api_key = hdr_api_key
    )

    # Process data
    if (length(data) > 0) {
      df <- bind_rows(data) |>
        mutate(year = as.numeric(year)) |>
        mutate(value = as.numeric(value)) |>
        separate(col = country, into = c("code", "country"), sep = " - ", extra = "merge")

      hdi_data(df)
      removeNotification(id = "fetch")
      showNotification("Data fetched successfully!", type = "message", duration = 3)
    } else {
      removeNotification(id = "fetch")
      showNotification("Failed to fetch data. Check API key and selections.", type = "error", duration = 5)
    }

    removeNotification(id = "fetch")
  })


  # Reactive parts
  civic_classification <- reactive({
    req(civic_data())

    civic_data() |>
      mutate(classification = classify_civic(value)) |>
      filter(code == input$main_country) |>
      filter(year == max(civic_data()$year))
  })


  hdi_classification <- reactive({
    req(hdi_data())

    hdi_data() |>
      mutate(classification = classify_hdi(value)) |>
      filter(code == input$main_country) |>
      filter(year == max(hdi_data()$year))
  })


  get_country_name <- reactive({
    names(country_list)[country_list == input$main_country]
  })


  # Delib. Democracy Plot
  output$delib_plot <- renderPlot({
    req(delib_data())

    df_delib <- delib_data() |>
      left_join(country_tibble, by = "code")

    nyears <- length(unique(df_delib$year))
    main_country <- input$main_country

    # country <- get_country_name()

    draw_plot(df_delib, main_country, nyears, "Deliberative Democracy Index for the selected countries", "V-DEM")

  })


  # Civic Summary text
  output$civic_summary <- renderUI({
    req(civic_classification())

    civic <- civic_classification()

    if (nrow(civic) > 0) {
      HTML(sprintf(
        "The Civic Space classification for <strong>%s</strong> is, as of <strong>%s</strong>, estimated at <strong>%s</strong>, which is classified as <strong>%s</strong>.",
        get_country_name(),
        civic$year[1],
        round(civic$value[1], 3),
        civic$classification[1]
      ))
    }
  })

  # Civic Plot
  output$civic_plot <- renderPlot({
    req(civic_data())

    df <- civic_data() |>
      left_join(country_tibble, by = "code")

    nyears <- length(unique(df$year))
    main_country <- input$main_country

    # country <- get_country_name()

    draw_plot(df, main_country, nyears, "Civic Space index for the selected countries", "CIVICUS")

  })

  # HDI Summary text
  output$hdi_summary <- renderUI({
    req(hdi_classification())

    hdi <- hdi_classification()

    if (nrow(hdi) > 0) {
      HTML(sprintf(
        "The HDI for <strong>%s</strong> is, as of <strong>%s</strong>, estimated at <strong>%s</strong>, which is classified as <strong>%s</strong>.",
        hdi$country[1],
        hdi$year[1],
        round(hdi$value[1], 3),
        hdi$classification[1]
      ))
    }
  })

  # HDI Plot
  output$hdi_plot <- renderPlot({
    req(hdi_data())

    df <- hdi_data()
    nyears <- length(unique(df$year))
    main_country <- input$main_country

    draw_plot(df, main_country, nyears, "Human Development Index (HDI) for the selected countries", "UNDP")

  })

  # Download handler for Word document
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("ShinyMERV_Report_", Sys.Date(), ".docx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      req(hdi_data())
      df <- hdi_data()
      hdi <- hdi_classification()

      req(civic_data())
      df_civ <- civic_data() |>
        left_join(country_tibble, by = "code")
      #civ <- civic_classification()

      req(delib_data())
      df_delib <- delib_data() |>
        left_join(country_tibble, by = "code")

      # Create Word document
      doc <- read_docx(path = "Shiny_Merv_Markdown_Template.docx")

      # Main Title
      doc <- doc |>
        body_add_par("IC Programme Context Monitoring", style = "Title") |>
        body_add_par(input$main_country, style = "Title") |>
        body_add_par(paste("Date:", Sys.Date()), style = "Date") |>
        body_add_par("", style = "Normal")

      # Intro
      doc <- doc |>
        body_add_par("OVERVIEW", style = "Subtitle") |>
        body_add_par("", style = "Normal")

      # Analyses
      doc <- doc |>
        body_add_par("ANALYSES", style = "Subtitle") |>
        body_add_par("", style = "Normal")

      # A) Political System section
      doc <- doc |>
        body_add_par("Political System", style = "heading 1") |>
        body_add_par("", style = "Normal")

      # 1) International political context
      doc <- doc |>
        body_add_par("1) International political context", style = "heading 2")


      # International political context Analysis and Consequences
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_par("Situation Analysis", style = "heading 3") |>
        body_add_par(analysis_pol_1, style = "List Bullet") |>
        body_add_par(analysis_pol_2, style = "List Bullet") |>
        body_add_par("", style = "Normal") |>
        body_add_par("Consequences for the operation", style = "heading 3") |>
        body_add_par("", style = "Normal")

      # 2) Domestic political stability
      doc <- doc |>
        body_add_par("2) Domestic political stability", style = "heading 2")

      # Create and save the civic plot
      temp_plot_delib <- tempfile(fileext = ".png")

      nyears <- length(unique(df_delib$year))
      main_country <- input$main_country

      ggsave(temp_plot_delib, plot = draw_plot(df_delib, main_country, nyears, "Deliberative Democracy Index for the selected countries", "V-DEM"), width = 6, height = 2.5, dpi = 200)

      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_delib, width = 6, height = 2.5)

      # Domestic political stability Analysis and Consequences
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_par("Situation Analysis", style = "heading 3") |>
        body_add_par(analysis_delib_1, style = "List Bullet") |>
        body_add_par(analysis_delib_2, style = "List Bullet") |>
        body_add_par(analysis_delib_3, style = "List Bullet") |>
        body_add_par("", style = "Normal") |>
        body_add_par("Consequences for the operation", style = "heading 3") |>
        body_add_par("", style = "Normal")

      # A.3) Voice & accountability
      if (!is.null(civic_data())) {
        civic <- civic_classification()

        if (nrow(civic) > 0) {
          civic_text <- sprintf(
            "The Civic Space Index for %s is, as of %s, estimated at %s, which is classified as %s.",
            get_country_name(),
            civic$year[1],
            round(civic$value[1], 1),
            civic$classification[1]
          )

          doc <- doc |>
            body_add_par("3) Voice & accountability, civil society, and media", style = "heading 2") |>
            body_add_par(civic_text, style = "Normal")
        }
      }

      # Create and save the civic plot
      temp_plot_civic <- tempfile(fileext = ".png")

      nyears <- length(unique(df_civ$year))
      main_country <- input$main_country

      ggsave(temp_plot_civic, plot = draw_plot(df_civ, main_country, nyears, "Civic Space index for the selected countries", "CIVICUS"), width = 6, height = 2.5, dpi = 200)

      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_civic, width = 6, height = 2.5)

      # Civic Situation Analysis and Consequences
      doc <- doc |>
       # body_add_par("", style = "Normal") |>
        body_add_par("Situation Analysis", style = "heading 3") |>
        body_add_par(analysis_civic_1, style = "List Bullet") |>
        body_add_par(analysis_civic_2, style = "List Bullet") |>
        body_add_par(analysis_civic_3, style = "List Bullet") |>
        body_add_par("", style = "Normal") |>
        body_add_par("Consequences for the operation", style = "heading 3") |>
        body_add_par("", style = "Normal")

      # B) Development baslines
      doc <- doc |>
        body_add_par("Development baselines", style = "heading 1") |>
        body_add_par("(in IC programme sectors)", style = "Normal") |>
        body_add_par("", style = "Normal")

      # B.2 Human capital
      doc <- doc |>
        body_add_par("2) Human capital, poverty and inequalities", style = "heading 2")

      # Add HDI summary text
      if (nrow(hdi) > 0) {
        summary_text <- sprintf(
          "The HDI for %s is, as of %s, estimated at %s, which is classified as %s.",
          hdi$country[1],
          hdi$year[1],
          round(hdi$value[1], 3),
          hdi$classification[1]
        )
        doc <- doc |> body_add_par(summary_text, style = "Normal")
      }

      # Create and save the plot
      temp_plot_hdi <- tempfile(fileext = ".png")

      nyears <- length(unique(df$year))
      main_country <- input$main_country

      ggsave(temp_plot_hdi, plot = draw_plot(df, main_country, nyears, "Human Development Index (HDI) for the selected countries", "UNDP"), width = 6, height = 2.5, dpi = 200)

      # Add plot to document
      doc <- doc |>
        body_add_par("", style = "Normal") |>
        body_add_img(src = temp_plot_hdi, width = 6, height = 2.5)

     # HDI Situation Analysis and Consequences
      doc <- doc |>
        body_add_par("", style = "Normal") |>
        body_add_par("Situation Analysis", style = "heading 3") |>
        body_add_par(analysis_hdi_1, style = "List Bullet") |>
        body_add_par(analysis_hdi_2, style = "List Bullet") |>
        body_add_par(analysis_hdi_3, style = "List Bullet") |>
        body_add_par("", style = "Normal") |>
        body_add_par("Consequences for the operation", style = "heading 3") |>
        body_add_par("", style = "Normal")


      # Save document
      print(doc, target = file)

      # Clean up temp file
      unlink(temp_plot_hdi)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
