library(shiny)
library(tidyverse)
library(httr2)
library(ggrepel)
library(officer)
library(flextable)
library(config)

source("functions.R")
source("text.R")
source("comments.R")

# UI
ui <- fluidPage(
    titlePanel("International Cooperation (IC) Context Monitoring"),
  
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
        selected = "LAO"
      ),

      checkboxGroupInput(
        "comparison_countries",
        "Comparison Countries:",
        choices = country_list,
        selected = c("KHM", "THA")
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

      h3("A) Political System"),

      hr(),

      h4("2) Domestic Political Stability"),

      hr(),

      plotOutput("delib_plot", height = "300px"),

      hr(),

      h4("3) Civic and political rights, voice and media"),

      # hr(),

      # uiOutput("civic_summary"),
      # 
      # hr(),

      plotOutput("civil_lib_plot", height = "300px"),

      hr(),
      
      h4("4) Rule of law, independence of justice, division of power"),
      
      hr(),
      
      plotOutput("judic_plot", height = "300px"),
      
      hr(),

      h3("B) Development baselines"),

      hr(),
      
      h4("1) GDP Growth"),
      
      plotOutput("gdp_growth_plot", height = "300px"),
      
      hr(),

      h4("2) Human capital, poverty and inequalities"),

      uiOutput("hdi_summary"),

      plotOutput("hdi_plot", height = "300px"),

      hr(),
      
      h4("3) Climate & environment risks"),
      
      plotOutput("climate_change_plot", height = "300px"),
      
      hr(),
      
      h3("C) Domestic partner context"),
      
      hr(),
      
      h4("1) Operational space"),
      
      plotOutput("risk_index_plot", height = "300px"),
      
      hr(),
      
      h4("2) Government effectiveness and control of corruption"),
      
      plotOutput("cpi_plot", height = "300px"),
      
      # plotOutput("gov_effectiveness_plot", height = "300px"),
      # plotOutput("ctrl_corruption_plot", height = "300px"),
      
      hr(),
      
      h4("3) ODA as percent of recipient GNI"),
      
      plotOutput("oda_gni_plot", height = "300px"),
      
      hr(),
      
      h4("4) Non-state actors and private sector"),
      
      plotOutput("ccsi_plot", height = "300px"),
      
      plotOutput("bready_resolution_plot", height = "300px"),
      
      hr(),

      downloadButton(
        "download_report",
        "2. Download the IC Context Monitoring Word template",
        class = "btn-success",
        style = "width: 100%; margin-top: 20px;"
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive value to store data
  civil_lib_data <- reactiveVal(NULL)
  judic_data <- reactiveVal(NULL)
  delib_data <- reactiveVal(NULL)
  gdp_growth_data <- reactiveVal(NULL)
  gni_pc_data <- reactiveVal(NULL)
  hdi_data <- reactiveVal(NULL)
  climate_change_data <- reactiveVal(NULL)
  risk_index_data <- reactiveVal(NULL)
  cpi_data <- reactiveVal(NULL)
  # gov_effectiveness_data <- reactiveVal(NULL)
  # ctrl_corruption_data <- reactiveVal(NULL)
  oda_gni_data <- reactiveVal(NULL)
  ccsi_data <- reactiveVal(NULL)
  bready_resolution_data <- reactiveVal(NULL)
  

  # Fetch data when button is clicked
  observeEvent(input$fetch_data, {
    req(input$main_country, input$years, hdr_api_key)

    countries <- c(input$main_country, input$comparison_countries)
    years <- as.numeric(input$years)

    # A_2_Fetch deliberative democracy data
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

    # A_3_Fetch civil liberties data
    showNotification("Fetching civil liberties data...", type = "message", duration = NULL, id = "fetch_civil_lib")

    civil_lib_df <- get_wb_a_3_civil_lib_data(countries, years)

    if (!is.null(civil_lib_df) && nrow(civil_lib_df) > 0) {
      civil_lib_data(civil_lib_df)
      removeNotification(id = "fetch_civil_lib")
      showNotification("Civil liberties data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_civil_lib")
      showNotification("Failed to fetch civil liberties data.", type = "warning", duration = 5)
    }
    
    # A_4_Fetch judicial constraints data
    showNotification("Fetching judicial constraints data...", type = "message", duration = NULL, id = "fetch_judic")
    
    judic_df <- get_wb_judic_data(countries, years)
    
    if (!is.null(judic_df) && nrow(judic_df) > 0) {
      judic_data(judic_df)
      removeNotification(id = "fetch_judic")
      showNotification("Judicial constraints data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_judic")
      showNotification("Failed to fetch judicial constraints data.", type = "warning", duration = 5)
    }
    
    # B_1_Fetch GDP growth data
    showNotification("Fetching GDP growth data...", type = "message", duration = NULL, id = "fetch_gdp_growth")
    
    b_1_gdp_growth_df <- get_wb_b_1_gdp_growth(countries, years)
    
    if (!is.null(b_1_gdp_growth_df) && nrow(b_1_gdp_growth_df) > 0) {
      gdp_growth_data(b_1_gdp_growth_df)
      removeNotification(id = "fetch_gdp_growth")
      showNotification("GDP growth data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_gdp_growth")
      showNotification("Failed to fetch GDP growth data.", type = "warning", duration = 5)
    }
    
    # B_1_Fetch income classification data
    showNotification("Fetching income classification data...", type = "message", duration = NULL, id = "fetch_gni_pc")
    
    b_1_gni_pc_df <- get_wb_b_1_gni_pc(countries, years)
    
    if (!is.null(b_1_gni_pc_df) && nrow(b_1_gni_pc_df) > 0) {
      gni_pc_data(b_1_gni_pc_df)
      removeNotification(id = "fetch_gni_pc")
      showNotification("Income classification data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_gni_pc")
      showNotification("Failed to fetch income classification data.", type = "warning", duration = 5)
    }

    # B_2_Fetch HDI data
    showNotification("Fetching data on HDI (always takes a little longer)...", type = "message", duration = NULL, id = "fetch")
    
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
    
    # B_3_Climate change data
    showNotification("Fetching INFORM Climage Change data...", type = "message", duration = NULL, id = "fetch_climate_change")
    
    b_3_climate_change_df <- get_wb_b_3_climate_change(countries, years)
    
    if (!is.null(b_3_climate_change_df) && nrow(b_3_climate_change_df) > 0) {
      climate_change_data(b_3_climate_change_df)
      removeNotification(id = "fetch_climate_change")
      showNotification("INFORM Climate Change data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_climate_change")
      showNotification("Failed to fetch INFORM Climate Change data.", type = "warning", duration = 5)
    }
    
    # C_1_Risk Index data
    showNotification("Fetching risk index data...", type = "message", duration = NULL, id = "fetch_risk_index")
    
    c_1_risk_index_df <- get_c_1_risk_index(countries, years)
    
    if (!is.null(c_1_risk_index_df) && nrow(c_1_risk_index_df) > 0) {
      risk_index_data(c_1_risk_index_df)
      removeNotification(id = "fetch_risk_index")
      showNotification("Risk index data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_risk_index")
      showNotification("Failed to fetch Risk index data.", type = "warning", duration = 5)
    }
    
    # C_2_Corruption Perceptions data
    showNotification("Fetching Corruption perceptions data...", type = "message", duration = NULL, id = "fetch_cpi")
    
    c_2_cpi_df <- get_c_2_cpi(countries, years)
    
    if (!is.null(c_2_cpi_df) && nrow(c_2_cpi_df) > 0) {
      cpi_data(c_2_cpi_df)
      removeNotification(id = "fetch_cpi")
      showNotification("Corruption perceptions data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_cpi")
      showNotification("Failed to fetch corruption perceptions data.", type = "warning", duration = 5)
    }
    
    # # C_2_Government effectiveness data
    # showNotification("Fetching Government effectiveness data...", type = "message", duration = NULL, id = "fetch_gov_effectiveness")
    # 
    # c_2_gov_effectiveness_df <- get_wb_c_2_gov_effectiveness(countries, years)
    # 
    # if (!is.null(c_2_gov_effectiveness_df) && nrow(c_2_gov_effectiveness_df) > 0) {
    #   gov_effectiveness_data(c_2_gov_effectiveness_df)
    #   removeNotification(id = "fetch_gov_effectiveness")
    #   showNotification("Government effectiveness data fetched successfully!", type = "message", duration = 2)
    # } else {
    #   removeNotification(id = "fetch_gov_effectiveness")
    #   showNotification("Failed to fetch Government effectiveness data.", type = "warning", duration = 5)
    # }
    # 
    # # C_2_Control of corruption data
    # showNotification("Fetching Control of corruption data...", type = "message", duration = NULL, id = "fetch_ctrl_corruption")
    # 
    # c_2_ctrl_corruption_df <- get_wb_c_2_ctrl_corruption(countries, years)
    # 
    # if (!is.null(c_2_ctrl_corruption_df) && nrow(c_2_ctrl_corruption_df) > 0) {
    #   ctrl_corruption_data(c_2_ctrl_corruption_df)
    #   removeNotification(id = "fetch_ctrl_corruption")
    #   showNotification("Control of corruption data fetched successfully!", type = "message", duration = 2)
    # } else {
    #   removeNotification(id = "fetch_ctrl_corruption")
    #   showNotification("Failed to fetch Control of corruption data.", type = "warning", duration = 5)
    # }
    # 
    # C_3_ODA percent of GNI data
    showNotification("Fetching ODA/GNI data...", type = "message", duration = NULL, id = "fetch_oda_gni")
    
    c_3_oda_gni_df <- get_wb_c_3_oda_gni(countries, years)
    
    if (!is.null(c_3_oda_gni_df) && nrow(c_3_oda_gni_df) > 0) {
      oda_gni_data(c_3_oda_gni_df)
      removeNotification(id = "fetch_oda_gni")
      showNotification("ODA/GNI data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_oda_gni")
      showNotification("Failed to fetch ODA/GNI data.", type = "warning", duration = 5)
    }
    
    # C_4_Core Civil Society Index
    showNotification("Fetching CCSI data...", type = "message", duration = NULL, id = "fetch_ccsi")
    
    c_4_ccsi_df <- get_wb_c_4_ccsi(countries, years)
    
    if (!is.null(c_4_ccsi_df) && nrow(c_4_ccsi_df) > 0) {
      ccsi_data(c_4_ccsi_df)
      removeNotification(id = "fetch_ccsi")
      showNotification("CCSI data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_ccsi")
      showNotification("Failed to fetch CCSI data.", type = "warning", duration = 5)
    }
    
    # C_4_B-READY: Dispute Resolution
    showNotification("Fetching B-READY data...", type = "message", duration = NULL, id = "fetch_bready")
    
    c_4_bready_resolution_df <- get_wb_c_4_bready_resolution(countries, years)
    
    if (!is.null(c_4_bready_resolution_df) && nrow(c_4_bready_resolution_df) > 0) {
      bready_resolution_data(c_4_bready_resolution_df)
      removeNotification(id = "fetch_bready")
      showNotification("B-READY data fetched successfully!", type = "message", duration = 2)
    } else {
      removeNotification(id = "fetch_bready")
      showNotification("Failed to fetch B-READY data.", type = "warning", duration = 5)
    }
    
    removeNotification(id = "fetch")
  })


  # Reactive parts
  # civic_classification <- reactive({
  #   req(civic_data())
  # 
  #   civic_data() |>
  #     mutate(classification = classify_civic(value)) |>
  #     filter(code == input$main_country) |>
  #     filter(year == max(civic_data()$year))
  # })


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


  # A_2_Delib. Democracy Plot
  output$delib_plot <- renderPlot({
    req(delib_data())

    df_delib <- delib_data() |>
      left_join(country_tibble, by = "code")

    nyears <- length(unique(df_delib$year))
    main_country <- input$main_country

    # country <- get_country_name()

    draw_plot(df_delib, main_country, nyears, "Deliberative Democracy Index", "V-DEM")

  })


  # A_3_Civic Summary text
  # output$civic_summary <- renderUI({
  #   req(civic_classification())
  # 
  #   civic <- civic_classification()
  # 
  #   if (nrow(civic) > 0) {
  #     HTML(sprintf(
  #       "The Civic Space classification for <strong>%s</strong> is, as of <strong>%s</strong>, estimated at <strong>%s</strong>, which is classified as <strong>%s</strong>.",
  #       get_country_name(),
  #       civic$year[1],
  #       round(civic$value[1], 3),
  #       civic$classification[1]
  #     ))
  #   }
  # })

  # A_3_Civil Liberties Plot
  output$civil_lib_plot <- renderPlot({
    req(civil_lib_data())

    df <- civil_lib_data() |>
      left_join(country_tibble, by = "code")

    nyears <- length(unique(df$year))
    main_country <- input$main_country

    # country <- get_country_name()

    draw_plot(df, main_country, nyears, "Politiical civil liberties index", "V-DEM")

  })
  
 
  # A_4_Judicial Constraints Plot
  output$judic_plot <- renderPlot({
    req(judic_data())
    
    df_judic <- judic_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_judic$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_judic, main_country, nyears, "Judicial Constraints on the Executive Index", "V-DEM")
    
  })
  
  # B_1_GDP growth Plot
  output$gdp_growth_plot <- renderPlot({
    req(gdp_growth_data())
    
    df_gdp_growth <- gdp_growth_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_gdp_growth$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_gdp_growth, main_country, nyears, "GDP Growth (% change)", "World Development Indicators (WDI)")
    
  })
  
  
  # B_2_HDI Summary text
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

  # B_2_HDI Plot
  output$hdi_plot <- renderPlot({
    req(hdi_data())

    df <- hdi_data()
    nyears <- length(unique(df$year))
    main_country <- input$main_country

    draw_plot(df, main_country, nyears, "Human Development Index (HDI)", "UNDP")

  })
  
  # B_3_Climate Change Plot
  output$climate_change_plot <- renderPlot({
    req(climate_change_data())
    
    df_climate_change <- climate_change_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_climate_change$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_climate_change, main_country, nyears, "Climate Change Risk Index", "INFORM")
    
  })
  
  # C_1_Risk Index Plot
  output$risk_index_plot <- renderPlot({
    req(risk_index_data())
    
    df_risk_index <- risk_index_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_risk_index$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_risk_index, main_country, nyears, "Risk Index", "INFORM")
    
  })
  
  # C_2_Corruption perceptions Plot
  output$cpi_plot <- renderPlot({
    req(cpi_data())
    
    df_cpi <- cpi_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_cpi$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_cpi, main_country, nyears, "Corruption Perceptions Index", "Transparency International")
    
  })
  
  # # C_2_Government effectiveness Plot
  # output$gov_effectiveness_plot <- renderPlot({
  #   req(gov_effectiveness_data())
  #   
  #   df_gov_effectiveness <- gov_effectiveness_data() |>
  #     left_join(country_tibble, by = "code")
  #   
  #   nyears <- length(unique(df_gov_effectiveness$year))
  #   main_country <- input$main_country
  #   
  #   # country <- get_country_name()
  #   
  #   draw_plot(df_gov_effectiveness, main_country, nyears, "Government Effectiveness: Estimate", "World Bank")
  #   
  # })
  # 
  

  
  # # C_2_Control of corruption Plot
  # output$ctrl_corruption_plot <- renderPlot({
  #   req(ctrl_corruption_data())
  #   
  #   df_ctrl_corruption <- ctrl_corruption_data() |>
  #     left_join(country_tibble, by = "code")
  #   
  #   nyears <- length(unique(df_ctrl_corruption$year))
  #   main_country <- input$main_country
  #   
  #   # country <- get_country_name()
  #   
  #   draw_plot(df_ctrl_corruption, main_country, nyears, "Control of corruption: Estimate", "World Bank")
  #   
  # })

  # C_3_ODA percent of GNI Plot
  output$oda_gni_plot <- renderPlot({
    req(oda_gni_data())
    
    df_oda_gni <- oda_gni_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_oda_gni$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_oda_gni, main_country, nyears, "Net ODA received (% of GNI)", "OECD")
    
  })
  
  # C_4_Core Civil Society Index
  output$ccsi_plot <- renderPlot({
    req(ccsi_data())
    
    df_ccsi <- ccsi_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_ccsi$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_ccsi, main_country, nyears, "Core Civil Society Index", "V-DEM")
    
  })
  
  # C_4_B-Ready Dispute Resolution
  output$bready_resolution_plot <- renderPlot({
    req(bready_resolution_data())
    
    df_bready_resolution <- bready_resolution_data() |>
      left_join(country_tibble, by = "code")
    
    nyears <- length(unique(df_bready_resolution$year))
    main_country <- input$main_country
    
    # country <- get_country_name()
    
    draw_plot(df_bready_resolution, main_country, nyears, "B-READY: Dispute Resolution", "B-READY")
    
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

      req(civil_lib_data())
      df_civil_lib <- civil_lib_data() |>
        left_join(country_tibble, by = "code")
      #civ <- civic_classification()

      req(judic_data())
      df_jud <- judic_data() |>
        left_join(country_tibble, by = "code")
      
      req(delib_data())
      df_delib <- delib_data() |>
        left_join(country_tibble, by = "code")
      
      req(gdp_growth_data())
      df_gdp_growth <- gdp_growth_data() |>
        left_join(country_tibble, by = "code")
      
      req(gni_pc_data())
      df_gni_pc <- gni_pc_data() |>
        left_join(country_tibble, by = "code")
      
      req(climate_change_data())
      df_climate_change <- climate_change_data() |>
        left_join(country_tibble, by = "code")
      
      req(risk_index_data())
      df_risk_index <- risk_index_data() |>
        left_join(country_tibble, by = "code")
      
      req(cpi_data())
      df_cpi <- cpi_data() |>
        left_join(country_tibble, by = "code")
      
      # req(gov_effectiveness_data())
      # df_gov_effectiveness <- gov_effectiveness_data() |>
      #   left_join(country_tibble, by = "code")
      # 
      # req(ctrl_corruption_data())
      # df_ctrl_corruption <- ctrl_corruption_data() |>
      #   left_join(country_tibble, by = "code")
      
      req(oda_gni_data())
      df_oda_gni <- oda_gni_data() |>
        left_join(country_tibble, by = "code")
      
      req(ccsi_data())
      df_ccsi <- ccsi_data() |>
        left_join(country_tibble, by = "code")
      
      req(bready_resolution_data())
      df_bready_resolution <- bready_resolution_data() |>
        left_join(country_tibble, by = "code")
      
      code <- input$main_country
      df_title <- as.data.frame(code) |> 
        left_join(country_tibble, by = "code") |> 
        pull(country)
      
      # Classifications
      
      ## i.1 Scenarios
      scenario_dev <- data.frame(
        `Remains unchanged` = "☐",
        `Change with no implications to the programme` = "☐",
        `Change with moderate implications to the programme` = "☐",
        `Change with significant implications to the programme` = "☐", 
        check.names = FALSE
      )
      ft_scenario_dev <- flextable(scenario_dev) |> 
        bold(part = "header") |>
        bg(j = "Remains unchanged", bg = "#DAE9F7") |> 
        bg(j = "Change with no implications to the programme", bg = "#D9F2D0") |> 
        bg(j = "Change with moderate implications to the programme", bg = "#FFFFA3") |> 
        bg(j = "Change with significant implications to the programme", bg = "#FFA7A7") |> 
        width(j = 1:4, width = 1.2) |> 
        fontsize(part = "header", size = 10) |> 
        align(align = "center", part = "all")

      # General table
      empty <- data.frame(
        "Please describe" = "",
        check.names = FALSE
      )
      ft_empty <- flextable(empty) |>
         # bold(part = "header") |>
          color(color = "#808080", part = "header") |> 
          fontsize(part = "header", size = 10) |>
          fontsize(part = "body", size = 10) |>
          border(border.top = fp_border(width = 1),
                 border.bottom = fp_border(width = 1),
                 part = "all") |>
          align(align = "left", part = "all") |> 
          autofit()
        
      
      # Create Word document
      doc <- read_docx(path = "Shiny_Merv_Markdown_Template.docx")

      # Main Title
      doc <- doc |>
        body_add_par("IC Context Monitoring", style = "Title") |>
        body_add_par(df_title, style = "Title") |>
        body_add_par(paste("Date:", Sys.Date()), style = "Date") |>
        body_add_par("", style = "Normal")

      # Intro
      doc <- doc |>
        body_add_par("i. CONCLUSIONS", style = "Title_grey") |>
        body_add_fpar(value = comment_fun("i.1 On scenario development", i_1), 
                      style = "Subtitle_green") |> 
        body_add_par("Scenario development within the past 12 months:", style = "Non_Bullet_Instruction") |> 
        
        body_add_flextable(ft_scenario_dev) |> 
        body_add_par("", style = "Normal") |> 
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("i.2 For strategic and operational steering", i_2), 
                      style = "Subtitle_red") |> 

        text_input_field(placeholder = "Add text here") |> 
        body_add_fpar(value = comment_fun("i.3 For political dialogue and programme advocacy work", i_2), 
                      style = "Subtitle_blue") |> 
        text_input_field(placeholder = "Add text here") |>
        body_add_break()

      # Analyses
      doc <- doc |>
        body_add_par("ii. TREND ANALYSES", style = "Title_grey") 

      # A) Political System section
      doc <- doc |>
        body_add_par("A) Political System", style = "heading 1") 

      # 1) International political context
      doc <- doc |>
        body_add_par("1) International political context", style = "Heading 2_blue")


      # International political context Analysis and Consequences
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_fpar(value = comment_fun("Analysis", analysis_pol), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_par("", style = "Normal") |>
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")

      # 2) Domestic political stability
      doc <- doc |>
        body_add_par("2) Domestic political stability", style = "Heading 2_blue")

      # Create and save the deliberative democracy plot
      temp_plot_delib <- tempfile(fileext = ".png")

      nyears <- length(unique(df_delib$year))
      main_country <- input$main_country

      ggsave(temp_plot_delib, plot = draw_plot(df_delib, main_country, nyears, "Deliberative Democracy index", "V-DEM"), width = 6, height = 2, dpi = 200)

      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_delib, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_delib_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_delib_cat, plot = draw_plot_categories(df_delib, main_country, delib_label, delib_min, delib_max, delib_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_delib_cat, width = 6, height = 0.7, style = "Compact")
      

      # Domestic political stability Analysis and Consequences
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official). The Deliberative Democracy Index ranges from 0 (least democratic)
                     to 1 (most democratic).", style = "Caption_Note") |>
        body_add_fpar(value = comment_fun("Analysis", analysis_delib), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      
        
      # A.3) Civic and political rights, voice and media
      doc <- doc |>
        body_add_par("3) Civil and political rights, voice and media", style = "Heading 2_blue")

      # Create and save the civil liberties plot
      temp_plot_civil_lib <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_civil_lib$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_civil_lib, plot = draw_plot(df_civil_lib, main_country, nyears, "Political civil liberties index", "V-DEM"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_civil_lib, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_civil_lib_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_civil_lib_cat, plot = draw_plot_categories(df_civil_lib, main_country, civil_lib_label, civil_lib_min, civil_lib_max, civil_lib_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_img(src = temp_plot_civil_lib_cat, width = 6, height = 0.7, style = "Compact")
      
      
      # Domestic political stability Analysis and Consequences
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official). The Political Civil Liberties Index ranges from 0 (least liberties)
                     to 1 (most liberties).", style = "Caption_Note") |>  
        body_add_fpar(value = comment_fun("Analysis", analysis_civic), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 

        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      
      # Rule of law, independence of justice, division of power
      doc <- doc |>
        body_add_par("4) Rule of law, independence of justice, division of power", style = "Heading 2_blue") 
      
      # Create and save the judiciary plot
        
      temp_plot_judic <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_jud$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_judic, plot = draw_plot(df_jud, main_country, nyears, "Judicial Constraints on the Executive Index", "V-DEM"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_judic, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_judic_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_judic_cat, plot = draw_plot_categories(df_jud, main_country, judic_label, judic_min, judic_max, judic_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_judic_cat, width = 6, height = 0.7, style = "Compact")
      
      
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official). The Judicial Constraints on the Executive Index ranges from 0 (least constrained)
                     to 1 (most constrained).", style = "Caption_Note") |>    
        body_add_fpar(value = comment_fun("Analysis", analysis_rol), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal") |> 
        # body_add_par("", style = "Line") |> 
        body_add_break()

      # B) Development baselines
      doc <- doc |>
        body_add_par("B) Development baselines", style = "heading 1") |>
        body_add_par("(in IC programme sectors)", style = "Non_Bullet_Instruction")

      # B.1 Economic prospects and systemic gaps
      doc <- doc |>
        body_add_par("1) Economic prospects and systemic gaps", style = "Heading 2_green")
      
      # Create and save the gdp growth plot
      temp_plot_gdp_growth <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_gdp_growth$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_gdp_growth, plot = draw_plot(df_gdp_growth, main_country, nyears, "GDP Growth (% change)", "World Development Indicators (WDI)"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_gdp_growth, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_gni_pc_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_gni_pc_cat, plot = draw_plot_categories(df_gni_pc, main_country, gni_pc_label, gni_pc_min, gni_pc_max, gni_pc_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_par("Income classification:", style = "Normal") |>
        body_add_img(src = temp_plot_gni_pc_cat, width = 6, height = 0.7, style = "Compact")
      
      doc <- doc |>
        body_add_fpar(value = comment_fun("Analysis", analysis_eco), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      

      # B.2 Human capital
      doc <- doc |>
        body_add_par("2) Human capital, poverty and inequalities", style = "Heading 2_green")

      # Create and save the plot
      temp_plot_hdi <- tempfile(fileext = ".png")

      nyears <- length(unique(df$year))
      main_country <- input$main_country

      ggsave(temp_plot_hdi, plot = draw_plot(df, main_country, nyears, "Human Development Index (HDI)", "UNDP"), width = 6, height = 2, dpi = 200)

      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_hdi, width = 6, height = 2, style = "Compact")

      # Create and save the categories plot
      temp_plot_hdi_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_hdi_cat, plot = draw_plot_categories(df, main_country, hdi_label, hdi_min, hdi_max, hdi_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_img(src = temp_plot_hdi_cat, width = 6, height = 0.7, style = "Compact")
 
     # B.2 Analysis and Consequences
      doc <- doc |>
        body_add_par("", style = "Normal") |>
        body_add_fpar(value = comment_fun("Analysis", analysis_hdi), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      
        
      # B.3 Climate & environment risks
      doc <- doc |>
        body_add_par("3) Climate & environment risks", style = "Heading 2_green")
      
      # Create and save the plot
      temp_plot_climate_change <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_climate_change$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_climate_change, plot = draw_plot(df_climate_change, main_country, nyears, "Climate Change Risk Index", "INFORM"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_climate_change, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_climate_change_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_climate_change_cat, plot = draw_plot_categories(df_climate_change, main_country, climate_change_label, climate_change_min, climate_change_max, climate_change_color), width = 6, height = 0.7, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_climate_change_cat, width = 6, height = 0.7, style = "Compact")
      
      doc <- doc |>
        body_add_par("The INFORM Climate Change is essentially a future projection of the
                    INFORM Risk Index. It ranges from 0 (very low risk) 
                     to 10 (very high risk).", style = "Caption_Note") |>    
        body_add_fpar(value = comment_fun("Analysis", analysis_env), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal") |> 
        body_add_break()
      
      # C) Domestic partner context
      
      doc <- doc |>
        body_add_par("C) Domestic partner context", style = "heading 1") |>
        body_add_par("(in IC programme sectors)", style = "Non_Bullet_Instruction")

      # 1) Operational space
      doc <- doc |>
        body_add_par("1) Operational space", style = "Heading 2_red")
      
      # Create and save the plot
      temp_plot_risk_index <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_risk_index$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_risk_index, plot = draw_plot(df_risk_index, main_country, nyears, "Risk Index", "INFORM"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_risk_index, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_risk_index_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_risk_index_cat, plot = draw_plot_categories(df_risk_index, main_country, risk_index_label, risk_index_min, risk_index_max, risk_index_color), width = 6, height = 0.8, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        # body_add_par("", style = "Normal") |>
        body_add_img(src = temp_plot_risk_index_cat, width = 6, height = 0.8, style = "Compact")
      
      doc <- doc |>
        body_add_par("The INFORM Risk Index is a global, open source risk assessment
                     for humanitarian crises and disasters. 
                      It ranges from 0 (very low risk)
                    to 10 (very high risk).", style = "Caption_Note") |>    
        body_add_fpar(value = comment_fun("Analysis", analysis_ops), 
                      style = "heading 3") |>
        body_add_par(operational_note_1, style = "Non_Bullet_Instruction") |>
        body_add_par(operational_note_2, style = "Non_Bullet_Instruction") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      
        
      # 2) Government effectiveness and control of corruption
      doc <- doc |>
        body_add_par("2) Government effectiveness and control of corruption", style = "Heading 2_red") 
      
      # Create and save the plot
      temp_plot_cpi <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_cpi$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_cpi, plot = draw_plot(df_cpi, main_country, nyears, "Corruption Perceptions Index", "Transparency International"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_cpi, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_cpi_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_cpi_cat, plot = draw_plot_categories(df_cpi, main_country, cpi_label, cpi_min, cpi_max, cpi_color), width = 6, height = 0.8, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_cpi_cat, width = 6, height = 0.8, style = "Compact")
      
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official). The Corruption Perceptions Index ranges from 0 (most corrupt)
                     to 100 (least corrupt).", style = "Caption_Note")
      
      doc <- doc |>
        body_add_fpar(value = comment_fun("Analysis", analysis_gov), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal")
      
        
      # 3) Role and relevance of official development assistance (ODA)
      doc <- doc |>
        body_add_par("3) Role and relevance of official development assistance (ODA)", style = "Heading 2_red")
      
      # Create and save the plot
      temp_plot_oda_gni <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_oda_gni$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_oda_gni, plot = draw_plot(df_oda_gni, main_country, nyears, "Net ODA received (% of GNI)", "OECD"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_oda_gni, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_oda_gni_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_oda_gni_cat, plot = draw_plot_categories(df_oda_gni, main_country, oda_gni_label, oda_gni_min, oda_gni_max, oda_gni_color), width = 6, height = 0.8, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_oda_gni_cat, width = 6, height = 0.8, style = "Compact")
      
      
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official).", style = "Caption_Note") |> 
        body_add_par("", style = "Normal") |>
        body_add_fpar(value = comment_fun("Analysis", analysis_oda), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |>         
        body_add_par("", style = "Normal")

        
      # 4) Non-state actors and private sector
      doc <- doc |>
        body_add_par("4) Non-state actors and private sector", style = "Heading 2_red")
      
      # Create and save the CCSI plot
      temp_plot_ccsi <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_ccsi$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_ccsi, plot = draw_plot(df_ccsi, main_country, nyears, "Core Civil Society Index", "CCSI"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_ccsi, width = 6, height = 2, style = "Compact")
      
      # Create and save the categories plot
      temp_plot_ccsi_cat <- tempfile(fileext = ".png")
      
      main_country <- input$main_country
      
      ggsave(temp_plot_ccsi_cat, plot = draw_plot_categories(df_ccsi, main_country, ccsi_label, ccsi_min, ccsi_max, ccsi_color), width = 6, height = 0.8, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_ccsi_cat, width = 6, height = 0.8, style = "Compact")
      
      doc <- doc |>
        body_add_par("The categories shown are indicative (cuf-off values are not official). The Core Civil Society Index ranges from 0 (weak, repressed civil society)
                     to 1 (robust, autonomous civil society).", style = "Caption_Note")
      
      
      # Create and save the B-READY plot
      temp_plot_bready <- tempfile(fileext = ".png")
      
      nyears <- length(unique(df_bready_resolution$year))
      main_country <- input$main_country
      
      ggsave(temp_plot_bready, plot = draw_plot(df_bready_resolution, main_country, nyears, "B-READY: Dispute Resolution", "B-READY"), width = 6, height = 2, dpi = 200)
      
      # Add plot to document
      doc <- doc |>
        body_add_img(src = temp_plot_bready, width = 6, height = 2, style = "Compact")
      
      
      
      doc <- doc |>
        body_add_par("The B-READY: Dispute Resolution measures efficiency and quality
                     of the resolution of commercial disputes based on three dimensions
                     (quality of regulations, public services and ease of resolving a commercial
                     dispute. The overall score ranges from 0 (worst)
                     to 100 (best). This is a new indicator and not yet available
                     for all countries.", style = "Caption_Note") |> 
        body_add_fpar(value = comment_fun("Analysis", analysis_nsa), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>
        text_input_field(placeholder = "Add text here") |> 
        body_add_par("", style = "Normal") |> 
      
        body_add_break()

      
      # D) Additional fields of observation
      
      doc <- doc |>
        body_add_par("D) OPTIONAL: Additional fields of observation", style = "heading 1") |>
        body_add_par("(in IC programme sectors)", style = "Non_Bullet_Instruction") |>
        body_add_par("", style = "Normal") |> 
        body_add_par("Analysis", style = "heading 3") |> 
        text_input_field(placeholder = "Add text here") |> 
        body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                      style = "heading 3") |>        
        text_input_field(placeholder = "Add text here")
      
      # Save document
      print(doc, target = file)

      # Clean up temp file
      unlink(temp_plot_hdi)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
