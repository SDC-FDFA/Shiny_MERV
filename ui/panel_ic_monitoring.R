panel_ic_monitoring <- nav_panel(
  title = "IC Template",
  icon = icon("file-word"),
  
  page_sidebar(sidebar = sidebar(
    width = 250,
    p("Select countries and year and click on 'Fetch data' at the bottom",
      style = "font-size: 0.75rem; color: #666;"),
    selectInput("main_country", "Main Country:", choices = country_list, 
                #selected = "LAO"
                ),
    checkboxGroupInput("comparison_countries", "Reference Countries:", choices = country_list, 
                       #selected = c("KHM", "THA")
                       ),
    checkboxGroupInput("years", "Years:", choices = 2018:2025, selected = 2018:2025, inline = TRUE),
    actionButton("fetch_data", "1. Fetch Data", icon = icon("download"),
                 class = "btn-primary w-100 mt-2")
  ),
  
  div(class = "section-header", "A) Political System"),
  
  # h3("A) Political System"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "1) International political context"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("fsi_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Domestic Political Stability"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("stability_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("2) Domestic Political Stability"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("elect_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) Civic and political rights, voice and media"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("civil_lib_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("3) Civic and political rights, voice and media"),
  # 
  # # hr(),
  # 
  # # uiOutput("civic_summary"),
  # # 
  # # hr(),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("civil_lib_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "4) Rule of law, independence of justice, division of power"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("rol_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("4) Rule of law, independence of justice, division of power"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("rol_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  #h3("B) Development baselines"),
  
  div(class = "section-header", "B) Development baselines"),
  
  hr(),
  
  #h4("1) GDP Growth"),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "1) GDP Growth"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("gdp_growth_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  #withSpinner(girafeOutput("gdp_growth_plot", width = "100%", height = "300px")),
  
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("gdp_growth_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Human capital, poverty and inequalities"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("hdi_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("2) Human capital, poverty and inequalities"),
  # 
  # #uiOutput("hdi_summary"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("hdi_plot", width = "100%", height = "300px")
  # ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "2) Human capital, poverty and inequalities"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("gii_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("gii_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) Climate & environment risks"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("climate_change_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("3) Climate & environment risks"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("climate_change_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(class = "section-header", "C) Domestic partner context"),
  
  # h3("C) Domestic partner context"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "1) Operational space"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("risk_index_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("1) Operational space"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("risk_index_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Government effectiveness and control of corruption"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("fgi_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("2) Government effectiveness and control of corruption"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("fgi_plot", width = "100%", height = "300px")
  # ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "2) Government effectiveness and control of corruption"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("cpi_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("cpi_plot", width = "100%", height = "300px")
  # ),
  
  # plotOutput("gov_effectiveness_plot", height = "300px"),
  # plotOutput("ctrl_corruption_plot", height = "300px"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) ODA as percent of recipient GNI"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("oda_gni_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("3) ODA as percent of recipient GNI"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("oda_gni_plot", width = "100%", height = "300px")
  # ),
  # 
  # hr(),
  # 
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "4) Non-state actors and private sector"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("ccsi_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # h4("4) Non-state actors and private sector"),
  # 
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("ccsi_plot", width = "100%", height = "300px")
  # ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "4) Non-state actors and private sector"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("bready_resolution_plot", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  # div(
  #   style = "width: 100%; max-width: 900px; height: 300px; overflow: hidden;",
  #   girafeOutput("bready_resolution_plot", width = "100%", height = "300px")
  # ),
  
  hr(),
  
  downloadButton(
    "download_report",
    "2. Download the IC Context Monitoring Word template",
    class = "btn-success",
    style = "width: 100%; margin-top: 20px;"
  )
  ))