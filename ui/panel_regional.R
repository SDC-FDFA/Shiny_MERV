panel_regional <- nav_panel(
  title = "Regional Analysis",
  icon = icon("chart-line"),
  
  page_sidebar(sidebar = sidebar(
    width = 250,
    p("Select countries and year and click on 'Fetch data' at the bottom",
      style = "font-size: 0.75rem; color: #666;"),
    #selectInput("main_country", "Main Country:", choices = country_list, selected = "LAO"),
    checkboxGroupInput("comparison_countries_reg", "Select Countries:", choices = country_list, 
                       #selected = c("KHM", "THA")
                       ),
    checkboxGroupInput("years", "Years:", choices = 2018:2025, selected = 2018:2025, inline = TRUE),
    actionButton("fetch_data_reg", "1. Fetch Data", icon = icon("download"),
                 class = "btn-primary w-100 mt-2")
  ),
  
  div(class = "section-header", "A) Political System"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "1) International political context"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("fsi_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  # h3("A) Political System"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Domestic Political Stability"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("stability_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) Civic and political rights, voice and media"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("civil_lib_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "4) Rule of law, independence of justice, division of power"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("rol_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
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
      withSpinner(girafeOutput("gdp_growth_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Human capital, poverty and inequalities"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("hdi_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "2) Human capital, poverty and inequalities"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("gii_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) Climate & environment risks"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("climate_change_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(class = "section-header", "C) Domestic partner context"),
  
  # h3("C) Domestic partner context"),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "1) Operational space"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("risk_index_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "2) Government effectiveness and control of corruption"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("qog_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "2) Government effectiveness and control of corruption"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("cpi_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  hr(),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "3) ODA as percent of recipient GNI"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("oda_gni_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  div(
    class = "plot-card",
    div(class = "plot-card-header", "4) Non-state actors and private sector"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("ccsi_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  div(
    class = "plot-card",
    #div(class = "plot-card-header", "4) Non-state actors and private sector"),
    div(
      class = "plot-card-body",
      withSpinner(girafeOutput("bready_resolution_plot_reg", width = "100%", height = "300px"),
                  type = 7, color.background = "#FFFFFF", color = "#519796")
    )
  ),
  
  div(class = "small", "If B-READY value is 0, no values available for the selected country."),
  
  hr(),
  
  ))