
# A_2_Elect. Democracy Plot
output$elect_plot_reg <- renderGirafe({
  req(elect_data_reg())
  
  df_elect <- elect_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_elect$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_elect, nyears, "Electoral Democracy Index", v_dem)
  
}
# res = 96
)



# A_3_Civil Liberties Plot
output$civil_lib_plot_reg <- renderGirafe({
  req(civil_lib_data_reg())
  
  df <- civil_lib_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df, nyears, "Political civil liberties index", v_dem)
  
})


# A_4_Rule of law  Plot
output$rol_plot_reg <- renderGirafe({
  req(rol_data_reg())
  
  df_rol <- rol_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_rol$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_rol, nyears, "Rule of Law Index", v_dem)
  
})

# B_1_GDP growth Plot
output$gdp_growth_plot_reg <- renderGirafe({
  req(gdp_growth_data_reg())
  
  df_gdp_growth <- gdp_growth_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_gdp_growth$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_gdp_growth, nyears, "GDP Growth (% change)", "World Development Indicators (WDI)")
  
})


# B_2_HDI Summary text
# output$hdi_summary <- renderUI({
#   req(hdi_classification())
# 
#   hdi <- hdi_classification()
# 
#   if (nrow(hdi) > 0) {
#     HTML(sprintf(
#       "The HDI for <strong>%s</strong> is, as of <strong>%s</strong>, estimated at <strong>%s</strong>, which is classified as <strong>%s</strong>.",
#       hdi$country[1],
#       hdi$year[1],
#       round(hdi$value[1], 3),
#       hdi$classification[1]
#     ))
#   }
# })

# B_2_HDI Plot
output$hdi_plot_reg <- renderGirafe({
  
  req(hdi_data_reg())
  
  df_hdi <- hdi_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_hdi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_hdi, nyears, "Human Development Index (HDI)", undp)
  
})

# B_2_GII Plot
output$gii_plot_reg <- renderGirafe({
  
  req(gii_data_reg())
  
  df_gii <- gii_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_gii$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_gii, nyears, "Gender Inequality Index", undp)
  
})

# B_3_Climate Change Plot
output$climate_change_plot_reg <- renderGirafe({
  req(climate_change_data_reg())
  
  df_climate_change <- climate_change_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_climate_change$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_climate_change, nyears, "Climate Change Risk Index", "INFORM")
  
})

# C_1_Risk Index Plot
output$risk_index_plot_reg <- renderGirafe({
  req(risk_index_data_reg())
  
  df_risk_index <- risk_index_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_risk_index$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_risk_index, nyears, "Risk Index", "INFORM")
  
})

# C_2_Corruption perceptions Plot
output$cpi_plot_reg <- renderGirafe({
  req(cpi_data_reg())
  
  df_cpi <- cpi_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_cpi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_cpi, nyears, "Corruption Perceptions Index", "Transparency International")
  
})

# C_2_Functioning government index Plot
output$fgi_plot_reg <- renderGirafe({
  req(fgi_data_reg())
  
  df_fgi <- fgi_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_fgi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_fgi, nyears, "Functioning Government Index", "Economist Intelligence Unit processed by Our World in Data")
  
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
#   draw_plot(df_gov_effectiveness, nyears, "Government Effectiveness: Estimate", "World Bank")
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
#   draw_plot(df_ctrl_corruption, nyears, "Control of corruption: Estimate", "World Bank")
#   
# })

# C_3_ODA percent of GNI Plot
output$oda_gni_plot_reg <- renderGirafe({
  req(oda_gni_data_reg())
  
  df_oda_gni <- oda_gni_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_oda_gni$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_oda_gni, nyears, "Net ODA received (% of GNI)", "OECD")
  
})

# C_4_Core Civil Society Index
output$ccsi_plot_reg <- renderGirafe({
  req(ccsi_data_reg())
  
  df_ccsi <- ccsi_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_ccsi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_ccsi, nyears, "Civil Society Participation Index", v_dem)
  
})

# C_4_B-Ready Dispute Resolution
output$bready_resolution_plot_reg <- renderGirafe({
  req(bready_resolution_data_reg())
  
  df_bready_resolution <- bready_resolution_data_reg() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_bready_resolution$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe_reg(df_bready_resolution, nyears, "B-READY: Dispute Resolution", "B-READY")
  
})