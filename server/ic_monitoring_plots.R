get_country_name <- reactive({
  names(country_list)[country_list == input$main_country]
})

# A_1_Fragile States Index Plot
output$fsi_plot <- renderGirafe({
  req(fsi_data())
  
  df_fsi <- fsi_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_fsi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_fsi, main_country, nyears, "Fragile States Index", "Fund for Peace")
  
}
# res = 96
)

# A_2_Political Stability Plot
output$stability_plot <- renderGirafe({
  req(stability_data())
  
  df_stability <- stability_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_stability$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_stability, main_country, nyears, "Political Stability Index", wb_wgi)
  
}
# res = 96
)

# A_2_Elect. Democracy Plot
output$elect_plot <- renderGirafe({
  req(elect_data())
  
  df_elect <- elect_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_elect$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_elect, main_country, nyears, "Electoral Democracy Index", v_dem)
  
}
# res = 96
)


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
output$civil_lib_plot <- renderGirafe({
  req(civil_lib_data())
  
  df <- civil_lib_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df, main_country, nyears, "Political civil liberties index", v_dem)
  
})


# A_4_Rule of law  Plot
output$rol_plot <- renderGirafe({
  req(rol_data())
  
  df_rol <- rol_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_rol$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_rol, main_country, nyears, "Rule of Law Index", v_dem)
  
})

# B_1_GDP growth Plot
output$gdp_growth_plot <- renderGirafe({
  req(gdp_growth_data())
  
  df_gdp_growth <- gdp_growth_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_gdp_growth$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_gdp_growth, main_country, nyears, "GDP Growth (% change)", "World Development Indicators (WDI)")
  
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
output$hdi_plot <- renderGirafe({
  
  req(hdi_data())
  
  df_hdi <- hdi_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_hdi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_hdi, main_country, nyears, "Human Development Index (HDI)", undp)
  
})

# B_2_GII Plot
output$gii_plot <- renderGirafe({
  
  req(gii_data())
  
  df_gii <- gii_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_gii$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_gii, main_country, nyears, "Gender Inequality Index", undp)
  
})

# B_3_Climate Change Plot
output$climate_change_plot <- renderGirafe({
  req(climate_change_data())
  
  df_climate_change <- climate_change_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_climate_change$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_climate_change, main_country, nyears, "Climate Change Risk Index", "INFORM")
  
})

# C_1_Risk Index Plot
output$risk_index_plot <- renderGirafe({
  req(risk_index_data())
  
  df_risk_index <- risk_index_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_risk_index$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_risk_index, main_country, nyears, "Risk Index", "INFORM")
  
})

# C_2_Corruption perceptions Plot
output$cpi_plot <- renderGirafe({
  req(cpi_data())
  
  df_cpi <- cpi_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_cpi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_cpi, main_country, nyears, "Corruption Perceptions Index", "Transparency International")
  
})

# C_2_Quality of Government Plot
output$fhfgi_plot <- renderGirafe({
  req(fhfgi_data())

  df_fhfgi <- fhfgi_data() |>
    left_join(country_tibble, by = "code")

  nyears <- length(unique(df_fhfgi$year))
  main_country <- input$main_country

  # country <- get_country_name()

  draw_plot_girafe(df_fhfgi, main_country, nyears, "Functioning of Government", "Freedom House")

})

# C_2_Functioning government index Plot
# output$fgi_plot <- renderGirafe({
#   req(fgi_data())
#   
#   df_fgi <- fgi_data() |>
#     left_join(country_tibble, by = "code")
#   
#   nyears <- length(unique(df_fgi$year))
#   main_country <- input$main_country
#   
#   # country <- get_country_name()
#   
#   draw_plot_girafe(df_fgi, main_country, nyears, "Functioning Government Index", "Economist Intelligence Unit processed by Our World in Data")
#   
# })
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
output$oda_gni_plot <- renderGirafe({
  req(oda_gni_data())
  
  df_oda_gni <- oda_gni_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_oda_gni$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_oda_gni, main_country, nyears, "Net ODA received (% of GNI)", "OECD")
  
})

# C_4_Core Civil Society Index
output$ccsi_plot <- renderGirafe({
  req(ccsi_data())
  
  df_ccsi <- ccsi_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_ccsi$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_ccsi, main_country, nyears, "Civil Society Participation Index", v_dem)
  
})

# C_4_B-Ready Dispute Resolution
output$bready_resolution_plot <- renderGirafe({
  req(bready_resolution_data())
  
  df_bready_resolution <- bready_resolution_data() |>
    left_join(country_tibble, by = "code")
  
  nyears <- length(unique(df_bready_resolution$year))
  main_country <- input$main_country
  
  # country <- get_country_name()
  
  draw_plot_girafe(df_bready_resolution, main_country, nyears, "B-READY: Dispute Resolution", "B-READY")
  
})