# Reactive value to store data
fsi_data <- reactiveVal(NULL)
civil_lib_data <- reactiveVal(NULL)
stability_data <- reactiveVal(NULL)
rol_data <- reactiveVal(NULL)
elect_data <- reactiveVal(NULL)
regime_data <- reactiveVal(NULL)
gdp_growth_data <- reactiveVal(NULL)
gni_pc_data <- reactiveVal(NULL)
hdi_data <- reactiveVal(NULL)
gii_data <- reactiveVal(NULL)
climate_change_data <- reactiveVal(NULL)
risk_index_data <- reactiveVal(NULL)
cpi_data <- reactiveVal(NULL)
# fgi_data <- reactiveVal(NULL)
# gov_effectiveness_data <- reactiveVal(NULL)
# ctrl_corruption_data <- reactiveVal(NULL)
oda_gni_data <- reactiveVal(NULL)
ccsi_data <- reactiveVal(NULL)
bready_resolution_data <- reactiveVal(NULL)

# Fetch data when button is clicked
observeEvent(input$fetch_data, {
  req(input$main_country, input$years)
  
  countries <- c(input$main_country, input$comparison_countries)
  years <- as.numeric(input$years)
  
  # A_1_Fetch Fragile States index data
  showNotification("Fetching Fragile States index data...", type = "message", duration = NULL, id = "fetch_fsi")
  
  fsi_df <- get_a_1_fs_index(countries, years)
  
  if (!is.null(fsi_df) && nrow(fsi_df) > 0) {
    fsi_data(fsi_df)
    removeNotification(id = "fetch_fsi")
    showNotification("Fragile States index data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_fsi")
    showNotification("Failed to fetch Fragile States index data.", type = "warning", duration = 5)
  }
  
  # A_2_Fetch Political Stability data
  showNotification("Fetching Political Stability index data...", type = "message", duration = NULL, id = "fetch_stability")
  
  stability_df <- get_wb_stability_data(countries, years)
  
  if (!is.null(stability_df) && nrow(stability_df) > 0) {
    stability_data(stability_df)
    removeNotification(id = "fetch_stability")
    showNotification("Political Stability index data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_stability")
    showNotification("Failed to fetch Political Stability index data.", type = "warning", duration = 5)
  }
  
  # A_2_Fetch electoral democracy data
  showNotification("Fetching electoral democracy index data...", type = "message", duration = NULL, id = "fetch_elect")
  
  elect_df <- get_owid_electoral_data(countries, years)
  
  if (!is.null(elect_df) && nrow(elect_df) > 0) {
    elect_data(elect_df)
    removeNotification(id = "fetch_elect")
    showNotification("Electoral democracy index data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_elect")
    showNotification("Failed to fetch electoral democracy index data.", type = "warning", duration = 5)
  }
  
  # A_2_Fetch regime type data
  showNotification("Fetching regime type data...", type = "message", duration = NULL, id = "fetch_regime")
  
  regime_df <- get_owid_regime_data(countries, years)
  
  if (!is.null(regime_df) && nrow(regime_df) > 0) {
    regime_data(regime_df)
    removeNotification(id = "fetch_regime")
    showNotification("Regime type data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_regime")
    showNotification("Failed to fetch regime type data.", type = "warning", duration = 5)
  }
  
  
  # A_3_Fetch civil liberties data
  showNotification("Fetching civil liberties data...", type = "message", duration = NULL, id = "fetch_civil_lib")
  
  civil_lib_df <- get_owid_a_3_civil_lib_data(countries, years)
  
  if (!is.null(civil_lib_df) && nrow(civil_lib_df) > 0) {
    civil_lib_data(civil_lib_df)
    removeNotification(id = "fetch_civil_lib")
    showNotification("Civil liberties data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_civil_lib")
    showNotification("Failed to fetch civil liberties data.", type = "warning", duration = 5)
  }
  
  # A_4_Fetch rule of law data
  showNotification("Fetching rule of law data...", type = "message", duration = NULL, id = "fetch_rol")
  
  rol_df <- get_owid_a_4_rol_data(countries, years)
  
  if (!is.null(rol_df) && nrow(rol_df) > 0) {
    rol_data(rol_df)
    removeNotification(id = "fetch_rol")
    showNotification("Rule of law data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_rol")
    showNotification("Failed to fetch rule of law data.", type = "warning", duration = 5)
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
  showNotification("Fetching data on HDI...", type = "message", duration = NULL, id = "fetch_hdi")
  
  b_2_hdi_df <- get_owid_b_2_hdi_data(countries, years)
  
  if (!is.null(b_2_hdi_df) && nrow(b_2_hdi_df) > 0) {
    hdi_data(b_2_hdi_df)
    removeNotification(id = "fetch_hdi")
    showNotification("HDI data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_hdi")
    showNotification("Failed to fetch HDI data.", type = "warning", duration = 5)
  }
  
  # B_2_Fetch Gender Inequality data
  showNotification("Fetching Gender Inequality Index data...", type = "message", duration = NULL, id = "fetch_gii")
  
  b_2_gii_df <- get_owid_b_2_gender_ineq_data(countries, years)
  
  if (!is.null(b_2_gii_df) && nrow(b_2_gii_df) > 0) {
    gii_data(b_2_gii_df)
    removeNotification(id = "fetch_gii")
    showNotification("Gender Inequality Index data fetched successfully!", type = "message", duration = 2)
  } else {
    removeNotification(id = "fetch_gii")
    showNotification("Failed to fetch Gender Inequality Index data.", type = "warning", duration = 5)
  }
  
  
  # B_3_Climate change data
  showNotification("Fetching INFORM Climate Change data...", type = "message", duration = NULL, id = "fetch_climate_change")
  
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
  
  c_1_risk_index_df <- get_wb_c_1_risk_index(countries, years)
  
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
  
  # # C_2_Functioning Government data
  # showNotification("Fetching Functioning government index data...", type = "message", duration = NULL, id = "fetch_fgi")
  # 
  # c_2_fgi_df <- get_owid_c_2_fgi_data(countries, years)
  # 
  # if (!is.null(c_2_fgi_df) && nrow(c_2_fgi_df) > 0) {
  #   fgi_data(c_2_fgi_df)
  #   removeNotification(id = "fetch_fgi")
  #   showNotification("Functioning government data fetched successfully!", type = "message", duration = 2)
  # } else {
  #   removeNotification(id = "fetch_fgi")
  #   showNotification("Failed to fetch functioning government data.", type = "warning", duration = 5)
  # }
  # 
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