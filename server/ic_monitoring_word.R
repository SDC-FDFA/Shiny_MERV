# Download handler for Word document
output$download_report <- downloadHandler(
  filename = function() {
    paste0("IC_Monitoring_Report_", Sys.Date(), ".docx")
  },
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  content = function(file) {
    
    req(fsi_data())
    df_fsi <- fsi_data() |>
      left_join(country_tibble, by = "code")
    
    req(stability_data())
    df_stability <- stability_data() |>
      left_join(country_tibble, by = "code")
    
    req(civil_lib_data())
    df_civil_lib <- civil_lib_data() |>
      left_join(country_tibble, by = "code")
    #civ <- civic_classification()
    
    req(rol_data())
    df_jud <- rol_data() |>
      left_join(country_tibble, by = "code")
    
    req(elect_data())
    df_elect <- elect_data() |>
      left_join(country_tibble, by = "code")
    
    req(regime_data())
    df_regime <- regime_data() |>
      left_join(country_tibble, by = "code")
    
    req(gdp_growth_data())
    df_gdp_growth <- gdp_growth_data() |>
      left_join(country_tibble, by = "code")
    
    req(gni_pc_data())
    df_gni_pc <- gni_pc_data() |>
      left_join(country_tibble, by = "code")
    
    req(hdi_data())
    df_hdi <- hdi_data() |>
      left_join(country_tibble, by = "code")
    #hdi <- hdi_classification()
    
    req(gii_data())
    df_gii <- gii_data() |>
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
    
    req(qog_data())
    df_qog <- qog_data() |>
      left_join(country_tibble, by = "code")
    
    # req(fgi_data())
    # df_fgi <- fgi_data() |>
    #   left_join(country_tibble, by = "code")
    
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
      `Changes with no implications to the programme` = "☐",
      `Changes with moderate implications to the programme` = "☐",
      `Changes with significant implications to the programme` = "☐", 
      check.names = FALSE
    )
    ft_scenario_dev <- flextable(scenario_dev) |> 
      bold(part = "header") |>
      bg(j = "Remains unchanged", bg = "#e6e6e6ff") |> 
      bg(j = "Changes with no implications to the programme", bg = "#ccccccff") |> 
      bg(j = "Changes with moderate implications to the programme", bg = "#b3b3b3ff") |> 
      bg(j = "Changes with significant implications to the programme", bg = "#999999ff") |> 
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
      body_add_fpar(value = comment_fun("i.1 On context development", i_1), 
                    style = "Subtitle_green") |> 
      body_add_par("Context changes within the past 12 months:", style = "Non_Bullet_Instruction") |> 
      
      body_add_flextable(ft_scenario_dev) |> 
      body_add_par("", style = "Normal") |> 
      text_input_field(placeholder = "Add text here") |> 
      
      body_add_fpar(value = comment_fun("i.2 For strategic and operational steering", i_2), 
                    style = "Subtitle_red") |> 
      
      text_input_field(placeholder = "Add text here") |> 
      body_add_fpar(value = comment_fun("i.3 For political dialogue and programme advocacy work", i_3), 
                    style = "Subtitle_blue") |> 
      text_input_field(placeholder = "Add text here") |>
      body_add_break()
    
    # Analyses
    doc <- doc |>
      body_add_par("ii. TREND ANALYSES", style = "Title_grey") 
    
    # A) Political System section
    doc <- doc |>
      body_add_par("A) Political system", style = "heading 1") 
    
    # 1) International political context
    doc <- doc |>
      body_add_par("1) International political context", style = "Heading 2_blue")
    
    # Create and save the fragile states plot
    temp_plot_fsi <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_fsi$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_fsi, plot = draw_plot(df_fsi, main_country, nyears, "Fragile States Index", "Fund for Peace"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_fsi, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_fsi_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_fsi_cat, plot = draw_plot_categories_noval(df_fsi, main_country, fsi_label, fsi_min, fsi_max, fsi_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_fsi_cat, width = 6, height = 0.5, style = "Compact")
    
    # International political context Analysis and Consequences
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The "),
          hyperlink_ftext(
            text = "Fragile States Index",
            href = "https://fragilestatesindex.org/global-data/",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (least fragile) to 120 (most fragile). Published by the The Fund for Peace."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
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
    
    # Create and save the political stability plot
    temp_plot_stability <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_stability$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_stability, plot = draw_plot(df_stability, main_country, nyears, "Political Stability index", wb_wgi), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_stability, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_stability_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_stability_cat, plot = draw_plot_categories_noval(df_stability, main_country, stability_label, stability_min, stability_max, stability_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_stability_cat, width = 6, height = 0.5, style = "Compact")
    
    
    # Domestic political stability Analysis and Consequences
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Political Stability Index",
            href = "https://data360.worldbank.org/en/indicator/GOV_WGI_PV",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (very low stability) to 100 (very high stability)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
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
    
    ggsave(temp_plot_civil_lib, plot = draw_plot(df_civil_lib, main_country, nyears, "Political civil liberties index", v_dem), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_civil_lib, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_civil_lib_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_civil_lib_cat, plot = draw_plot_categories_noval(df_civil_lib, main_country, civil_lib_label, civil_lib_min, civil_lib_max, civil_lib_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      # body_add_par("", style = "Normal") |>
      body_add_img(src = temp_plot_civil_lib_cat, width = 6, height = 0.5, style = "Compact")
    
    
    # Domestic political stability Analysis and Consequences
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Political Civil Liberties Index",
            href = "https://ourworldindata.org/grapher/political-civil-liberties-index",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (least liberties)
                     to 1 (most liberties)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
      # 
      # body_add_par("The categories shown are indicative (cut-off values are not official). The Political Civil Liberties Index ranges from 0 (least liberties)
      #              to 1 (most liberties).", style = "Caption_Note") |>  
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
    
    # Create and save the roliary plot
    
    temp_plot_rol <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_jud$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_rol, plot = draw_plot(df_jud, main_country, nyears, "Rule of Law Index", v_dem), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_rol, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_rol_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_rol_cat, plot = draw_plot_categories_noval(df_jud, main_country, rol_label, rol_min, rol_max, rol_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_rol_cat, width = 6, height = 0.5, style = "Compact")
    
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Rule of Law Index",
            href = "https://ourworldindata.org/grapher/rule-of-law-index",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (least rule-based)
                     to 1 (most rule-based)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
      
      # body_add_par("The categories shown are indicative (cut-off values are not official). The Rule of Law Index ranges from 0 (least rule-based)
      #              to 1 (most rule_based).", style = "Caption_Note") |>    
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
    
    ggsave(temp_plot_gdp_growth, plot = draw_plot(df_gdp_growth, main_country, nyears, "GDP Growth (% change)", "World Development Indicators (WDI)"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_gdp_growth, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_gni_pc_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_gni_pc_cat, plot = draw_plot_categories(df_gni_pc, main_country, gni_pc_label, gni_pc_min, gni_pc_max, gni_pc_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_par("Income classification:", style = "Normal") |>
      body_add_img(src = temp_plot_gni_pc_cat, width = 6, height = 0.5, style = "Compact")
    
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
    
    nyears <- length(unique(df_hdi$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_hdi, plot = draw_plot(df_hdi, main_country, nyears, "Human Development Index (HDI)", undp), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_hdi, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_hdi_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_hdi_cat, plot = draw_plot_categories_noval(df_hdi, main_country, hdi_label, hdi_min, hdi_max, hdi_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_par("HDI classification:", style = "Normal") |>
      body_add_img(src = temp_plot_hdi_cat, width = 6, height = 0.5, style = "Compact")
    
    # Create and save the plot
    temp_plot_gii <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_gii$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_gii, plot = draw_plot(df_gii, main_country, nyears, "Gender Inequality Index", undp), width = 6, height = 1.8, dpi = 200)
    
    # Create and save the categories plot
    temp_plot_gii_pc_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_gii_pc_cat, plot = draw_plot_categories_noval(df_gii, main_country, gii_label, gii_min, gii_max, gii_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_gii, width = 6, height = 1.8, style = "Compact") |> 
      body_add_img(src = temp_plot_gii_pc_cat, width = 6, height = 0.5, style = "Compact") |> 
      body_add_fpar(
        fpar(
          ftext("These cut-offs vary slightly across studies but remain consistent in academic and policy literature. The "),
          hyperlink_ftext(
            text = "Gender Inequality Index",
            href = "https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" covers the dimensions of reproductive health, empowerment and economic status. It ranges from 0 (very low inequality) to 10 (very high inequality)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) 
    # body_add_par("The Gender Inequality Index covers the dimensions of reproductive health, 
    # empowerment and economic status. It ranges from 0 (very low inequality) 
    #              to 10 (very high inequality).", style = "Caption_Note")
    
    
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
    
    ggsave(temp_plot_climate_change, plot = draw_plot(df_climate_change, main_country, nyears, "Climate Change Risk Index", "INFORM"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_climate_change, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_climate_change_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_climate_change_cat, plot = draw_plot_categories_noval(df_climate_change, main_country, climate_change_label, climate_change_min, climate_change_max, climate_change_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_climate_change_cat, width = 6, height = 0.5, style = "Compact")
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The "),
          hyperlink_ftext(
            text = "INFORM Climate Change",
            href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Climate-Change",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" is essentially a future projection of the "),
          hyperlink_ftext(
            text = "INFORM Risk Index",
            href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(". It ranges from 0 (very low risk) to 10 (very high risk)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
      # body_add_par("The INFORM Climate Change is essentially a future projection of the
      #             INFORM Risk Index. It ranges from 0 (very low risk) 
      #              to 10 (very high risk).", style = "Caption_Note") |>    
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
    
    ggsave(temp_plot_risk_index, plot = draw_plot(df_risk_index, main_country, nyears, "Risk Index", "INFORM"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_risk_index, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_risk_index_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_risk_index_cat, plot = draw_plot_categories_noval(df_risk_index, main_country, risk_index_label, risk_index_min, risk_index_max, risk_index_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      # body_add_par("", style = "Normal") |>
      body_add_img(src = temp_plot_risk_index_cat, width = 6, height = 0.5, style = "Compact")
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The "),
          hyperlink_ftext(
            text = "INFORM Risk Index",
            href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" is a global, open-source risk assessment for humanitarian crises and disasters. It ranges from 0 (very low risk) to 10 (very high risk)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
      # body_add_par("The INFORM Risk Index is a global, open source risk assessment
      #              for humanitarian crises and disasters. 
      #               It ranges from 0 (very low risk)
      #             to 10 (very high risk).", style = "Caption_Note") |>    
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
    temp_plot_qog <- tempfile(fileext = ".png")

    nyears <- length(unique(df_qog$year))
    main_country <- input$main_country

    ggsave(temp_plot_qog, plot = draw_plot(df_qog, main_country, nyears, "Quality of Government (QoG) Index", "University of Gothenburg"), width = 6, height = 1.8, dpi = 200)

    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_qog, width = 6, height = 1.8, style = "Compact")

    # Create and save the categories plot
    temp_plot_qog_cat <- tempfile(fileext = ".png")

    main_country <- input$main_country

    ggsave(temp_plot_qog_cat, plot = draw_plot_categories_noval(df_qog, main_country, qog_label, qog_min, qog_max, qog_color), width = 6, height = 0.5, dpi = 200)

    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_qog_cat, width = 6, height = 0.5, style = "Compact")

    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Quality of Government (QoG) Index",
            href = "https://datafinder.qog.gu.se/variable/icrg_qog",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (least) to 1 (highest quality)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      )

    
    # # Create and save the plot
    # temp_plot_fgi <- tempfile(fileext = ".png")
    # 
    # nyears <- length(unique(df_fgi$year))
    # main_country <- input$main_country
    # 
    # ggsave(temp_plot_fgi, plot = draw_plot(df_fgi, main_country, nyears, "Functioning Government Index", "Economist Intelligence Unit - processed by Our World in Data"), width = 6, height = 1.8, dpi = 200)
    # 
    # # Add plot to document
    # doc <- doc |>
    #   body_add_img(src = temp_plot_fgi, width = 6, height = 1.8, style = "Compact")
    # 
    # # Create and save the categories plot
    # temp_plot_fgi_cat <- tempfile(fileext = ".png")
    # 
    # main_country <- input$main_country
    # 
    # ggsave(temp_plot_fgi_cat, plot = draw_plot_categories_noval(df_fgi, main_country, fgi_label, fgi_min, fgi_max, fgi_color), width = 6, height = 0.5, dpi = 200)
    # 
    # # Add plot to document
    # doc <- doc |>
    #   body_add_img(src = temp_plot_fgi_cat, width = 6, height = 0.5, style = "Compact")
    # 
    # doc <- doc |>
    #   body_add_fpar(
    #     fpar(
    #       ftext("The categories shown are indicative (cut-off values are not official). The "),
    #       hyperlink_ftext(
    #         text = "Functioning Government Index",
    #         href = "https://ourworldindata.org/grapher/functioning-government-index-eiu",
    #         prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
    #       ),
    #       ftext(" ranges from 0 (least effective) to 10 (most effective)."),
    #       fp_p = fp_par(word_style = "Caption_Note")
    #     )
    #   ) 
    # 
    # Create and save the plot
    temp_plot_cpi <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_cpi$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_cpi, plot = draw_plot(df_cpi, main_country, nyears, "Corruption Perceptions Index", "Transparency International"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_cpi, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_cpi_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_cpi_cat, plot = draw_plot_categories_noval(df_cpi, main_country, cpi_label, cpi_min, cpi_max, cpi_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_cpi_cat, width = 6, height = 0.5, style = "Compact")
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Corruption Perceptions Index",
            href = "https://ourworldindata.org/grapher/ti-corruption-perception-index",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (most corrupt) to 100 (least corrupt)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) 
    # body_add_par("The categories shown are indicative (cut-off values are not official). The Corruption Perceptions Index ranges from 0 (most corrupt)
    #              to 100 (least corrupt).", style = "Caption_Note")
    
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
    
    ggsave(temp_plot_oda_gni, plot = draw_plot(df_oda_gni, main_country, nyears, "Net ODA received (% of GNI)", "OECD"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_oda_gni, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_oda_gni_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_oda_gni_cat, plot = draw_plot_categories_noval(df_oda_gni, main_country, oda_gni_label, oda_gni_min, oda_gni_max, oda_gni_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_oda_gni_cat, width = 6, height = 0.5, style = "Compact")
    
    
    doc <- doc |>
      body_add_par("The categories shown are indicative (cut-off values are not official).", style = "Caption_Note") |> 
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
    
    ggsave(temp_plot_ccsi, plot = draw_plot(df_ccsi, main_country, nyears, "Civil Society Participation Index", v_dem), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_ccsi, width = 6, height = 1.8, style = "Compact")
    
    # Create and save the categories plot
    temp_plot_ccsi_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_ccsi_cat, plot = draw_plot_categories_noval(df_ccsi, main_country, ccsi_label, ccsi_min, ccsi_max, ccsi_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_ccsi_cat, width = 6, height = 0.5, style = "Compact")
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). The "),
          hyperlink_ftext(
            text = "Civil Society Participation Index",
            href = "https://ourworldindata.org/grapher/civil-society-participation-index",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" ranges from 0 (least active) to 1 (most active)."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) 
    # body_add_par("The categories shown are indicative (cut-off values are not official). The Core Civil Society Index ranges from 0 (weak, repressed civil society)
    #              to 1 (robust, autonomous civil society).", style = "Caption_Note")
    
    
    # Create and save the B-READY plot
    temp_plot_bready <- tempfile(fileext = ".png")
    
    nyears <- length(unique(df_bready_resolution$year))
    main_country <- input$main_country
    
    ggsave(temp_plot_bready, plot = draw_plot(df_bready_resolution, main_country, nyears, "B-READY: Dispute Resolution", "B-READY"), width = 6, height = 1.8, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_bready, width = 6, height = 1.8, style = "Compact")
    
    
    # Create and save the categories plot
    temp_plot_bready_cat <- tempfile(fileext = ".png")
    
    main_country <- input$main_country
    
    ggsave(temp_plot_bready_cat, plot = draw_plot_categories_noval(df_bready_resolution, main_country, bready_label, bready_min, bready_max, bready_color), width = 6, height = 0.5, dpi = 200)
    
    # Add plot to document
    doc <- doc |>
      body_add_img(src = temp_plot_bready_cat, width = 6, height = 0.5, style = "Compact")
    
    
    doc <- doc |>
      body_add_fpar(
        fpar(
          ftext("The categories shown are indicative (cut-off values are not official). "),
          hyperlink_ftext(
            text = "B-READY: Dispute Resolution",
            href = "https://data.worldbank.org/indicator/IC.BRE.DR.OS",
            prop  = fp_text(color = "#0563C1", underlined = TRUE, font.size = 8)
          ),
          ftext(" measures efficiency and quality
                     of the resolution of commercial disputes based on three dimensions
                     (quality of regulations, public services and ease of resolving a commercial
                     dispute. The overall score ranges from 0 (worst)
                     to 100 (best). This is a new indicator and not yet available
                     for all countries. If the value is 0, no values are available."),
          fp_p = fp_par(word_style = "Caption_Note")
        )
      ) |> 
      # body_add_par("The B-READY: Dispute Resolution measures efficiency and quality
      #              of the resolution of commercial disputes based on three dimensions
      #              (quality of regulations, public services and ease of resolving a commercial
      #              dispute. The overall score ranges from 0 (worst)
      #              to 100 (best). This is a new indicator and not yet available
      #              for all countries.", style = "Caption_Note") |> 
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
      body_add_par("D) OPTIONAL", style = "heading 1") |>
      body_add_par("(in IC programme sectors)", style = "Non_Bullet_Instruction") |>
      body_add_par("Additional fields of observation", style = "heading 2") |>
      body_add_par("", style = "Normal") |> 
      body_add_par("Analysis", style = "heading 3") |> 
      text_input_field(placeholder = "Add text here") |> 
      body_add_fpar(value = comment_fun("Consequences for the programme operations", conseq_all), 
                    style = "heading 3") |>        
      text_input_field(placeholder = "Add text here")
    
    # Save document
    #tmp_docx <- tempfile(fileext = ".docx")
    print(doc, target = file)
    
    # collapse_heading_sections(
    #   tmp_docx,
    #   output_path = file,
    #   heading_styles = "Heading2blue"  # adjust if Step 1 shows a different ID
    # )
    # Clean up temp file
    # unlink(temp_plot_hdi)
  }
)