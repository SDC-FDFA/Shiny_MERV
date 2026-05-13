panel_about <- nav_panel(
  title = "Indicators",
  icon = icon("circle-info"),
  # your documentation content here
  div(class = "section-header", "A) Political System"),
  hr(),
  div(class = "plot-card-header", "1) International political context"),
  tags$p(
    "The ",
    tags$a("Fragile States Index",
           href = "https://fragilestatesindex.org/global-data/",
           target = "_blank"),
    " ranges from 0 (least fragile) to 120 (most fragile). Published by the The Fund for Peace."
  ),
  hr(),
  div(class = "plot-card-header", "2) Domestic Political Stability"),
  tags$p(
    "The ",
    tags$a("Political Stability Index",
           href = "https://data360.worldbank.org/en/indicator/GOV_WGI_PV",
           target = "_blank"),
    " Governance score ranges from 0 (very low stability) to 100 (very high stability)."
  ),
  hr(),
  # tags$p(
  #   "The ",
  #   tags$a("Electoral Democracy Index",
  #          href = "https://ourworldindata.org/grapher/electoral-democracy-index",
  #          target = "_blank"),
  #   " ranges from 0 (least democratic) to 1 (most democratic). The same source also provides information on ",
  #   tags$a("Regimes of the World",
  #          href = "https://ourworldindata.org/regimes-of-the-world-data",
  #          target = "_blank"),
  #   ". Classifications as used by ",
  #   tags$a("V-DEM",
  #          href = "https://v-dem.net/",
  #          target = "_blank"),
  #   "."
  # ),
  # hr(),
  div(class = "plot-card-header", "3) Civic and political rights, voice and media"),
  tags$p(
    "The ",
    tags$a(
      "Political Civil Liberties Index",
      href = "https://ourworldindata.org/grapher/political-civil-liberties-index",
      target = "_blank"
    ),
    " ranges from 0 (least liberties)
                     to 1 (most liberties)."
  ),
  hr(),
  div(class = "plot-card-header", "4) Rule of law, independence of justice, division of power"),
  tags$p(
    "The ",
    tags$a(
      "Rule of Law Index",
      href = "https://ourworldindata.org/grapher/rule-of-law-index",
      target = "_blank"
    ),
    " ranges from 0 (least rule-based)
                     to 1 (most rule-based)."
    
  ),
  hr(),
  div(class = "section-header", "B) Development baselines"),
  hr(),
  div(class = "plot-card-header", "1) GDP Growth"),
  tags$p(
    "GDP growth as published by the ",
    tags$a(
      "World Bank",
      href = "https://data360.worldbank.org/en/indicator/WB_WDI_NY_GDP_MKTP_KD_ZG",
      target = "_blank"
    ),
    "."
  ),
  tags$p(
    "Income calculated as GNI per capita (current US$) as published by the ",
    tags$a(
      "World Bank",
      href = "https://data360.worldbank.org/en/indicator/WB_WDI_NY_GNP_PCAP_CD",
      target = "_blank"
    ),
    "."
  ),
  hr(),
  div(class = "plot-card-header", "2) Human capital, poverty and inequalities"),
  tags$p(
    "The ",
    tags$a(
      "Gender Inequality Index",
      href = "https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report",
      target = "_blank"
    ),
    " covers the dimensions of reproductive health, empowerment and economic status. It ranges from 0 (very low inequality) to 10 (very high inequality)."
  ),
  hr(),
  div(class = "plot-card-header", "3) Climate & environment risks"),
  tags$p(
    "The ",
    tags$a(
      "INFORM Climate Change",
      href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Climate-Change",
      target = "_blank"
    ),
    " is essentially a future projection of the ",
    tags$a(
      "INFORM Risk Index",
      href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk",
      target = "_blank"
    ),
    ". It ranges from 0 (very low risk) to 10 (very high risk)."
  ),
  hr(),
  div(class = "section-header", "C) Domestic partner context"),
  hr(),
  div(class = "plot-card-header", "1) Operational space"),
  tags$p(
    "The ",
    tags$a(
      "INFORM Risk Index",
      href = "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk",
      target = "_blank"
    ),
    " is a global, open-source risk assessment for humanitarian crises and disasters. It ranges from 0 (very low risk) to 10 (very high risk)."
  ),
  hr(),
  div(class = "plot-card-header", "2) Government effectiveness and control of corruption"),
  tags$p(
    "The ",
    tags$a(
      "Functioning Government Index",
      href = "https://ourworldindata.org/grapher/functioning-government-index-eiu",
      target = "_blank"
    ),
    " ranges from 0 (least effective) to 10 (most effective)."
  ),
  tags$p(
    "The ",
    tags$a(
      "Corruption Perceptions Index",
      href = "https://ourworldindata.org/grapher/ti-corruption-perception-index",
      target = "_blank"
    ),
    " ranges from 0 (most corrupt) to 100 (least corrupt)."
  ),
  hr(),
  div(class = "plot-card-header", "3) ODA as percent of recipient GNI"),
  tags$p(
    "Net ODA received as % of GNI, as published by the ",
    tags$a(
      "World Bank",
      href = "https://data.worldbank.org/indicator/DT.ODA.ODAT.GN.ZS",
      target = "_blank"
    ),
    "."
  ),
  hr(),
  div(class = "plot-card-header", "4) Non-state actors and private sector"),
  tags$p(
    "The ",
    tags$a(
      "Civil Society Participation Index",
      href = "https://ourworldindata.org/grapher/civil-society-participation-index",
      target = "_blank"
    ),
    " ranges from 0 (least active) to 1 (most active)."
  ),
  tags$p(
    "The ",
    tags$a(
      "B-READY: Dispute Resolution",
      href = "https://data.worldbank.org/indicator/IC.BRE.DR.OS",
      target = "_blank"
    ),
    " measures efficiency and quality
                     of the resolution of commercial disputes based on three dimensions
                     (quality of regulations, public services and ease of resolving a commercial
                     dispute. The overall score ranges from 0 (worst)
                     to 100 (best). This is a new indicator and not yet available
                     for all countries."
  )
  
)