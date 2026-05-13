
# Country list (ISO3 codes)
country_list <- c(
  "Afghanistan" = "AFG",
  "Albania" = "ALB",
  "Algeria" = "DZA",
  "Armenia" = "ARM",
  "Bangladesh" = "BGD",
  "Benin" = "BEN",
  "Bosnia Herzegovina" = "BIH",
  "Brazil" = "BRA",
  "Bulgaria" = "BGR",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cambodia" = "KHM",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "China" = "CHN",
  "Colombia" = "COL",
  "Congo (DRC)" = "COD",
  "Croatia" = "HRV",
  "Cyprus" = "CYP",
  "Czechia" = "CZE",
  "Egypt" = "EGY",
  "Estonia" = "EST",
  "Georgia" = "GEO",
  "Ghana" = "GHA",
  "Greece" = "GRC",
  "Haiti" = "HTI",
  "Hungary" = "HUN",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran" = "IRN",
  "Iraq" = "IRQ",
  "Jordan" = "JOR",
  "Kenya" = "KEN",
  "Kosovo" = "XKX",
  "Kyrgyzstan" = "KGZ",
  "Laos" = "LAO",
  "Latvia" = "LVA",
  "Lebanon" = "LBN",
  "Libya" = "LBY",
  "Lithuania" = "LTU",
  "Malaysia" = "MYS",
  "Mali" = "MLI",
  "Malta" = "MLT",
  "Mexico" = "MEX",
  "Moldova" = "MDA",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Nepal" = "NPL",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "N. Macedonia" = "MKD",
  "Occupied Palestinian Territories" = "PSE",
  "Pakistan" = "PAK",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Poland" = "POL",
  "Romania" = "ROU",
  "Russia" = "RUS",
  "Rwanda" = "RWA",
  "Saudi Arabia" = "SAU",
  "Serbia" = "SRB",
  "Slovakia" = "SVK",
  "Slovenia" = "SVN",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Sri Lanka" = "LKA",
  "Sudan" = "SDN",
  "Syria" = "SYR",
  "Switzerland" = "CHE",
  "Tajikistan" = "TJK",
  "Tanzania" = "TZA",
  "Thailand" = "THA",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Ukraine" = "UKR",
  "Uzbekistan" = "UZB",
  "Venezuela" = "VEN",
  "Vietnam" = "VNM",
  "Yemen" = "YEM",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

country_tibble <- tibble(
  code = unname(country_list),
  country = names(country_list)
)

# API Funs
source("fun/A_1_Fragile_States_Index.R")
source("fun/A_2_Political_Stability_Index.R")
source("fun/A_2_Deliberative_Dem_Index.R")
source("fun/A_2_Electoral_Dem_Index.R")
source("fun/A_2_Regime_Type.R")
source("fun/A_3_Civic_Space_Index.R")
source("fun/A_3_Civil_Liberties.R")
source("fun/A_4_Judicial_Constraints.R")
source("fun/A_4_RuleOfLaw_Index.R")
source("fun/B_1_GDP_Growth.R")
source("fun/B_1_Income_Classification.R")
source("fun/B_2_HDI.R")
source("fun/B_2_Gender_Inequality_Index.R")
source("fun/B_3_Climate_Change.R")
source("fun/C_1_Risk_Index.R")
source("fun/C_2_CPI.R")
source("fun/C_2_Functioning_Gov_Index.R")
source("fun/C_2_Government_Effectiveness.R")
source("fun/C_2_Control_Corruption.R")
source("fun/C_3_ODA_GNI.R")
source("fun/C_4_CCSI.R")
source("fun/C_4_B_Ready.R")


# Plotting
source("fun/Plot.R")
source("fun/Plot_Categories.R")



