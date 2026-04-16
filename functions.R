
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
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cambodia" = "KHM",
  "Chad" = "TCD",
  "China" = "CHN",
  "Colombia" = "COL",
  "Congo (DRC)" = "COD",
  "Egypt" = "EGY",
  "Georgia" = "GEO",
  "Ghana" = "GHA",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran" = "IRN",
  "Iraq" = "IRQ",
  "Kenya" = "KEN",
  "Kyrgyzstan" = "KGZ",
  "Laos" = "LAO",
  "Malaysia" = "MYS",
  "Mali" = "MLI",
  "Mexico" = "MEX",
  "Moldova" = "MDA",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Nepal" = "NPL",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "N. Macedonia" = "MKD",
  "Pakistan" = "PAK",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Russia" = "RUS",
  "Rwanda" = "RWA",
  "Saudi Arabia" = "SAU",
  "Serbia" = "SRB",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "Tajikistan" = "TJK",
  "Tanzania" = "TZA",
  "Thailand" = "THA",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Ukraine" = "UKR",
  "Uzbekistan" = "UZB",
  "Vietnam" = "VNM",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

country_tibble <- tibble(
  code = unname(country_list),
  country = names(country_list)
)

# API Fun
source("fun/A_2_Deliberative_Dem_Index.R")
source("fun/A_3_Civic_Space_Index.R")
source("fun/A_3_Civil_Liberties.R")
source("fun/A_4_Judicial_Constraints.R")
source("fun/B_1_GDP_Growth.R")
source("fun/B_2_HDI.R")
source("fun/B_3_Climate_Change.R")
source("fun/C_1_Risk_Index_patch.R")
source("fun/C_2_CPI.R")
source("fun/C_2_Government_Effectiveness.R")
source("fun/C_2_Control_Corruption.R")
source("fun/C_3_ODA_GNI.R")
source("fun/C_4_CCSI.R")
source("fun/C_4_B_Ready.R")


# One Plot to Rule Them ALl
source("fun/Plot.R")
source("fun/Plot_Categories.R")



