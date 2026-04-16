
# INFORM Risk Index

get_c_2_cpi <- function(countries, years) {
  data_values <- read.csv("https://data.humdata.org/dataset/fb4adde0-93d5-4ff9-befc-4a6916c1181b/resource/a6a4e225-50bb-4abf-958c-c37e3c3b380b/download/global-cpi-all.csv") |> 
    select(iso3, year, score) |> 
    rename(code = iso3,
           value = score) |> 
    filter(year %in% years) |> 
    filter(code %in% countries) |> 
    mutate(
      value = as.numeric(value),
      year = as.numeric(year)
    )
  return(data_values)
  
}

cpi_label = c("Very High", "High", "Moderate", "Clean", "Very Clean")
cpi_min   = c(0,      20,         40,      60,      80)
cpi_max   = c(20,     40,         60,      80,      100)
cpi_color = c("#823a53", "#ef7d00", "#f2a758", "#60b3b1", "#519795")
