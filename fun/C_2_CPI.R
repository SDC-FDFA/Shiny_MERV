
# INFORM Risk Index

get_c_2_cpi <- function(countries, years) {
  data_values <- read.csv("https://ourworldindata.org/grapher/ti-corruption-perception-index.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = cpi_score) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 
  
  return(data_values)
  
}

cpi_label = c("Very High", "High", "Moderate", "Clean", "Very Clean")
cpi_min   = c(0,      20,         40,      60,      80)
cpi_max   = c(20,     40,         60,      80,      100)
cpi_color = c("#823a53", "#ef7d00", "#f2a758", "#60b3b1", "#519795")
