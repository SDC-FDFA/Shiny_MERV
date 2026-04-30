library(jsonlite)
library(dplyr)


# countries <- c("AFG", "PAK", "IRN")
# years <- c(2018, 2020, 2022)

# Fetch the data
get_owid_electoral_data <- function(countries, years) {
  
  data_values <- read.csv("https://ourworldindata.org/grapher/electoral-democracy-index.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = electdem_vdem__estimate_best) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 

  return(data_values)
}
