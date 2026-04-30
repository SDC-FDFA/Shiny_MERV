# library(jsonlite)
# library(dplyr)


# countries <- c("AFG", "PAK", "IRN")
# years <- c(2018, 2020, 2022)

# Fetch the data
get_owid_regime_data <- function(countries, years) {
  
  data_values <- read.csv("https://ourworldindata.org/grapher/political-regime.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = regime_row_owid) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) |> 
    mutate(value = value + 0.5)
  
    # mutate(case_when(
    #   value == 0 ~ "Closed Autocracy",
    #   value == 1 ~ "Electoral Autocracy",
    #   value == 2 ~ "Electoral Democracy",
    #   value == 3 ~ "Liberal Democracy"
    #   TRUE ~ NA
    # ))
  
  return(data_values)
}


regime_label = c("Closed Autocracies", "Electoral Autocracies", "Electoral Democracies", "Liberal Democracies")
regime_min   = c(0,      1,         2,      3)
regime_max   = c(0.99,     1.99,         2.99,      3.99)
regime_color = c("#823a53", "#ef7d00", "#60b3b1", "#519795")


