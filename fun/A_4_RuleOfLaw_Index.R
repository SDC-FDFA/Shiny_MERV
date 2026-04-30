

get_owid_a_4_rol_data <- function(countries, years) {
  
  data_values <- read.csv("https://ourworldindata.org/grapher/rule-of-law-index.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = rule_of_law_vdem__estimate_best) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 
  # mutate(case_when(
  #   value == 0 ~ "Closed Autocracy",
  #   value == 1 ~ "Electoral Autocracy",
  #   value == 2 ~ "Electoral Democracy",
  #   value == 3 ~ "Liberal Democracy"
  #   TRUE ~ NA
  # ))
  
  return(data_values)
}

rol_label = c("Very Weak", "Weak", "Moderate", "Strong", "Very Strong")
rol_min   = c(0,      0.2,         0.4,      0.6,      0.8)
rol_max   = c(0.2,     0.4,         0.6,      0.8,      1)
rol_color = c( "#823a53", "#ef7d00", "#f2a758", "#60b3b1", "#519795")