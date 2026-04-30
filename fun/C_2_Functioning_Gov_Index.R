

get_owid_c_2_fgi_data <- function(countries, years) {
  
  data_values <- read.csv("https://ourworldindata.org/grapher/functioning-government-index-eiu.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = funct_gov_eiu) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 
  
  return(data_values)
}

fgi_label = c("Very Low", "Low", "Medium", "High", "Very High")
fgi_min   = c(0,      2,         4,      6,      8)
fgi_max   = c(2,     4,         6,      8,      10)
fgi_color = c( "#823a53", "#ef7d00", "#f2a758", "#60b3b1", "#519795")