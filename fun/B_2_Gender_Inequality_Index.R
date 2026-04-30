

get_owid_b_2_gender_ineq_data <- function(countries, years) {
  
  data_values <- read.csv("https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report.csv?v=1&csvType=full&useColumnShortNames=true") |> 
    rename(value = gii) |> 
    select(year, code, value) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 

  
  return(data_values)
}

gii_label = c("Very Low", "Low", "Medium", "High", "Very High")
gii_min   = c(0,      0.09,         0.27,      0.45,      0.72)
gii_max   = c(0.09,     0.27,         0.45,      0.72,      1)
gii_color = c( "#519795", "#60b3b1", "#f2a758", "#ef7d00","#823a53")