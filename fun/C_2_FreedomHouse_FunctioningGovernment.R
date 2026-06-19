# countries <- c("AFG", "BGD", "BGR")
# years <- c(2018, 2020, 2022)
# 
# test <- get_fhfgi_c_2_data(countries, years)
  
get_fhfgi_c_2_data <- function(countries, years) {
  data_values <- read.csv("https://raw.githubusercontent.com/SDC-FDFA/Shiny_MERV/refs/heads/main/rawdata/qog_std_ts_jan26.csv", sep = ";") |> 
    select(ccodealp,year, fh_fog) |> 
    rename(value = fh_fog) |> 
    rename(code = ccodealp) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 
  
  return(data_values)
}

fhfgi_label = c("Dysfunctional", "Flawed", "Moderate", "Robust")
fhfgi_min   = c(0,      3,         6,      9)
fhfgi_max   = c(3,     6,        9,      12)
fhfgi_color = c("#823a53", "#ef7d00", "#60b3b1", "#519795")