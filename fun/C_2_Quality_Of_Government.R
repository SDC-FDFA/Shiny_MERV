
get_qog_c_2_data <- function(countries, years) {
  data_values <- read.csv("https://raw.githubusercontent.com/SDC-FDFA/Shiny_MERV/refs/heads/main/rawdata/qog_std_ts_jan26.csv", sep = ";") |> 
    select(ccodealp,year, icrg_qog) |> 
    rename(value = icrg_qog) |> 
    rename(code = ccodealp) |> 
    filter(code %in% countries) |> 
    filter(year %in% years) 

  return(data_values)
}

qog_label = c("Fragile or Failed", "Weak", "Transitional", "High")
qog_min   = c(0,      0.3,         0.55,      0.8)
qog_max   = c(0.3,     0.55,         0.8,      1)
qog_color = c("#823a53", "#ef7d00", "#60b3b1", "#519795")