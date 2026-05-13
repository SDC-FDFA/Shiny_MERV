

## A_1_Fragile States Index Index (temporary solution lacking an API)
get_a_1_fs_index <- function(countries, years) {
  data_values <- read.delim(
    "https://raw.githubusercontent.com/SDC-FDFA/Shiny_MERV/refs/heads/main/rawdata/FSI.csv", 
    sep = ";") |> 
    filter(year %in% years) |> 
    filter(code %in% countries) |> 
    mutate(
      value = as.numeric(value),
      year = as.numeric(year)
    )
  return(data_values)
  
}


# countries <- c("AFG", "PAK", "IRN")
# years <- c(2018, 2020, 2022)
# CLASSIFICATION

fsi_label = c("Sustainable", "Stable", "Warning", "Alert")
fsi_min   = c(0,      30,         60,      90)
fsi_max   = c(30,     60,         90,      120)
fsi_color = c("#519795", "#60b3b1", "#ef7d00", "#823a53")

