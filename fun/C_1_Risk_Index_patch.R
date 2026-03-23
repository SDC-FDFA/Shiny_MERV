

## C_1_Risk Index (temporary solution lacking an API to get multiple years of data)
get_c_1_risk_index <- function(countries, years) {
  data_values <- read.delim(
    "https://raw.githubusercontent.com/SDC-FDFA/Shiny_MERV/refs/heads/main/rawdata/inform_risk_index.csv", 
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


