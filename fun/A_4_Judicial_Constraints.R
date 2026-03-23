

## Judicial Constraints on the Executive Index
get_wb_judic_data <- function(countries, years) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"
  
  # countries <- c("AFG", "PAK", "IRN")
  # years <- c(2018, 2020, 2022)
  # Build and execute the request
  
  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "VDEM_CORE",
      INDICATOR = "VDEM_CORE_V2X_JUCON",
      REF_AREA = countries,
      #   TIME_PERIOD = years,
      .multi = "comma"
    ) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  # response_data <- req_wb |>
  #   resp_body_json()
  # data_values <- response_data$value |>
  #       bind_rows()
  #     #  filter(REF_AREA %in% countries)
  #
  # data_values
  
  
  if (resp_status(req_wb) == 200) {
    response_data <- req_wb |>
      resp_body_json()
    data_values <- response_data$value |>
      bind_rows() |>
      filter(TIME_PERIOD %in% years) |>
      select(OBS_VALUE, REF_AREA, TIME_PERIOD) |>
      rename(
        value = OBS_VALUE,
        code = REF_AREA,
        year = TIME_PERIOD
      ) |>
      mutate(
        value = as.numeric(value),
        year = as.numeric(year)
      )
    return(data_values)
  } else {
    return(NULL)
  }
}
# 
# # CLASSIFICATION - WRONG
# classify_judic <- function(value) {
#   case_when(
#     value >= 0.8 ~ "Very high",
#     value < 0.8 & value >= 0.7 ~ "High",
#     value < 0.7 & value >= 0.55 ~ "Above average",
#     value < 0.55 & value >= 0.5 ~ "Average",
#     value < 0.5 & value >= 0.4 ~ "Below average",
#     value < 0.4 & value >= 0.3 ~ "Low",
#     value < 0.3 & value >= 0 ~ "Very low",
#     TRUE ~ "NA"
#   )
# }