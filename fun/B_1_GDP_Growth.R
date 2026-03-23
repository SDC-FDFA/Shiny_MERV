

## GDP Growth
get_wb_b_1_gdp_growth <- function(countries, years) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"
  
  # countries <- c("AFG", "PAK", "IRN")
  # years <- c(2018, 2020, 2022)
  # Build and execute the request
  
  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "WB_WDI",
      INDICATOR = "WB_WDI_NY_GDP_MKTP_KD_ZG",
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
