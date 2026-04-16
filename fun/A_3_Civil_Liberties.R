
## A_3_Political Civil Liberties Index
get_wb_a_3_civil_lib_data <- function(countries, years) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"
  
  # countries <- c("AFG", "PAK", "IRN")
  # years <- c(2018, 2020, 2022)
  # Build and execute the request
  
  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "VDEM_CORE",
      INDICATOR = "VDEM_CORE_V2X_CLPOL",
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

civil_lib_label = c("Very Low", "Low", "Moderate", "High", "Very High")
civil_lib_min   = c(0,      0.2,         0.4,      0.6,      0.8)
civil_lib_max   = c(0.2,     0.4,         0.6,      0.8,      1)
civil_lib_color = c("#823a53", "#ef7d00",  "#f2a758", "#60b3b1", "#519795")



