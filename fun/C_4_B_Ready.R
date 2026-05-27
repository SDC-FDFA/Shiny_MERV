wb_base_url <- "https://data360api.worldbank.org"
wb_endpoint <- "/data360/data"

countries <- c("AFG", "IRN")
years <- c(2018, 2020, 2022)
# Build and execute the request

req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
  req_url_query(
    DATABASE_ID = "WB_WDI",
    INDICATOR = "WB_WDI_IC_BRE_DR_OS",
    REF_AREA = countries,
    #   TIME_PERIOD = years,
    .multi = "comma"
  ) |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform()

response_data <- req_wb |>
  resp_body_json()
data_values <- response_data$value |>
  bind_rows()

data_values <- data.frame(value = 0,   code = countries[1],  year = years[1])

## C_4_B-Ready Commercial Dispute Resolution
get_wb_c_4_bready_resolution <- function(countries, years) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"
  
  # countries <- c("AFG", "PAK", "IRN")
  # years <- c(2018, 2020, 2022)
  # Build and execute the request
  
  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "WB_WDI",
      INDICATOR = "WB_WDI_IC_BRE_DR_OS",
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
      bind_rows() 
    
    if (nrow(data_values) == 0) {
      data_values <- data.frame(value = 0,   code = countries[1],  year = 1900)
      return(data_values)
    } else {
    
    data_values <- data_values |>
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
    }
  } else {
    return(NULL)
  }
}

bready_label = c("Very Weak", "Weak", "Moderate", "Strong", "Very Strong")
bready_min   = c(0,      35,         50,      65,      80)
bready_max   = c(35,     50,         65,      80,      100)
bready_color = c("#823a53", "#ef7d00", "#f2a758", "#60b3b1", "#519795")
