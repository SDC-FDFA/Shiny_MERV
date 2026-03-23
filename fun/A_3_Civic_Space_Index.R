

## Civic Space Data
get_wb_civic_data <- function(countries) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"
  
  # countries <- c("AFG", "PAK", "IRN")
  # Build and execute the request
  
  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "WB_SSGD",
      INDICATOR = "WB_SSGD_CIVIC_SPACE_IDX"
    ) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  if (resp_status(req_wb) == 200) {
    response_data <- req_wb |> resp_body_json()
    data_values <- response_data$value |>
      bind_rows() |>
      filter(REF_AREA %in% countries) |>
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



# CLASSIFICATION
classify_civic <- function(value) {
  case_when(
    value >= 81 ~ "Open",
    value < 81 & value >= 61 ~ "Narrowed",
    value < 61 & value >= 41 ~ "Obstructed",
    value < 41 & value >= 21 ~ "Repressed",
    TRUE ~ "Closed"
  )
}