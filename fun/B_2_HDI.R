# API UNDP
hdr_api_key <- config::get("hdr_api")

if (!nzchar(hdr_api_key)) {
  stop("HDR API key not configured")
}

get_hdr_data <- function(country, year, indicator, api_key) {
  response <- request("https://hdrdata.org/api/CompositeIndices/query") |>
    req_url_query(
      apikey = hdr_api_key,
      countryOrAggregation = country,
      year = year,
      indicator = indicator
    ) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  
  if (resp_status(response) == 200) {
    return(resp_body_json(response))
  } else {
    return(NULL)
  }
}

get_multiple_hdr <- function(countries, years, indicators = NULL, api_key) {
  results <- list()
  
  for (country in countries) {
    for (year in years) {
      if (!is.null(indicators)) {
        for (indicator in indicators) {
          key <- paste(country, year, indicator, sep = "_")
          
          response <- request("https://hdrdata.org/api/CompositeIndices/query") |>
            req_url_query(
              apikey = hdr_api_key,
              countryOrAggregation = country,
              year = year,
              indicator = indicator
            ) |>
            req_error(is_error = \(resp) FALSE) |>
            req_perform()
          
          if (resp_status(response) == 200) {
            results[[key]] <- resp_body_json(response)
          }
          
          Sys.sleep(0.5)
        }
      } else {
        key <- paste(country, year, sep = "_")
        results[[key]] <- get_hdr_data(country, year, "hdi", api_key)
        Sys.sleep(0.5)
      }
    }
  }
  
  return(results)
}

# CLASSIFICATION
classify_hdi <- function(value) {
  case_when(
    value >= 0.8 ~ "Very high",
    value < 0.8 & value >= 0.7 ~ "High",
    value < 0.7 & value >= 0.55 ~ "Medium",
    TRUE ~ "Low"
  )
}

hdi_label = c("Low", "Medium", "High", "Very High")
hdi_min   = c(0,      0.55,         0.7,      0.8)
hdi_max   = c(0.55,     0.7,         0.8,      1)
hdi_color = c("#823a53", "#ef7d00", "#60b3b1", "#519795")
