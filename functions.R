
# Country list (ISO3 codes)
country_list <- c(
  "Afghanistan" = "AFG",
  "Albania" = "ALB",
  "Algeria" = "DZA",
  "Armenia" = "ARM",
  "Bangladesh" = "BGD",
  "Benin" = "BEN",
  "Bosnia Herzegovina" = "BIH",
  "Brazil" = "BRA",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cambodia" = "KHM",
  "Chad" = "TCD",
  "China" = "CHN",
  "Colombia" = "COL",
  "Congo (DRC)" = "COD",
  "Egypt" = "EGY",
  "Georgia" = "GEO",
  "Ghana" = "GHA",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran" = "IRN",
  "Iraq" = "IRQ",
  "Kenya" = "KEN",
  "Kyrgyzstan" = "KGZ",
  "Laos" = "LAO",
  "Malaysia" = "MYS",
  "Mali" = "MLI",
  "Mexico" = "MEX",
  "Moldova" = "MDA",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Nepal" = "NPL",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "N. Macedonia" = "MKD",
  "Pakistan" = "PAK",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Russia" = "RUS",
  "Rwanda" = "RWA",
  "Saudi Arabia" = "SAU",
  "Serbia" = "SRB",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "Tajikistan" = "TJK",
  "Tanzania" = "TZA",
  "Thailand" = "THA",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Ukraine" = "UKR",
  "Uzbekistan" = "UZB",
  "Vietnam" = "VNM",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

country_tibble <- tibble(
  code = unname(country_list),
  country = names(country_list)
)


# API functions
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

# WB Data 360

## Civic Space Data
get_wb_deliberative_data <- function(countries, years) {
  wb_base_url <- "https://data360api.worldbank.org"
  wb_endpoint <- "/data360/data"

  # countries <- c("AFG", "PAK", "IRN")
  # years <- c(2018, 2020, 2022)
  # Build and execute the request

  req_wb <- request(paste0(wb_base_url, wb_endpoint)) |>
    req_url_query(
      DATABASE_ID = "VDEM_CORE",
      INDICATOR = "VDEM_CORE_V2X_DELIBDEM",
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
# Deliberative Democracy
# TBD

# Civic Space
classify_civic <- function(value) {
  case_when(
    value >= 81 ~ "Open",
    value < 81 & value >= 61 ~ "Narrowed",
    value < 61 & value >= 41 ~ "Obstructed",
    value < 41 & value >= 21 ~ "Repressed",
    TRUE ~ "Closed"
  )
}

# UNDP HDI Data
classify_hdi <- function(value) {
  case_when(
    value >= 0.8 ~ "Very high",
    value < 0.8 & value >= 0.7 ~ "High",
    value < 0.7 & value >= 0.55 ~ "Medium",
    TRUE ~ "Low"
  )
}



draw_plot <- function(x, main_c, nyears, full_title, full_caption) {
  #message("main_c: '", main_c, "'; countries: ", paste(unique(x$country), collapse = ", "))

  x <- x |>
  mutate(linewidth = if_else(code == main_c, 2, 0.5))  # Adjust the thickness as desired

  p <- ggplot(
    x,
    aes(
      x = year,
      y = value,
      color = country
    )
  ) +
    geom_line(aes(size = linewidth)) +
    scale_size_identity() +
    geom_label_repel(
      data = filter(x, year == max(year)),
      aes(label = paste0(country, ": ", value)),
      nudge_y = 0.03,
      size = 10 / ggplot2::.pt,
      show.legend = FALSE,
      direction = "y"
    ) +
    labs(
      title = str_wrap(full_title, width = 60),
      caption = paste0("Source: ", full_caption)
    ) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = nyears)) +
    scale_y_continuous(
      expand = expansion(c(0, 0.1)),
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      limits = c(min(x$value) * 0.6, max(x$value) * 1.1)
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      #panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(size = 8, family = "sans"),
      axis.text.y = element_text(size = 8, family = "sans"),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      text = element_text(size = 8, family = "sans"),
      strip.text = element_text(size = 10, face = "bold"),
      title = element_text(size = 10, family = "sans", face = "bold"),
      plot.caption = element_text(size = 7, family = "sans", face = "plain"),
      legend.position = "none",
      legend.title = element_blank(),
      #axis.title = element_blank(),
      legend.box = element_blank()
    )
  p
}
