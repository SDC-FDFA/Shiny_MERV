
# INFORM Severity Index

auth_token <- "30fa0c6777b5aa4fcfe79dc41280f25b54ac2e21"

library(httr)
library(jsonlite)
library(dplyr)

#request_url <- "https://api.acaps.org/api/v1/inform-severity-index/"
request_url_2 <- "https://api.acaps.org/api/v1/inform-severity-index/core-indicators/"


df <- data.frame()
last_request_time <- Sys.time()

while (TRUE) {
  while (as.numeric(Sys.time() - last_request_time) < 1) {
    Sys.sleep(0.1)
  }
  
  response <- httr::GET(
    request_url_2,
    add_headers(Authorization = paste("Token", auth_token),
                Accept = "application/json" )
  )
  last_request_time <- Sys.time()
  
  if (status_code(response) != 200) {
    stop("Data fetch failed [", status_code(response), "]: ", 
         content(response, "text"))
         }
  
  content_list <- fromJSON(content(response, "text", encoding = "UTF-8"))
  df_results <- content_list$results
  
  if (!is.null(df_results)) {
    df_results <- df_results %>%
      mutate(across(where(is.list), ~ sapply(., toString)))
    df <- bind_rows(df, df_results)
  }
  
  # ✅ "next" quoted so R treats it as a string, not a keyword
  if (!is.null(content_list[["next"]]) && nzchar(content_list[["next"]])) {
    request_url <- content_list[["next"]]
  } else {
    break
  }
}

countries <- c("AFG", "PAK", "IRN")

data_values <- df |>
  filter(iso3 %in% countries) |>
  select(crisis_name, iso3, "INFORM Severity Index") |>
  rename(
    value = `INFORM Severity Index`,
    code = iso3
  ) |>
  mutate(
    value = as.numeric(value),
  )
