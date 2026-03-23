
# INFORM Risk Index


app_identifier <- "UlN0dWRpbzptYXR0aWEuemFuYXp6aUBlZGEuYWRtaW4uY2g="

base_url <- paste0(
"https://hapi.humdata.org/api/v2/coordination-context/national-risk")

library(httr2)
library(dplyr)
response <- request(base_url) |>
  req_url_query(
    output_format = "json",
    app_identifier = app_identifier
  ) |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform()


response_data <- response |>
  resp_body_json()
data_values <- response_data$data |>
  bind_rows()



resp <- request("https://data.humdata.org/api/3/action/package_show") |> 
  req_url_query(id = "inform-risk-index-2021") |> 
  req_perform() |> 
  resp_body_json()


df <- openxlsx::read.xlsx("https://data.humdata.org/dataset/f5ec2ee7-8a1b-49b4-864b-70bdb582a022/resource/9640d1d5-83da-4969-aa6a-52ff57386d10/download/inform2024_trend_2015_2024_v70_all.xlsx")





