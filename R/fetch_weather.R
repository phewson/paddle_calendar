library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)

# 1. Fetch Open-Meteo Weather (e.g., Plymouth / Sound coords: 50.3656, -4.1423)
get_paddle_weather <- function(lat = 50.3656, lon = -4.1423) {
  resp <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      daily = "temperature_2m_max,wind_speed_10m_max,wind_direction_10m_dominant,precipitation_sum",
      timezone = "Europe/London"
    ) %>%
    req_perform() %>%
    resp_body_json()

  # Parse daily forecast into a clean data frame
  tibble(
    date = as.Date(unlist(resp$daily$time)),
    temp_max_c = unlist(resp$daily$temperature_2m_max),
    wind_max_mph = round(unlist(resp$daily$wind_speed_10m_max) * 0.621371, 1), # km/h to mph
    wind_dir_deg = unlist(resp$daily$wind_direction_10m_dominant),
    rain_mm = unlist(resp$daily$precipitation_sum)
  )
}


get_paddle_hourly_weather <- function(lat = 50.3656, lon = -4.1423) {
  resp <- request("https://api.open-meteo.com/v1/forecast") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      # Request specific hourly metrics
      hourly = "temperature_2m,precipitation,wind_speed_10m,wind_direction_10m,wind_gusts_10m",
      timezone = "Europe/London"
    ) %>%
    req_perform() %>%
    resp_body_json()

  # Parse the hourly lists into a clean data frame
  tibble(
    datetime = ymd_hm(unlist(resp$hourly$time), tz = "Europe/London"),
    date = as.Date(datetime),
    hour = hour(datetime),
    temp_c = unlist(resp$hourly$temperature_2m),
    rain_mm = unlist(resp$hourly$precipitation),
    wind_mph = round(unlist(resp$hourly$wind_speed_10m) * 0.621371, 1), # km/h to mph
    wind_gust_mph = round(unlist(resp$hourly$wind_gusts_10m) * 0.621371, 1),
    wind_dir_deg = unlist(resp$hourly$wind_direction_10m)
  )
}

get_paddle_swell <- function(lat = 50.35, lon = -4.15) {
  resp <- request("https://marine-api.open-meteo.com/v1/marine") %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      hourly = "swell_wave_height,swell_wave_direction,swell_wave_period,wave_height",
      timezone = "Europe/London"
    ) %>%
    req_perform() %>%
    resp_body_json()

  # Parse hourly marine data into a clean data frame
  tibble(
    datetime = ymd_hm(unlist(resp$hourly$time), tz = "Europe/London"),
    date = as.Date(datetime),
    hour = hour(datetime),
    swell_height_m = unlist(resp$hourly$swell_wave_height),
    swell_dir_deg  = unlist(resp$hourly$swell_wave_direction),
    swell_period_s = unlist(resp$hourly$swell_wave_period),
    total_wave_m   = unlist(resp$hourly$wave_height)
  )
}


deg_to_cardinal <- function(degrees) {
  # 16-point compass sectors (22.5 degrees each)
  cards <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
             "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

  # Handle NA values gracefully
  ifelse(is.na(degrees), NA, {
    val <- floor((degrees + 11.25) / 22.5)
    cards[(val %% 16) + 1]
  })
}


hourly_paddle_table <- function(weather_df, swell_df) {
  combined_paddle_data <- weather_df |>
    inner_join(swell_df, by = c("date", "hour", "datetime")) |>
    mutate(
      Time = sprintf("%02d:00", hour),
      wind_dir_cardinal  = deg_to_cardinal(wind_dir_deg),
      swell_dir_cardinal = deg_to_cardinal(swell_dir_deg),
      temp_c        = round(temp_c, 1),
      wind_mph      = round(wind_mph, 1),
      wind_gust_mph = round(wind_gust_mph, 1),
      swell_height_m = round(swell_height_m, 1),
      total_wave_m  = round(total_wave_m, 1),
      swell_period_s  = round(swell_period_s, 1)) |>
  select(
    datetime, date, Time,
    temp_c, rain_mm,
    wind_mph, wind_gust_mph, wind_dir_cardinal,
    total_wave_m, swell_height_m, swell_period_s, swell_dir_cardinal
  )
  return(combined_paddle_data)
}
