library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
library(glue)

get_free_tide_predictions <- function(lat, lon, target_date) {
  base_url <- "https://api.openwaters.io/tides/extremes"

  resp <- request(base_url) %>%
    req_url_query(
      latitude = lat,
      longitude = lon,
      start = paste0(target_date, "T00:00:00Z"),
      end = paste0(as.Date(target_date) + 1, "T00:00:00Z")
    ) %>%
    req_perform()

  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(parsed$extremes) || length(parsed$extremes) == 0) {
    warning("No tide predictions returned for ", target_date)
    return(NULL)
  }
  tides_df <- parsed$extremes %>%
    mutate(
      dateTime = ymd_hms(time),
      tide_type = label
    ) %>%
    select(dateTime, level, tide_type)

  return(tides_df)
}



format_tide_report <- function(tides_df) {
  if (is.null(tides_df) || nrow(tides_df) == 0) return("No tide data available.")

  formatted <- tides_df %>%
    mutate(
      # Extract just the HH:MM time and convert dateTime to proper POSIXct if needed
      time_str = format(as.POSIXct(dateTime), "%H:%M"),
      # Clean up height formatting (e.g., rounded to 1 decimal place with 'm' unit)
      height_str = sprintf("%.1fm", level),
      # Capitalize/ensure proper labeling
      tide_label = str_to_title(tide_type)
    )

  # Option A: Generate clean markdown bullet points
  bullet_lines <- formatted %>%
    rowwise() %>%
    mutate(bullet = glue("* **{time_str}** — {tide_label} ({height_str})")) %>%
    pull(bullet)

  return(bullet_lines)
}

# Example usage:
# tide_bullets <- format_tide_report(plymouth_preds)
# paste(tide_bullets, collapse = "\n")


