library(dplyr)
library(knitr)
library(glue)

source("R/fetch_calendar.R")
source("R/fetch_weather.R")
source("R/fetch_riverheight.R")
source("R/fetch_tide.R")

today <- Sys.Date()
wday_val <- as.POSIXlt(today)$wday
last_sunday   <- today - wday_val
next_saturday <- today + (6 - wday_val)

message("Running test report for target Saturday: ", next_saturday)
message("Fetching weather/calendar data starting from: ", last_sunday)

if (!dir.exists("reports")) {
  dir.create("reports")
}

filename_sea <- paste0("reports/", next_saturday, "-weather-report.md")
filename_river <- paste0("reports/", last_sunday, "-river-report.md")
calendar_df <- process_calendar_events(get_calendar_data(), "2026-04-01", "2026-12-31")

weather_df <- get_paddle_hourly_weather() |>
  filter(wday(datetime, label = TRUE, abbr = FALSE) == "Saturday" & hour %in% 10:15)
swell_df <- get_paddle_swell() |>
  filter(wday(datetime, label = TRUE, abbr = FALSE) == "Saturday" & hour %in% 10:15)
md_table <- hourly_paddle_table(weather_df, swell_df) |>
  knitr::kable(format = "markdown")

calendar_entries <- get_events(calendar_df, next_saturday)

plymouth_preds <- get_free_tide_predictions(50.3684, -4.1852, next_saturday)
bullets <- format_tide_report(plymouth_preds)

writeLines(c(
  paste("# Paddle Weather & Sea-State Log —", add_pretty_date(next_saturday)),
  "",
  "Official environmental data fetched from Defra and marine forecasts.",
  "",
  calendar_entries,
  "",
  md_table,
  "",
  "Tidal predictions",
  paste(bullets, collapse = "\n")), filename_sea)

message("Report successfully saved to: ", filename_sea)

df_readings <- get_river_level(property = "waterLevel", hours_back = 7*24)
plot_river(df_readings, last_sunday)

calendar_entries <- get_events(calendar_df, last_sunday)

writeLines(c(
  paste("# Paddle Weather & Sea-State Log —", last_sunday),
  "",
  calendar_entries,
  "",
  paste0("![River Level Chart](figures/", last_sunday, "-river-plot.png)"),
  ""
), filename_river)

message("Report successfully saved to: ", filename_river)
