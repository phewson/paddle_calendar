library(httr2)
library(dplyr)
library(tibble)
library(ggplot2)
STATION_ID <- "fd8ea26c-8052-48c5-a1bb-bbd5ebbbb3d3"

get_river_level <- function(station_guid = STATION_ID, property = "waterLevel", hours_back = 24) {
  url <- "https://environment.data.gov.uk/hydrology/data/readings"

  # Calculate ISO8601 timestamps for your time window
  min_time <- format(Sys.time() - (hours_back * 3600), "%Y-%m-%d")

  req <- request(url) |>
    req_url_query(
      station = station_guid,
      observedProperty = property,
      `min-date` = min_time) |>
    req_headers(Accept = "application/json")

  resp <- req |> req_perform()
  parsed <- resp |> resp_body_json(simplifyVector = TRUE)

  if (!is.null(parsed$items) && length(parsed$items) > 0) {
    df <- as_tibble(parsed$items)

    if ("dateTime" %in% names(df)) {
      df <- df |>
        mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    }

    return(df)
  } else {
    message("No readings found for the specified time window.")
    return(tibble())
  }
}



plot_river <- function(df_readings, last_sunday) {
  rect_start <- as.POSIXct(paste(last_sunday, "09:00:00"), tz = "UTC")
  rect_end   <- as.POSIXct(paste(last_sunday, "12:00:00"), tz = "UTC")
  max_flow <- max(1, max(df_readings$value))
  last_sunday <- as.Date(last_sunday)
  start_window_date <- last_sunday - 3
  p <- df_readings %>%
    filter(as.Date(date) >= start_window_date & as.Date(date) <= last_sunday) %>%
    ggplot(aes(x = dateTime, y = value)) +
    ylim(0.2, max_flow) +
    annotate("rect", xmin = rect_start, xmax = rect_end, ymin = -Inf, ymax = Inf,
             fill = "orange", alpha = 0.3) +
    geom_line(color = "#1f78b4", linewidth = 0.8) +
    labs(title = "River Water Level — Last 3 Days",
         subtitle = paste("Highlighted paddle window:", last_sunday, "09:00–12:00 UTC"),
          x = "Time (UTC)", y = "Water Level (m)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  if (!dir.exists("reports/figures")) {
  dir.create("reports/figures", recursive = TRUE)
}
  plot_filename <- paste0("reports/figures/", last_sunday, "-river-plot.png")
  ggsave(plot_filename, plot = p, width = 7, height = 4, dpi = 300)
}
