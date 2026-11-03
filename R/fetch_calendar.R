library(ical)
library(stringr)
library(dplyr)
library(lubridate)

CALENDAR_URL <- "https://calendar.google.com/calendar/ical/ppca.100%40gmail.com/public/basic.ics"

PHRASES <- c(
  "{coach} led a great paddle on {day_name}.",
  "{day_name}’s paddle was led by {coach}.",
  "{coach} took a happy group out on {day_name}.",
  "Paddle leader for {day_name} was {coach}.",
  "{coach} ran the {day_name} paddle.",
  "{coach} was in charge on {day_name}.",
  "Thanks to {coach} for leading the paddle on {day_name}.",
  "Our {day_name} paddle was led by {coach}.",
  "Big thanks to {coach} for taking the lead on {day_name}.",
  "The paddle on {day_name} was under the guidance of {coach}."
  )

get_calendar_data <- function(calendar_url = CALENDAR_URL) {
  calendar_data <- ical::ical_parse_df(calendar_url)
  calendar_data %>%
    mutate(
      start_datetime = with_tz(start, tzone = "Europe/London"),
      end_datetime = with_tz(end, tzone = "Europe/London"),
      duration = as.numeric(difftime(end_datetime, start_datetime, units = "mins")),
      date = as.Date(start_datetime)) %>%
    select(summary, date, start_datetime, end_datetime, duration)
}

process_calendar_events <- function(df, start_date, end_date) {

  processed <- df %>%
    # Filter date range upfront
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%

    # Extract cancellation status (case-insensitive)
    mutate(
      is_cancelled = str_detect(summary, regex("CANCELLED\\s*$", ignore_case = TRUE))
    ) %>%

    # Categorize events cleanly
    mutate(
      category = case_when(
        # Ignore administrative deadlines
        str_detect(summary, regex("Newsletter Editorial Deadline", ignore_case = TRUE)) ~ "ignore",

        # Specific distinct categories
        str_detect(summary, regex("Committee Meeting|Committee|PPCA AGM", ignore_case = TRUE)) ~ "committee",
        str_detect(summary, regex("White Water|WW Paddle", ignore_case = TRUE)) ~ "white_water",
        str_detect(summary, regex("Pool Session|Pool", ignore_case = TRUE)) ~ "pool",

        # Skills & Coaching (Coach, Coaching, Intro, Intro Plus)
        str_detect(summary, regex("^Coach|^Coaching|Intro Course|Intro Plus|Tides/Weather", ignore_case = TRUE)) ~ "skills",

        # Specific Rec Variations
        str_detect(summary, regex("Easter Paddle", ignore_case = TRUE)) ~ "rec_paddle",
        str_detect(summary, regex("Short Rec", ignore_case = TRUE)) ~ "rec_short",
        str_detect(summary, regex("Evening Rec", ignore_case = TRUE)) ~ "rec_paddle",
        str_detect(summary, regex("Sea Kayak", ignore_case = TRUE)) ~ "rec_sea",
        str_detect(summary, regex("Estuary Paddle|Estuary", ignore_case = TRUE)) ~ "rec_estuary",
        str_detect(summary, regex("Rec Paddle|Rec", ignore_case = TRUE)) ~ "rec_paddle",

        # Fallback / Unclassified
        TRUE ~ "UNCLASSIFIED"
      )
    ) %>%

    # Remove ignored items
    filter(category != "ignore") %>%

    # Parse coach name & clean summary
    mutate(
      # Extract text after last dash, removing trailing CANCELLED (case-insensitive)
      coach = summary %>%
        str_extract("-\\s*[^-\\n]*$") %>%
        str_remove("^-\\s*") %>%
        str_remove(regex("\\s*CANCELLED\\s*$", ignore_case = TRUE)) %>%
        str_trim(),

      # Clean event title by stripping "- Coach Name" and trailing "CANCELLED"
      clean_title = summary %>%
        str_remove("-\\s*[^-\\n]*$") %>%
        str_remove(regex("\\s*CANCELLED\\s*$", ignore_case = TRUE)) %>%
        str_remove(regex("\\bCoach\\s*\\d*(\\s*/\\s*\\d+)?", ignore_case = TRUE)) %>%
        str_replace(regex("Disco/Ex", ignore_case = TRUE), "Discover/Explore") %>%
        str_squish(),

      day_name = wday(start_datetime, label = TRUE, abbr = FALSE)
    ) %>%
    arrange(start_datetime)

  # --- 3. Safety Check: Flag Unclassified Events ---
  unclassified_events <- processed %>% filter(category == "UNCLASSIFIED")

  if (nrow(unclassified_events) > 0) {
    warning(
      sprintf(
        "\n[WARNING] Found %d unclassified event(s) in date range! Check summaries:\n%s",
        nrow(unclassified_events),
        paste("  -", unclassified_events$summary, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  return(processed)
}


add_pretty_date <- function(datetime) {
  day_name <- wday(datetime, label = TRUE, abbr = FALSE)
  day_num <- day(datetime)
  month_name <- month(datetime, label = TRUE, abbr = FALSE)

  suffix <- case_when(
    day_num %% 10 == 1 & day_num != 11 ~ "st",
    day_num %% 10 == 2 & day_num != 12 ~ "nd",
    day_num %% 10 == 3 & day_num != 13 ~ "rd",
    TRUE ~ "th"
  )

  glue("{day_name} {day_num}{suffix} {month_name}")
}


get_events <- function(calendar_df, run_date) {
  today_events <- calendar_df %>%
    filter(date == run_date)
  if (nrow(today_events) > 0) {
    processed_events <- today_events %>%
      mutate(pretty_date = add_pretty_date(run_date),
             phrase_template = sample(PHRASES, n(), replace = TRUE)) %>%
      rowwise() %>%
      mutate(evaluated_sentence = as.character(glue_data(pick(everything()), phrase_template)),
      evaluated_sentence = if_else(is_cancelled, "Cancelled", evaluated_sentence)) %>%
      ungroup() |>
      select(pretty_date, evaluated_sentence)
    } else {
    processed_events <- data.frame(pretty_date = add_pretty_date(start_datetime), evaluted_sentence = c("No scheduled paddle events found for today"))}
  return(processed_events |> knitr::kable(format = "markdown"))
}

