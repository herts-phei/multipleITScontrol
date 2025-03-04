# Load necessary libraries
library(tibble)
library(dplyr)

# Create a date range vector for Fridays only
date_range <- seq(as.Date("2025-03-03"), as.Date("2026-08-30"), by = "day")
fridays <- date_range[weekdays(date_range) == "Friday"]

# Create a tibble with the date range
tibble_data <- tibble(Date = rep(fridays, 2))

# Add group_var with 'treatment' and 'control' groups
tibble_data <- tibble_data %>%
  mutate(group_var = rep(c('treatment', 'control'), each = length(fridays)))

# Add Period column using mutate and case_when
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2025-03-03") & Date <= as.Date("2025-08-31") ~ "Pre-intervention period",
    Date >= as.Date("2025-09-01") & Date <= as.Date("2026-03-01") ~ "Intervention 1) Reading Program",
    Date >= as.Date("2026-03-02") & Date <= as.Date("2026-08-30") ~ "Intervention 2) Peer Tutoring Sessions"
  ))

# Add outcome_var with pre-intervention points hovering around 82, first intervention around 90, and second intervention even higher

tibble_data <- tibble_data %>%
  mutate(score = case_when(
    Period == "Pre-intervention period" & group_var == "treatment" ~ rnorm(n(), mean = 82, sd = 2),
    Period == "Intervention 1) Reading Program" & group_var == "treatment" ~ rnorm(n(), mean = 90, sd = 2),
    Period == "Intervention 2) Peer Tutoring Sessions" & group_var == "treatment" ~ rnorm(n(), mean = 95, sd = 2),
    group_var == "control" ~ rnorm(n(), mean = 82, sd = 2)
  ))

# Define intervention_dates
intervention_dates <- c(as.Date("2025-09-05"), as.Date("2026-03-06"))

wrong_dates <- c(as.Date("2025-09-01"), as.Date("2026-03-02"))


transform_data(df = tibble_data,
               time_var = "Date",
               group_var = "group_var",
               outcome_var =  "score",
               intervention_dates = intervention_dates)

