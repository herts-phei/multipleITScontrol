# Load necessary libraries
library(tibble)
library(dplyr)

# Create a date range vector for Fridays only
date_range <- seq(as.Date("2025-03-03"), as.Date("2026-08-30"), by = "day")
fridays <- date_range[weekdays(date_range) == "Friday"]

# Create a tibble with the date range
tibble_data <- tibble(Date = fridays)

# Add time_var as a sequential time index
tibble_data <- tibble_data %>%
  mutate(time_var = row_number())

# Add group_var with 'treatment' and 'control' groups
tibble_data <- tibble_data %>%
  mutate(group_var = if_else(row_number() %% 2 == 0, 'control', 'treatment'))

# Add Period column using mutate and case_when
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2025-03-03") & Date <= as.Date("2025-08-31") ~ "Pre-intervention period",
    Date >= as.Date("2025-09-01") & Date <= as.Date("2026-03-01") ~ "Intervention 1) Reading Program",
    Date >= as.Date("2026-03-02") & Date <= as.Date("2026-08-30") ~ "Intervention 2) Peer Tutoring Sessions"
  ))

# Add outcome_var with pre-intervention points hovering around 82, first intervention around 90, and second intervention even higher
set.seed(42)
tibble_data <- tibble_data %>%
  mutate(score = case_when(
    Period == "Pre-intervention period" ~ rnorm(n(), mean = 82, sd = 2),
    Period == "Intervention 1) Reading Program" ~ rnorm(n(), mean = 90, sd = 5),
    Period == "Intervention 2) Peer Tutoring Sessions" ~ rnorm(n(), mean = 95, sd = 5)
  ))

# Define intervention_dates
intervention_dates <- c(as.Date("2025-09-01"), as.Date("2026-03-02"))

# Print the tibble
print(tibble_data)

transform_data(tibble_data, "time_var", "group_var", "score", c(as.Date("2025-09-01"), as.Date("2026-03-02")))
