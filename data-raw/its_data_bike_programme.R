# Create a date range vector for Fridays only
date_range <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
mondays <- date_range[weekdays(date_range) == "Monday"]

# Create a tibble with the date range
tibble_data <- tibble(Date = rep(mondays, 2))

# Add group_var with 'treatment' and 'control' groups
tibble_data <- tibble_data %>%
  mutate(group_var = rep(c("treatment", "control"), each = length(mondays)))

# Add Period column using mutate and case_when
set.seed(48)
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2023-01-01") & Date <= as.Date("2023-04-02") ~ "Pre-intervention period",
    Date >= as.Date("2023-04-03") & Date <= as.Date("2023-06-04") ~ "Intervention 1) Smart-Lock Upgrade",
    Date >= as.Date("2023-06-05") & Date <= as.Date("2023-12-31") ~ "Intervention 2) Predictive Maintenance Algorithm"
  )) %>%
  mutate(
    score = case_when(
      Period == "Pre-intervention period" & group_var == "treatment" ~ rnorm(n(), mean = 35, sd = 1),
      Period == "Intervention 1) Smart-Lock Upgrade" & group_var == "treatment" ~ rnorm(n(), mean = 30, sd = 2),
      Period == "Intervention 2) Predictive Maintenance Algorithm" & group_var == "treatment" ~ rnorm(n(), mean = 20, sd = 1),
      group_var == "control" ~ rnorm(n(), mean = 35, sd = 1)
    )
  ) |>
  mutate(across(score, round, 1))

its_data_bike_programme <- tibble_data

usethis::use_data(its_data_bike_programme, overwrite = TRUE)
