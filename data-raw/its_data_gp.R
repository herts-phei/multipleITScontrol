# Create a date range vector for Fridays only
date_range <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
mondays <- date_range[weekdays(date_range) == "Monday"]

# Create a tibble with the date range
tibble_data <- tibble(Date = rep(mondays, 2))

# Add group_var with 'treatment' and 'control' groups
tibble_data <- tibble_data %>%
  mutate(group_var = rep(c("treatment", "control"), each = length(mondays)))

# Add Period column using mutate and case_when
set.seed(919)
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2022-01-01") & Date <= as.Date("2022-04-03") ~ "Pre-intervention period",
    Date >= as.Date("2022-04-04") & Date <= as.Date("2022-06-05") ~ "Intervention 1) Low-Intensity Interventions",
    Date >= as.Date("2022-06-06") & Date <= as.Date("2022-12-31") ~ "Intervention 2) High-Intensity Interventions"
  )) %>%
  mutate(score = case_when(
    Period == "Pre-intervention period" & group_var == "treatment" ~ rnorm(n(), 15, 0.1) + 0.004 * as.numeric(Date - as.Date("2022-04-04")),
    Period == "Intervention 1) Low-Intensity Interventions" & group_var == "treatment" ~ rnorm(n(), 15, 0.1) + 0.05 * as.numeric(Date - as.Date("2022-04-04")),
    Period == "Intervention 2) High-Intensity Interventions" & group_var == "treatment" ~ rnorm(n(), 18, 0.1) + 0.03 * as.numeric(Date - as.Date("2022-06-06")) * 0.015,
    group_var == "control" ~ rnorm(n(), 15, 0.1) + 0.0008 * as.numeric(Date - as.Date("2022-04-04"))
  )) |>
  mutate(across(score, round, 1))

its_data_gp <- tibble_data

usethis::use_data(its_data_gp, overwrite = TRUE)

#####

#
# tibble_data |> ggplot(aes(Date, score, group = group_var)) +
#   geom_point() +
#   geom_line()
