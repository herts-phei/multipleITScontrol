# Load necessary libraries
library(tibble)
library(dplyr)
devtools::install()
library(multipleITScontrol)

# Create a date range vector for Fridays only
date_range <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")
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
    Date >= as.Date("2022-01-01") & Date <= as.Date("2022-04-03") ~ "Pre-intervention period",
    Date >= as.Date("2022-04-04") & Date <= as.Date("2022-06-05") ~ "Intervention 1) Low-Intensity Interventions",
    Date >= as.Date("2022-06-06") & Date <= as.Date("2022-12-31") ~ "Intervention 2) High-Intensity Interventions"
  ))

# Add outcome_var with pre-intervention points hovering around 82, first intervention around 90, and second intervention even higher

tibble_data <- tibble_data %>%
  mutate(score = case_when(
    Period == "Pre-intervention period" & group_var == "treatment" ~ 20 + 0.1 * as.numeric(Date - min(Date)), ### 20 being baseline scores start from, 0.1 for initial slope increase
    Period == "Intervention 1) Low-Intensity Interventions" & group_var == "treatment" ~ 20 + 0.1 * as.numeric(Date - min(Date)) + 0.2 * as.numeric(Date - as.Date("2022-04-04")), ## 0.2 for slope increase but no level change
    Period == "Intervention 2) High-Intensity Interventions" & group_var == "treatment" ~ 20 + 0.1 * as.numeric(Date - min(Date)) + 0.2 * as.numeric(Date - as.Date("2022-04-04")) + 0.3 * as.numeric(Date - as.Date("2022-06-05")), ### 0.3 for 2nd slop increas ebut no level change
    group_var == "control" ~ 15 + 0.1 * as.numeric(Date - min(Date))
  ))

# Define intervention_dates
intervention_dates <- c(as.Date("2022-04-04"), as.Date("2022-06-06"))


transform_data(
  df = tibble_data,
  time_var = "Date",
  group_var = "group_var",
  outcome_var = "score",
  intervention_dates = intervention_dates
)

plot(tibble_data[["Date"]], tibble_data[["score"]], type = "l", main = "Numeric Value Over Time", xlab = "Date", ylab = "Value")

plot(tibble_data[["Date"]][tibble_data[["group_var"]] == "treatment"], tibble_data[["score"]][tibble_data[["group_var"]] == "treatment"],
  type = "l", col = "blue",
  main = "Numeric Value Over Time by Category", xlab = "Date", ylab = "Value"
)
lines(tibble_data[["Date"]][tibble_data[["group_var"]] == "control"], tibble_data[["score"]][tibble_data[["group_var"]] == "control"], col = "red")
abline(v = as.Date(c("2022-04-01", "2022-06-01")), col = "black", lty = 2)
legend("topleft", legend = c("Treatment Group", "Control Group"), col = c("blue", "red"), lty = 1)
