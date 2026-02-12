set.seed(2)

# Create a date range vector for Fridays only
date_range <- seq(as.Date("2025-03-03"), as.Date("2026-08-30"), by = "day")
fridays <- date_range[weekdays(date_range) == "Friday"]

# Create a tibble with the date range
tibble_data <- tibble(Date = rep(fridays, 2))

# Add group_var with 'treatment' and 'control' groups
tibble_data <- tibble_data %>%
  mutate(group_var = rep(c("treatment", "control"), each = length(fridays)))

# Add Period column using mutate and case_when
tibble_data <- tibble_data %>%
  mutate(Period = case_when(
    Date >= as.Date("2025-03-03") & Date <= as.Date("2025-08-31") ~ "Pre-intervention period",
    Date >= as.Date("2025-09-01") & Date <= as.Date("2026-03-01") ~ "Intervention 1) Reading Program",
    Date >= as.Date("2026-03-02") & Date <= as.Date("2026-08-30") ~ "Intervention 2) Peer Tutoring Sessions"
  ))

# Add outcome_var with pre-intervention points hovering around 82, first intervention around 90, and second intervention even higher

# Create a linear sequence from 82 to 90
base <- seq(82, 90, length.out = 26)
# Add small random noise to each value
noise <- runif(26, min = -0.3, max = 0.3)
# Combine and sort to maintain gradual increase
intervention_1_values <- base + noise


# Create a linear sequence from 80 to 84
base <- seq(80, 84, length.out = 26)
# Add small random noise to each value
noise <- runif(26, min = -0.3, max = 0.3)
# Combine and sort to maintain gradual increase
pre_intervention_values_treatment <- base + noise


# Create a linear sequence from 88 to 96
base <- seq(88, 96, length.out = 26)
# Add small random noise to each value
noise <- runif(26, min = -0.3, max = 0.3)
# Combine and sort to maintain gradual increase
intervention_2_values <- base + noise

pre_intervention_values_treatment_2 <- rnorm(26, mean = 82, sd = 0.2)

pre_intervention_values_control <- rnorm(78, mean = 82, sd = 0.2)


tibble_data <- tibble_data %>%
  mutate(score = case_when(
    Period == "Pre-intervention period" & group_var == "treatment" ~ NA,
    Period == "Intervention 1) Reading Program" & group_var == "treatment" ~ NA,
    Period == "Intervention 2) Peer Tutoring Sessions" & group_var == "treatment" ~ NA,
    group_var == "control" ~ NA
  ) |> round(1))

tibble_data[tibble_data$group_var == "treatment" & tibble_data$Period == "Intervention 2) Peer Tutoring Sessions", "score"] <- intervention_2_values
tibble_data[tibble_data$group_var == "treatment" & tibble_data$Period == "Intervention 1) Reading Program", "score"] <- intervention_1_values
tibble_data[tibble_data$group_var == "treatment" & tibble_data$Period == "Pre-intervention period", "score"] <- pre_intervention_values_treatment_2
tibble_data[tibble_data$group_var == "control", "score"] <- pre_intervention_values_control

tibble_data <- tibble_data |> mutate(across(score, round, 1))

its_data_school <- tibble_data

usethis::use_data(its_data_school, overwrite = TRUE)
