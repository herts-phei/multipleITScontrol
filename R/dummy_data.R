library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Define the number of time points
n_timepoints_pre <- 30    # Pre-intervention period
n_timepoints_post1 <- 30  # Post intervention 1 period
n_timepoints_post2 <- 30  # Post intervention 2 period
n_total_timepoints <- n_timepoints_pre + n_timepoints_post1 + n_timepoints_post2

# Time variable (1 to 90)
time <- 1:n_total_timepoints

# Generate baseline outcome for control and intervention groups
control <- 50 + rnorm(n_total_timepoints, 0, 5)  # Control group remains stable
intervention <- 50 + rnorm(n_total_timepoints, 0, 5)  # Baseline for intervention group

# Define the level and slope changes for the interventions
# Intervention 1 starts at time point 31 (after 30 pre-intervention points)
level_change1 <- 10  # Immediate level change at intervention 1
slope_change1 <- 0.5 # Change in slope after intervention 1

# Intervention 2 starts at time point 61 (after 30 points of Intervention 1)
level_change2 <- 15  # Immediate level change at intervention 2
slope_change2 <- 0.8 # Change in slope after intervention 2

# Apply the first intervention (time point 31 onwards)
intervention[31:n_total_timepoints] <- intervention[31:n_total_timepoints] +
  level_change1 + (time[31:n_total_timepoints] - 30) * slope_change1

# Apply the second intervention (time point 61 onwards)
intervention[61:n_total_timepoints] <- intervention[61:n_total_timepoints] +
  level_change2 + (time[61:n_total_timepoints] - 60) * slope_change2

# Combine the control and intervention data into a data frame
data <- data.frame(
  time = rep(time, 2),
  group = rep(c("control", "treatment"), each = n_total_timepoints),
  outcome = c(control, intervention)
)

data <- data |>
  rename(time_xxx = time,
         group_xxx = group)

# Preview the dataset
head(data)

# Visualize the dataset using ggplot2
library(ggplot2)
ggplot(data, aes(x = time, y = outcome, color = group)) +
  geom_line(size = 1) +
  labs(title = "Interrupted Time Series (ITS) with Control and Interventions",
       x = "Time",
       y = "Outcome") +
  theme_minimal()
