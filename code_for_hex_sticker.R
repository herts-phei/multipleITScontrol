# Load required libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

# Generate data
time <- 0:49
intervention_1 <- 20
intervention_2 <- 35

# Generate baseline trend for treatment and control
control_trend <- 0.5 * time + rnorm(length(time), 0, 2)
treatment_trend <- 0.5 * time + rnorm(length(time), 0, 2)

# Apply intervention effect on treatment
treatment_trend[intervention_1:length(time)] <- treatment_trend[intervention_1:length(time)] + 10
treatment_trend[intervention_2:length(time)] <- treatment_trend[intervention_2:length(time)] + 5

# Create a data frame for plotting
data <- data.frame(
  time = rep(time, 2),
  outcome = c(control_trend, treatment_trend),
  group = rep(c("Control", "Treatment"), each = length(time))
)

# Plot using ggplot2
ggplot(data, aes(x = time, y = outcome, color = group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(intervention_1, intervention_2), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Control" = "#a6bddb", "Treatment" = "#2b8cbe")) +
  # Load required libraries
  library(ggplot2)

# Set seed for reproducibility
set.seed(42)

# Generate data
time <- 0:49
intervention_1 <- 20
intervention_2 <- 35

# Generate baseline trend for treatment and control
control_trend <- 0.5 * time + rnorm(length(time), 0, 2)
treatment_trend <- 0.5 * time + rnorm(length(time), 0, 2)

# Apply intervention effect on treatment
treatment_trend[intervention_1:length(time)] <- treatment_trend[intervention_1:length(time)] + 10
treatment_trend[intervention_2:length(time)] <- treatment_trend[intervention_2:length(time)] + 5

# Create a data frame for plotting
data <- data.frame(
  time = rep(time, 2),
  outcome = c(control_trend, treatment_trend),
  group = rep(c("Control", "Treatment"), each = length(time))
)

# Plot using ggplot2
p <- ggplot(data, aes(x = time, y = outcome, color = group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = c(intervention_1, intervention_2), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Control" = "indianred3", "Treatment" = "#2b8cbe")) +
  ggpubr::theme_transparent() +
  # theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

hexSticker::sticker(p, package = "multipleITScontrol", p_family = "Audrey", p_size=14, p_y = 1.45, s_x=1, s_y=.85, s_width=1.3, s_height=1, h_color = "lightsteelblue4", h_fill = "lightsteelblue3")
