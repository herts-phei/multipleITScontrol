#' @title slope_difference
#'
#' @description Ascertains whether there is a statistically significant difference in a slope or level during an intervention period between the pilot group and the control group. The estimated difference is relative to the control group. I.e. a positive coefficient means the slope of the pilot group is higher than the control group.
#'
#' @param model model output object from \code{multipleITScontrol::fit_its_model}
#' @param intervention which intervention to test significance in difference. Integer value of 1, 2, or 3.
#' @param return Logical for returning pretty output in console. TRUE by default
#' @return A transformed data frame to be passed to \code{transformed_df} in \link{fit_its_model}.
#' @examples slope_difference(fitted_ITS_model, intervention = 1)
#' @export
#'
#' @importFrom dplyr ungroup group_by arrange mutate case_when case_match across row_number tibble
#' @importFrom rlang sym !! :=
#' @importFrom magrittr %>%
#' @importFrom scales pvalue_format
#' @importFrom tibble tribble
#' @importFrom stats coef vcov qt pt

slope_difference <- function(model, intervention, return = TRUE) {
  if (!intervention %in% c(1L, 2L, 3L)) {
    stop("Error: 'intervention' must be one of the integers 1, 2, or 3.", call. = FALSE)
  }

  ## Notes
  ## Calculates difference in coefficient for specific intervention, for slope
  ## Extract variance–covariance matrix
  ## Compute standard error of a linear combination
  ## Compute degrees of freedom in model
  ## Compute critical t‑value
  ## Compute t‑statistic and p‑value
  ## Computes the 95% confidence interval


  model <- model

  ## extract coefficients ##
  coef_estimate <- coef(model)

  if (intervention == 3 & isFALSE(any(grepl("intervention 3", names(coef_estimate))))) {
    stop("Error: No third intervention in model", call. = FALSE)
  }

  if (intervention == 2 & isFALSE(any(grepl("intervention 2", names(coef_estimate))))) {
    stop("Error: No second intervention in model", call. = FALSE)
  }

  cov_matrix <- vcov(model)

  int_a <- names(coef(model))[grep(
    "Pilot pre-intervention slope difference to control",
    names(coef(model))
  )]

  int_b <- names(coef(model))[grep(
    "Pilot intervention 1 slope",
    names(coef(model))
  )]
  int_c <- names(coef(model))[grep(
    "Pilot intervention 2 slope",
    names(coef(model))
  )]
  int_d <- names(coef(model))[grep(
    "Control intervention 2 slope",
    names(coef(model))
  )]

  int_e <- names(coef(model))[grep(
    "Control intervention 1 slope",
    names(coef(model))
  )]

  int_f <- names(coef(model))[grep(
    "Control pre-intervention slope",
    names(coef(model))
  )]


  int_g <- names(coef(model))[grep(
    "Control intervention 3 slope",
    names(coef(model))
  )]

  int_h <- names(coef(model))[grep(
    "Pilot intervention 3 slope",
    names(coef(model))
  )]

  ## slope 1 intervention

  ## difference in slope ##

  if (intervention == 1) {
    difference_slope <- coef_estimate[int_a] + coef_estimate[int_b]

    combined_se <- sqrt(cov_matrix[int_a, int_a] +
      cov_matrix[int_b, int_b] +
      2 * cov_matrix[int_a, int_b])

    slope_control <- coef_estimate[int_e] + coef_estimate[int_f]

    slope_treatment <- coef_estimate[int_e] + coef_estimate[int_f] + coef_estimate[int_a] + coef_estimate[int_b]
  } else if (intervention == 2) {
    # difference in slope for second intervention is ----

    # (Pilot slope pre-intervention difference to control +
    # Pilot intervention 1 slope +
    # Pilot intervention 2 slope) - Control intervention 2 slope

    difference_slope <- (coef_estimate[int_a] + coef_estimate[int_b] + coef_estimate[int_c])

    combined_se <- sqrt(
      cov_matrix[int_a, int_a] +
        cov_matrix[int_b, int_b] +
        cov_matrix[int_c, int_c] +
        (2 * cov_matrix[int_a, int_b]) +
        (2 * cov_matrix[int_a, int_c]) +
        (2 * cov_matrix[int_b, int_c])
    )

    slope_control <- coef_estimate[int_e] + coef_estimate[int_f] + coef_estimate[int_d]

    slope_treatment <- (coef_estimate[int_e] + coef_estimate[int_f] + coef_estimate[int_a] + coef_estimate[int_b] + coef_estimate[int_d] + coef_estimate[int_c])
  } else if (intervention == 3) {
    difference_slope <- (coef_estimate[int_a] + coef_estimate[int_b] + coef_estimate[int_c] + coef_estimate[int_h])

    combined_se <- sqrt(
      cov_matrix[int_a, int_a] +
        cov_matrix[int_b, int_b] +
        cov_matrix[int_c, int_c] +
        cov_matrix[int_h, int_h] +
        (2 * cov_matrix[int_a, int_b]) +
        (2 * cov_matrix[int_a, int_c]) +
        (2 * cov_matrix[int_a, int_h]) +
        (2 * cov_matrix[int_b, int_c]) +
        (2 * cov_matrix[int_b, int_h]) +
        (2 * cov_matrix[int_c, int_h])
    )

    slope_control <- coef_estimate[int_e] + coef_estimate[int_f] + coef_estimate[int_d] + coef_estimate[int_g]

    slope_treatment <- (coef_estimate[int_e] + coef_estimate[int_f] + coef_estimate[int_a] + coef_estimate[int_b] + coef_estimate[int_d] + coef_estimate[int_c] + coef_estimate[int_g] + coef_estimate[int_h])
  }

  df <- model[["dims"]][["N"]] - model[["dims"]][["p"]]

  ## computes confidence interval ##
  crit_value <- qt(0.975, df = df) # two tail

  # 2 * pt(-abs(crit_value), df)

  t_statistic <- difference_slope / combined_se

  # 2 * pt(-abs(t_statistic), df)


  lower_bound <- difference_slope - (crit_value * combined_se)
  upper_bound <- difference_slope + (crit_value * combined_se)

  p_value <- 2 * pt(-abs(t_statistic), df)

  p_value_formatted <- scales::pvalue_format()(p_value)
  difference_slope_formatted <- as.character(round(difference_slope, 2))
  lower_bound_formatted <- as.character(round(lower_bound, 2))
  upper_bound_formatted <- as.character(round(upper_bound, 2))

  slope_treatment_formatted <- as.character(round(slope_treatment, 2))
  slope_control_formatted <- as.character(round(slope_control, 2))


  if (isTRUE(return)) {
    cat(
      "## INTERVENTION ", intervention, "##", "\n\n",
      "Slope for treatment per x-axis unit:", slope_treatment_formatted, "\n",
      "Slope for control per x-axis unit:", slope_control_formatted, "\n",
      "Slope difference:", difference_slope_formatted, "\n", "95% CI:", lower_bound_formatted, "to", upper_bound_formatted, "\n", "p-value:", p_value_formatted, "\n", "\n"
    )
  }

  inner_table <- tibble::tribble(
    ~Variable, ~Value_Raw, ~Value_Formatted,
    "Intervention", intervention, as.character(intervention),
    "Slope for treatment", slope_treatment, slope_treatment_formatted,
    "Slope for control", slope_control, slope_control_formatted,
    "Slope difference", difference_slope, difference_slope_formatted,
    "Lower 95% CI", lower_bound, lower_bound_formatted,
    "Upper 95% CI", upper_bound, upper_bound_formatted,
    "p.value", p_value, p_value_formatted
  )

  return(inner_table)

  ## code for aggregate effect ####

  ## 12.1) Find CI for aggregate effect ####

  # (difference_slope * 14 - (crit_value * combined_se * 14)) |> round(2)
  # difference_slope * 14
  # (difference_slope * 14 + (crit_value * combined_se * 14)) |> round(2)
}
#
# slope_difference(model = my_summary_its_model, intervention = 2) |> View()
