#' @title slope_difference
#'
#' @description Calculates the effect size of the difference in slopes between the pilot and control groups during intervention periods, along with 95% confidence intervals.
#'
#' @param glm_its A `glm()` object representing the interrupted time series model.
#' @param output_format A character scalar indicating output format. Only 'list' and 'df' are valid inputs. Defaults to 'list'.
#' @return A list or data.frame object containing the slope effect size difference and the corresponding 95% confidence intervals for the interventions in the ITS control model.
#' @examples
#' slope_difference(my_its_model)
#' @export
#' @importFrom stringr str_subset

slope_difference <- function(glm_its, output_format = 'list') {

if (!output_format %in% c("list", "df")) stop("Invalid output format selected")

coefs <- coef(glm_its)

relevant_coefs <- c("Pilot pre-intervention slope difference to control", names(coefs) |> str_subset(pattern = "Pilot intervention [0-9] slope"))

# Initialize an empty list to store results
results_list <- list()

for (i in seq_len(length(relevant_coefs)-1)) {

internal_coefs <- relevant_coefs[1:(i+1)]

## difference in slope ##

combined_estimate <- sum(coefs[internal_coefs])

cov_matrix <- vcov(glm_its)

# Extract the variances (diagonal elements)
variances <- diag(cov_matrix[internal_coefs, internal_coefs])

# Extract the covariances (off-diagonal elements)
covariances <- cov_matrix[internal_coefs, internal_coefs]

# Compute the combined standard error using the variances and covariances
combined_se <- sqrt(sum(variances) + 2 * sum(covariances[lower.tri(covariances)]))


# combined_se <- sqrt(cov_matrix["x:time_index", "x:time_index"] +
#                       cov_matrix["x:slope_1_intervention", "x:slope_1_intervention"] +
#                       cov_matrix["x:slope_2_intervention", "x:slope_2_intervention"] +
#                       2 * cov_matrix["x:time_index", "x:slope_1_intervention"] +
#                       2 * cov_matrix["x:slope_1_intervention", "x:slope_2_intervention"] +
#                       2 * cov_matrix["x:time_index", "x:slope_2_intervention"]) ## old code, same as above if 2 interventions but above allows for indeterminate interventions

df <- glm_its[["dims"]][["N"]] - glm_its[["dims"]][["p"]]

# computes confidence interval
crit_value <- qt(0.975, df = df) # two tail

# 2 * pt(-abs(crit_value), df)

t_statistic <- combined_estimate/combined_se

# 2 * pt(-abs(t_statistic), df)

lower_bound <- combined_estimate - (crit_value*combined_se)
upper_bound <- combined_estimate + (crit_value*combined_se)

# Create a named sublist for the current iteration
results_list[[paste0("Intervention ", i, " slope")]] <- list(
  `Combined Estimate` = combined_estimate,
  `95% Confidence Interval` = c(lower_bound, upper_bound)
)


# cat(i, " : Combined Estimate: ", combined_estimate, "\n")
# cat("95% Confidence Interval: [", lower_bound, ", ", upper_bound, "]\n")


}

if (output_format == "df") {
  # Convert results_list to a data frame
  results_df <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(
      `Combined Estimate` = x$`Combined Estimate`,
      `Lower Bound` = x$`95% Confidence Interval`[1],
      `Upper Bound` = x$`95% Confidence Interval`[2],
      stringsAsFactors = FALSE
    )
  }))
  # Set the row names to the intervention names
  rownames(results_df) <- names(results_list)
  return(results_df)
} else if (output_format == "list") {
  return(results_list)
}


}

# moo <- slope_difference(test, output_format = "list")


