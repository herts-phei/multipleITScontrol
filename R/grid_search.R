#' @title grid_search
#'
#' @description Performs an autocorrelation grid search on the model of interest to
#'
#' @param data A data set object
#' @param impact_model impact model hypothesised from interventions. Available options are 'level', 'slope', 'levelscope'.
#' @param response_var the response variable in your data frame
#' @param group_var character or factor variable indicating treatment and control group. Only accepts 'treatment' and 'control' as valid elements.
#' @param max_p Maximum order of auto regressive component to test for
#' @param max_q Maximum order of moving average component to test for
#' @param ... other arguments in nlme::gls()
#' @return Returns a gls object
#' @examples
#' output_table <- fit_its_model(data = df, response_var = 'outcome')
#' @export
#' @importFrom nlme gls corARMA
#' @importFrom dplyr mutate
#'
#'

# grid_search <- function(data, impact_model, time_var, group_var, response_var, intervention_dates, method, max_p, max_q, ...) {
#
# autocorrelation_grid <- expand.grid(pval = 0:max_p, qval = 0:max_q)
#
# p <- NA
#
# for(i in 1:nrow(autocorrelation_grid)) {
#   p[i] <- try(summary(
#     fit_gls_model_advanced(outcome_of_interest,
#                            "REML",
#                            combined_df,
#                            c(
#                              "final_model_x * final_model_time_index",
#                              "final_model_x * final_model_post_first_intervention",
#                              "final_model_x * final_model_post_second_intervention"
#                            ),
#                            autocorrelation_grid$pval[i],
#                            autocorrelation_grid$qval[i])
#   )$AIC)
# }
#
# p <- ifelse(is.na(as.numeric(p)), NA, p)
#
# autocorrelation_grid |>
#   dplyr::mutate(AIC = p) |>
#   dplyr::arrange(AIC)
