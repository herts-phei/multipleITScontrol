#' Generate Predictions from a Transformed Dataset and a Model
#'
#' This function generates predictions and standard errors for a given transformed dataset using the provided generalized least squares (GLS) model. It also calculates pre-intervention predictions based on model coefficients.
#'
#' @param transformed_data A data frame containing the transformed dataset. Output from `transform_data()`.
#' @param model A GLS model object used to make predictions. The model should include coefficients used to generate pre-intervention predictions.
#'
#' @return A data frame with the following additional columns:
#' \describe{
#'   \item{pre_intervention_predictions}{Predicted values for the pre-intervention period, calculated using model coefficients.}
#'   \item{predictions}{Predicted values for the entire dataset using the GLS model.}
#'   \item{se}{Standard errors for the predictions.}
#' }
#'
#' @details
#' The function first computes pre-intervention predictions using model coefficients, specifically the intercept, `time_index`, and interaction terms. It ensures that predictions for the pre-intervention period (`level_pre_intervention == 1`) are set to `NA`. Then, it uses the `AICcmodavg::predictSE.gls()` function to calculate predictions and standard errors from the provided GLS model.
#'
#' @examples
#' \dontrun{
#'   # Assuming `transformed_data` is a prepared data frame and `model` is a GLS model:
#'   predictions_df <- generate_predictions(transformed_data, model)
#'   head(predictions_df)
#' }
#'
#' @importFrom dplyr arrange ungroup mutate case_when
#' @importFrom AICcmodavg predictSE.gls
#' @importFrom nlme corARMA
#'
#' @export


generate_predictions <- function(transformed_data,
                                 model) {

  length <- nrow(transformed_data)/2

  # pre_intevention_version <- fit_its_model(
  #   transformed_data = transformed_data |> filter(date_pre_intervention == 1),
  #   impact_model = "pre_intervention",
  #   response_var,
  #   method = "REML",
  #   correlation = (
  #     if (is.null(p) && is.null(q)) {
  #       NULL
  #     } else {
  #       nlme::corARMA(
  #         p = p,
  #         q = q,
  #         form = ~ time_index | x
  #       )
  #     }
  #   )
  #   ,
  #   ...
  # )

  used_coefs <- model$coefficients

  df <- transformed_data |>
    arrange(x) |>
    ungroup()


  # valid_names <- name_map[names(name_map) %in% names(df)]
  # test <- df %>% rename(!!!setNames(names(valid_names), valid_names))
  #
  # internal_model <- model




  df <- df %>%
    mutate(
      pre_intervention_predictions = with(as.list(used_coefs), c( ## check 70
        `(Intercept)` + cumsum(rep(`time_index`, length)),
        c(`(Intercept)` + `x`) + cumsum(
          rep(
            `time_index` + `x:time_index`,
            length
          )
        )
      )),
      pre_intervention_predictions = case_when(
        level_pre_intervention == 1 ~ NA,
        TRUE ~ pre_intervention_predictions
      ),
      predictions = AICcmodavg::predictSE.gls(model, df, se.fit = T)$fit,
      se = AICcmodavg::predictSE.gls(model, df, se.fit = T)$se
    )

  return(df)

}

generate_predictions(transformed_data = moo, model) -> moo_generate
