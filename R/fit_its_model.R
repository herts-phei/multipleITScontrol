#' @title fit_its_model
#'
#' @description Fits an interrupted time series model using the \link{nlme} package, defaulting to an autocorrelation-moving average correlation structure of order (p, q)
#'
#'
#' @param transformed_df Am unmodified data frame created from `transform_data()`.
#' @param impact_model The hypothesized impact model from interventions. Available options are 'level', 'slope', or 'levelscope'.
#' @param num_interventions The number of interventions in your transformed data. Should be the vector length of `intervention_dates` passed in `transform_data()`.
#' @param method The estimation method for `gls()`, either "REML" (default) or "ML".
#' @param p The order of the autoregressive component. Defaults to `NULL`.
#' @param q The order of the moving average component. Defaults to `NULL`.
#' @param ... Additional arguments passed to `nlme::gls()`.
#' @return A `gls` object of the fitted model.
#' @examples
#' fit_its_model(transformed_data = df, impact_model)
#' @export
#' @importFrom nlme gls corARMA

fit_its_model <- function(transformed_data,
                          impact_model,
                          num_interventions,
                          method = "REML",
                          p = NULL,
                          q = NULL,
                          ...) {

  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")

  termlabels <- switch(impact_model,
                       "pre_intervention" = "x * time_index",
                       "level" = c("x * time_index", paste0("x * ", level_intervention_cols)),
                       "slope" = c("x * time_index", paste0("x * ", slope_intervention_cols)),
                       "levelslope" = c("x * time_index", paste0("x * ", level_intervention_cols), paste0("x * ", slope_intervention_cols))
                       )

  gls_object <- nlme::gls(
    reformulate(
      termlabels = termlabels,
      response = "outcome"
    ),
    data = transformed_data,
    method = method,
    correlation = (
      if (is.null(p) && is.null(q)) {
        NULL
      } else {
        nlme::corARMA(
          p = p,
          q = q,
          form = ~ time_index | x
        )
      }
    )
    ,
    ...
  )

return(gls_object)

}

# fit_its_model(moo, "levelslope", num_interventions = 2) -> model
