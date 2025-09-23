#' @title fit_its_model
#'
#' @description Fits an interrupted time series model using the \link{nlme} package, defaulting to an autocorrelation-moving average correlation structure of order (p, q)
#'
#'
#' @param transformed_df Am unmodified data frame created from `transform_data()`.
#' @param impact_model The hypothesized impact model from interventions. Available options are 'level', 'slope', or 'levelslope'.
#' @param num_interventions The number of interventions in your transformed data. Should be the vector length of `intervention_dates` passed in `transform_data()`.
#' @param method The estimation method for `gls()`, either "REML" (default) or "ML".
#' @param grid_search logical for whether to perform a grid search for determining lag parameters (p = AR, q = MA). By default, a grid up to values of 5 for each parameter is searched.
#' @param p The order of the autoregressive component. Defaults to `NULL`. If `grid_search is enabled`, this argument is ignored.
#' @param q The order of the moving average component. Defaults to `NULL`. If `grid_search is enabled`, this argument is ignored.
#' @param return_grid_search Logical flag returns the result of the grid search instead of the model. 'FALSE' by default.
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
                          grid_search = TRUE,
                          p = NULL,
                          q = NULL,
                          return_grid_search = FALSE,
                          ...) {
  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")

  termlabels <- switch(impact_model,
    "pre_intervention" = "x * time_index",
    "level" = c("x * time_index", paste0("x * ", level_intervention_cols)),
    "slope" = c("x * time_index", paste0("x * ", slope_intervention_cols)),
    "levelslope" = c("x * time_index", paste0("x * ", level_intervention_cols), paste0("x * ", slope_intervention_cols))
  )


  if (grid_search) {
    autocorrelation_grid <- expand.grid(pval = 0:5, qval = 0:5) ## lag parameters, p = AR, q = MA

    p <- NA

    for (i in 1:nrow(autocorrelation_grid)) {
      p[i] <- try(summary(
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
                p = autocorrelation_grid$pval[i],
                q = autocorrelation_grid$qval[i],
                form = ~ time_index | x
              )
            })
        )
      )$AIC, silent = TRUE)
    }

    p <- ifelse(is.na(as.numeric(p)), NA, p)

    autocorrelation_grid <- autocorrelation_grid |>
      mutate(AIC = p) |>
      filter(!is.na(AIC)) |>
      mutate(across(AIC, as.numeric)) |>
      arrange(-AIC)
  }

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
          p = if (grid_search) autocorrelation_grid$pval[1] else p,
          q = if (grid_search) autocorrelation_grid$qval[1] else q,
          form = ~ time_index | x
        )
      }),
    ...
  )

  if (isFALSE(return_grid_search)) {
    return(gls_object)
  } else if (isTRUE(return_grid_search) & isTRUE(grid_search)) {
    return(autocorrelation_grid)
  } else if (isTRUE(return_grid_search) & isFALSE(grid_search)) {
    stop("Grid search was not enabled")
  }
}

# fit_its_model(moo, "levelslope", num_interventions = 2) -> model
