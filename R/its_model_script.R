#' @title fit_its_model
#'
#' @description Fits an interrupted time series model using the nlme package, defaulting to an autocorrelation-moving average correlation structure of order (p, q)
#'
#' @param transformed_df Am unmodified data frame created from `transform_data()`.
#' @param impact_model The hypothesized impact model from interventions. Available options are 'level', 'slope', or 'levelscope'.
#' @param response_var  The response variable in your data frame.
#' @param intervention_dates A vector of time points (matching `time_var`) when interventions start. These time points are mutually exclusive and should not overlap. The function accepts up to 10 interventions.
#' @param method The estimation method for `gls()`, either "REML" (default) or "ML".
#' @param p The order of the autoregressive component. Defaults to `NULL`.
#' @param q The order of the moving average component. Defaults to `NULL`.
#' @param ... Additional arguments passed to `nlme::gls()`.
#' @return A `gls` object representing the fitted model.
#' @examples
#' output_table <- fit_its_model(data = df, response_var = 'outcome')
#' @export
#' @importFrom nlme gls corARMA
#' @importFrom dplyr ungroup group_by arrange mutate case_match sym recode
#' @importFrom rlang !! !!! :=

fit_its_model <- function(transformed_data,
                          impact_model,
                          response_var,
                          method = "REML",
                          p = NULL,
                          q = NULL,
                          ...) {

  length <- nrow(df)/2 ## only control and treatment

  num_interventions <- length(intervention_dates)

  if (is.na(num_interventions)) stop("No intervention dates supplied")
  if (num_interventions > 10) stop("More than 10 intervention dates supplied")

  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")


  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      x = case_match(!!sym(group_var), "control" ~ 0, "treatment" ~ 1),
      time_index = row_number(),
      level_pre_intervention = dplyr::case_match(
        time_index,
        1:(intervention_dates[1]-1) ~ 1,
        .default = 0
      )
    )

  for (i in seq_along(intervention_dates)) {


    level_col_name <- level_intervention_cols[i]
    slope_col_name <- slope_intervention_cols[i]

    to_index <- if (i == max(seq_along(intervention_dates))) length else (intervention_dates[i+1])-1

    df <- df %>%
      mutate(
        !!level_col_name := case_when(
          time_index %in% intervention_dates[i]:to_index ~ 1,
          .default = 0),
        !!slope_col_name := case_when(
          time_index %in% intervention_dates[i]:length ~ time_index - (intervention_dates[i]-1),
          .default = 0)
      )

  }

  termlabels <- switch(impact_model,
                       "level" = c("x * time_index", paste0("x * ", level_intervention_cols)),
                       "slope" = c("x * time_index", paste0("x * ", slope_intervention_cols)),
                       "levelscope" = c("x * time_index", paste0("x * ", level_intervention_cols), paste0("x * ", slope_intervention_cols))
                       )

  gls_object <- nlme::gls(
    reformulate(
      termlabels = termlabels,
      response = response_var
    ),
    data = df,
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

  name_map <- c(
    "(Intercept)" = "Control y-axis intercept",
    "x" = "Pilot y-axis intercept difference to control",
    "time_index" = "Control pre-intervention slope",
    "x:time_index" = "Pilot pre-intervention slope difference to control"
  )

  # Add slopes and interventions for up to 9 interventions
  for (i in 1:9) {
    name_map <- c(
      name_map,
      setNames(
        c(sprintf("Control intervention %d slope", i), sprintf("Pilot intervention %d slope", i)),
        c(sprintf("slope_%d_intervention", i), sprintf("x:slope_%d_intervention", i))
      )
    )
  }


  new_names_coeffs <- recode(
    names(coef(gls_object)),
    !!!name_map
  )

  new_names_row_matrix <- recode(
    rownames(gls_object$varBeta),
    !!!name_map
  )

  new_names_col_matrix <- recode(
    colnames(gls_object$varBeta),
    !!!name_map
  )

  new_names_parAssign <- recode(names(gls_object$parAssign), !!!name_map)

  new_names_row_varBetaFact <- recode(rownames(attr(gls_object$parAssign, "varBetaFact")), !!!name_map)

  new_names_col_varBetaFact <- recode(colnames(attr(gls_object$parAssign, "varBetaFact")), !!!name_map)


  names(gls_object$coefficients) <- new_names_coeffs
  rownames(gls_object$varBeta) <- new_names_row_matrix
  colnames(gls_object$varBeta) <- new_names_col_matrix
  names(gls_object$parAssign) <- new_names_parAssign

  rownames(attr(gls_object$parAssign, "varBetaFact")) <- new_names_row_varBetaFact
  colnames(attr(gls_object$parAssign, "varBetaFact")) <- new_names_col_varBetaFact



  return(gls_object)

}

test <- fit_its_model()
