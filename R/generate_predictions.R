
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
      predictions = AICcmodavg::predictSE.gls(model, moo, se.fit = T)$fit,
      se = AICcmodavg::predictSE.gls(model, moo, se.fit = T)$se
    )

  return(df)

}

generate_predictions(moo, model) -> moo_generate
