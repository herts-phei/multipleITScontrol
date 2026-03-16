generate_predictions_test <- function(transformed_data,
                                      model) {
  length <- nrow(transformed_data) / 2

  used_coefs <- model$coefficients

  df <- transformed_data |>
    arrange(x) |>
    ungroup()

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
        date_pre_intervention == 1 ~ NA,
        TRUE ~ pre_intervention_predictions
      ),
      predictions = AICcmodavg::predictSE.gls(model, df, se.fit = T)$fit,
      se = AICcmodavg::predictSE.gls(model, df, se.fit = T)$se
    )

  return(df)
}


####


combined_df <- readRDS("~/projects/multipleITScontrolOLD/data-raw/combined_df.Rds")

combined_df <- combined_df |> rename(
  "x" = "final_model_x",
  "time_index" = "final_model_time_index",
  "slope_1_intervention" = "final_model_post_first_intervention",
  "slope_2_intervention" = "final_model_post_second_intervention",
  "outcome" = "cum_pct_uptake"
)


test_model <- fit_its_model(combined_df, impact_model = "slope", grid_search = FALSE, p = 4, q = 4, num_interventions = 2)

test_generate <- generate_predictions_test(combined_df, test_model)

test_generate <- test_generate |> rename("category" = "component")

its_plot(
  data_with_predictions = test_generate, time_var = "date",
  intervention_dates = as.Date(c("2024/05/05", "2024/05/19"))
)


test_generate |>
  ggplot(aes(date, outcome)) +
  geom_point(aes(color = category), shape = 3, size = 1) +
  geom_line(aes(date, pre_intervention_predictions, color = category),
    lty = 2,
    size = 1
  ) +
  geom_line(aes(date, predictions, color = category), lty = 1, size = 1) ## prediction

######

transformed_data_with_predictions |>
  ggplot(aes(time, outcome)) +
  geom_point(aes(color = category), shape = 3, size = 1) +
  geom_line(aes(time, pre_intervention_predictions, color = category),
    lty = 2,
    size = 1
  ) +
  geom_line(aes(time, predictions, color = category), lty = 1, size = 1) ## prediction
