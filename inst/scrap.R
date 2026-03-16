transform_data <- function(df = tibble_data
                           time_var =  "Date"
                           group_var = "group_var"
                           outcome_var = "score"
                           intervention_dates = c(as.Date("2025-09-05"), as.Date("2026-03-06"))) {



  diffs <- diff(df[[time_var]] |> unique())
  equal_intervals <- all(diffs == diffs[1])

  num_interventions <- length(intervention_dates)

  if (isFALSE(equal_intervals)) stop("Time variable is not of equal intervals")
  if (is.na(num_interventions)) stop("No intervention dates supplied")
  if (num_interventions > 9) stop("More than 9 intervention dates supplied")
  if (!is.numeric(df[[outcome_var]])) stop("Outcome variable is not numeric")
  if (isFALSE(length(df[[group_var]] == 'treatment') == length(df[[group_var]] == 'control'))) stop("Treatment and Control groups have differing number of time points")

  length <- nrow(df)/2 ## only control and treatment

  date_vector <- df[["Date"]] |> sort() |> unique()
  matching_indices <- match(intervention_dates, date_vector)

  period_checks <- c(1, matching_indices, length)
  differences <- diff(period_checks)

  if (any(differences < 3)) "One or more intervention periods (including pre-intervention) have less than 3 time points"


  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")


  df <- df %>%
    dplyr::rename("category" := !!ensym(group_var),
                  "time" := !!ensym(time_var),
                  "outcome" := !!ensym(outcome_var)) |>
    dplyr::ungroup() %>%
    dplyr::group_by(category) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      x = case_match(category, "control" ~ 0, "treatment" ~ 1),
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

    to_index <- if (i == max(seq_along(intervention_dates))) length else (df[which(df[["time"]] == intervention_dates[i+1]), "time_index"] |> unique() |> pull() - 1)

    init_index <- df[which(df[["time"]] == intervention_dates[i]), "time_index"] |> unique() |> pull()

    df <- df %>%
      mutate(
        !!level_col_name := case_when(
          time_index %in% init_index:to_index ~ 1,
          .default = 0),
        !!slope_col_name := case_when(
          time_index %in% init_index:length ~ time_index - (init_index-1),
          .default = 0)
      )

  }

  return(df)

}

# transform_data(df = data,
#                time_var = 'time_xxx',
#                group_var = 'group_xxx',
#                outcome_var = 'outcome',
#                intervention_dates = c(31, 61)) -> moo
