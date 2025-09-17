#' @title transform_data
#'
#' @description Transforms a data frame ready for input into \link{fit_its_model}, creating relevant columns for slope and level effects of interventions.
#'
#' Requires a minimum of three time points in each intervention period and pre-intervention period.
#'
#' @param df A data frame containing the initial time series data.
#' @param time_var  A variable indicating the time index in the data frame. It must be a sequential time-series of equal intervals in numeric or a date/POSIXct/POSIXlt class.
#' @param group_var A character or factor variable indicating treatment and control groups. Only 'treatment' and 'control' are valid elements.
#' @param outcome_var A numeric variable indicating outcome.response variable.
#' @param intervention_dates A vector of time points (matching type of \code{time_var}) when interventions start. These time points are mutually exclusive and should not overlap. The argument accepts up to nine values representing the intervention start times.
#' @return A transformed data frame to be passed to \code{transformed_df} in \link{fit_its_model}.
#' @examples transform_data(data, time_var = "time_xxx", group_var = "group", outcome_var = "outcome", intervention_dates = c(31, 61))
#' @export
#'
#' @importFrom dplyr ungroup group_by arrange mutate case_when case_match across row_number
#' @importFrom rlang sym !! :=
#' @importFrom magrittr %>%


transform_data <- function(df,
                           time_var,
                           group_var,
                           outcome_var,
                           intervention_dates) {
  internal_data <- df

  diffs <- diff(internal_data[[time_var]] |> unique())
  equal_intervals <- all(diffs == diffs[1])

  num_interventions <- length(intervention_dates)

  if (isFALSE(equal_intervals)) stop("Time variable is not of equal intervals")
  if (is.na(num_interventions)) stop("No intervention dates supplied")
  if (num_interventions > 9) stop("More than 9 intervention dates supplied")
  if (!is.numeric(internal_data[[outcome_var]])) stop("Outcome variable is not numeric")
  if (isFALSE(length(internal_data[[group_var]] == "treatment") == length(internal_data[[group_var]] == "control"))) stop("Treatment and Control groups have differing number of time points")


  length <- nrow(internal_data) / 2 ## only control and treatment

  date_vector <- internal_data[[time_var]] |>
    sort() |>
    unique()

  if (isFALSE(all(intervention_dates %in% date_vector))) stop("Defined intervention timepoints do not have matching timepoints in date/time variable")

  matching_indices <- match(intervention_dates, date_vector)

  period_checks <- c(1, matching_indices, length)
  differences <- diff(period_checks)

  if (any(differences < 3)) "One or more intervention periods (including pre-intervention) have less than 3 time points"

  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")


  internal_data <- internal_data %>%
    dplyr::rename(
      "category" := !!ensym(group_var),
      "time" := !!ensym(time_var),
      "outcome" := !!ensym(outcome_var)
    ) |>
    dplyr::ungroup() %>%
    dplyr::group_by(category) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      x = case_match(category, "control" ~ 0, "treatment" ~ 1),
      time_index = row_number(),
      level_pre_intervention = dplyr::case_match(
        time_index,
        1:(which(date_vector == intervention_dates[1]) - 1) ~ 1,
        .default = 0
      )
    )

  for (i in seq_along(intervention_dates)) {
    level_col_name <- level_intervention_cols[i]
    slope_col_name <- slope_intervention_cols[i]

    to_index <- if (i == max(seq_along(intervention_dates))) length else (internal_data[which(internal_data[["time"]] == intervention_dates[i + 1]), "time_index"] |> unique() |> pull() - 1)

    init_index <- internal_data[which(internal_data[["time"]] == intervention_dates[i]), "time_index"] |>
      unique() |>
      pull()

    internal_data <- internal_data %>%
      mutate(
        !!level_col_name := case_when(
          time_index %in% init_index:to_index ~ 1,
          .default = 0
        ),
        !!slope_col_name := case_when(
          time_index %in% init_index:length ~ time_index - (init_index - 1),
          .default = 0
        )
      )
  }

  return(internal_data)
}

# transform_data(df = data,
#                time_var = 'time_xxx',
#                group_var = 'group_xxx',
#                outcome_var = 'outcome',
#                intervention_dates = c(31, 61)) -> moo
