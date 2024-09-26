#' @title transform_data
#'
#' @description Transforms a data frame ready for input into `fit_its_model()`.
#'
#' @param df A data frame containing the initial time series data.
#' @param time_var  A variable indicating the time index in the data frame. It must be a sequential time-series of equal intervals in numeric or a date/POSIXct/POSIXlt class.
#' @param group_var A character or factor variable indicating treatment and control groups. Only 'treatment' and 'control' are valid elements.
#' @param intervention_dates A vector of time points (matching `time_var`) when interventions start. These time points are mutually exclusive and should not overlap. The function accepts up to 10 interventions.
#' @return A transformed data frame to be passed to the `transformed_df` argument in `fit_its_model()`.
#' @examples transform_data(data, time_var = 'time_xxx', group_var = 'group', intervention_dates = c(31, 61))
#' @export
#'
#' @importFrom dplyr ungroup group_by arrange mutate case_when case_match across row_number
#' @importFrom rlang sym !! :=


transform_data <- function(df,
                           time_var,
                           group_var,
                           intervention_dates) {

  length <- nrow(df)/2 ## only control and treatment

  num_interventions <- length(intervention_dates)


  diffs <- diff(df[[time_var]] |> unique())
  equal_intervals <- all(diffs == diffs[1])

  if (isFALSE(equal_intervals)) stop("Time variable is not of equal intervals")
  if (is.na(num_interventions)) stop("No intervention dates supplied")
  if (num_interventions > 10) stop("More than 10 intervention dates supplied")


  level_intervention_cols <- paste0("level_", seq_len(num_interventions), "_intervention")
  slope_intervention_cols <- paste0("slope_", seq_len(num_interventions), "_intervention")


  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(group_var)) %>%
    dplyr::arrange(!!time_var) %>%
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

  return(df)

}

transform_data(data, time_var = 'time_xxx', group_var = 'group_xxx', intervention_dates = c(31, 61))
