#' @title its_plot
#'
#' @description Generates a ggplot2 with the values used in the ITS model along with predicted values.
#'
#' @param data_with_predictions A data frame containing the initial time series data along with predicts created from `generate_predictions()`
#' @param intervention_dates A vector of time points (matching `time_var` type) when interventions start. These time points are mutually exclusive and should not overlap. Should match `intervention_dates` argument used in `fit_its_model()`.
#' @param project_pre_intervention_trend Logical value whether to include a projection of the pre-intervention predicted values. Defaults to `TRUE`.
#' @param colours Colours passed to the `values` argument in `scale_color_manual()` and `scale_fill_manual()`. If no colours are given, defaults to `c("#3969B5", "#46C3AE")`.
#' @param se Logical value whether to include standard error values of the predictions. Defaults to `TRUE`.
#' @param point_shape Parameter passed to `shape` in `geom_point` to represent the shape of the treatment data points. Defaults to `3`.
#' @param point_size Parameter passed to `size` in `geom_point` to represent the size of the treatment data points. Defaults to `1`.
#' @param linetype Parameter passed to `linetype` in `geom_vline` to represent the line type of the vertical intervention break points. Defaults to `1`.
#' @param caption Optional argument passed to caption in `labs()`. If no argument is given, defaults to a few descriptive sentences on the lines shown in the plot.
#' @param title Optional argument passed to title in `labs()`.
#' @param subtitle Optional argument passed to subtitle in `labs()`.
#' @param x_axis Optional argument passed to x in `labs()`.
#' @param y_axis Optional argument passed to y in `labs()`.
#' @return A ggplot object
#' @examples transform_data(data, time_var = "time", group_var = "group", intervention_dates = c(31, 61))
#' @export
#'
#' @importFrom dplyr ungroup group_by arrange mutate case_when case_match across row_number
#' @importFrom rlang sym !! :=

its_plot <- function(data_with_predictions,
                     intervention_dates,
                     project_pre_intervention_trend = TRUE,
                     colours,
                     se = TRUE,
                     point_shape = 3,
                     point_size = 1,
                     linetype = 1,
                     caption = waiver(),
                     title = waiver(),
                     subtitle = waiver(),
                     x_axis = waiver(),
                     y_axis = waiver()) {
  library(grid)


  if (missing(colours)) {
    colours <- c("#3969B5", "#46C3AE")
  }

  if (inherits(caption, "waiver")) {
    caption <- if (isTRUE(project_pre_intervention_trend)) {
      "Coloured dotted lines represent a projection of the pre-intervention trend.\nColoured solid lines represent predictions from the ITS model.\nBlack vertical dotted lines represent intervention breakpoints."
    } else {
      "Coloured dotted lines represent a projection of the pre-intervention trend.\nColoured solid lines represent predictions from the ITS model."
    }
  }

  data_with_predictions |>
    ggplot(aes(time, outcome)) +
    geom_point(aes(color = category), shape = point_shape, size = point_size) + ## Actual data points
    purrr:::map(intervention_dates, ~ geom_vline(aes(xintercept = .x), linetype = linetype, size = 1)) +
    (if (isTRUE(project_pre_intervention_trend)) {
      geom_line(aes(time, pre_intervention_predictions, color = category),
        lty = 2,
        size = 1
      )
    } else {
      list() # Return an empty list if no vlines
    }) +
    geom_line(aes(time, predictions, color = category), lty = 1, size = 1) + ## prediction

    (if (isTRUE(se)) {
      geom_ribbon(aes(
        ymin = predictions - (1.96 * se),
        ymax = predictions + (1.96 * se),
        fill = category
      ), alpha = 0.1)
    } else {
      list()
    }) +
    purrr::imap(intervention_dates, ~
      annotation_custom(grob = grid::textGrob(
        label = paste("Start of intervention", .y),
        x = .x,
        y = unit(0.95, "npc"),
        just = c("left"),
        gp = gpar(fontsize = 16, fontface = "bold")
      ))) +
    # purrr:::imap(intervention_dates, ~ annotate("text", label = paste("Start of intervention", .y), x = .x, y = unit(0.95, "npc"), size = 4, hjust = 0, fontface = "bold")) +
    scale_color_manual(values = colours, name = NULL) +
    scale_fill_manual(values = colours, name = NULL) +
    theme(legend.position = "bottom") +
    labs(
      caption = caption,
      title = title,
      subtitle = subtitle,
      y = y_axis,
      x = x_axis
    )
}

# its_plot(data_with_predictions = moo_generate,
#          intervention_dates = c(31, 61),
#          project_pre_intervention_trend = TRUE,
#          colours = c("red", "blue"),
#          se = TRUE,
#          point_shape = 3,
#          point_size = 2) +
#   scale_x_continuous(breaks = seq(0, 100, 5))
