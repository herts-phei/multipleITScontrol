# its_plot

Generates a ggplot2 with the values used in the ITS model along with
predicted values.

## Usage

``` r
its_plot(
  model,
  data_with_predictions,
  time_var,
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
  y_axis = waiver()
)
```

## Arguments

- model:

  model output from \`multipleITScontrol::summary_its()\`

- data_with_predictions:

  A data frame containing the initial time series data along with
  predicts created from \`generate_predictions()\`

- time_var:

  A variable indicating the time index in the data frame. It must be a
  sequential time-series of equal intervals in numeric or a
  date/POSIXct/POSIXlt class.

- intervention_dates:

  A vector of time points (matching \`time_var\` type) when
  interventions start. These time points are mutually exclusive and
  should not overlap. Should match \`intervention_dates\` argument used
  in \`fit_its_model()\`.

- project_pre_intervention_trend:

  Logical value whether to include a projection of the pre-intervention
  predicted values. Defaults to \`TRUE\`.

- colours:

  Colours passed to the \`values\` argument in \`scale_color_manual()\`
  and \`scale_fill_manual()\`. If no colours are given, defaults to
  \`c("#3969B5", "#46C3AE")\`.

- se:

  Logical value whether to include standard error values of the
  predictions. Defaults to \`TRUE\`.

- point_shape:

  Parameter passed to \`shape\` in \`geom_point\` to represent the shape
  of the treatment data points. Defaults to \`3\`.

- point_size:

  Parameter passed to \`size\` in \`geom_point\` to represent the size
  of the treatment data points. Defaults to \`1\`.

- linetype:

  Parameter passed to \`linetype\` in \`geom_vline\` to represent the
  line type of the vertical intervention break points. Defaults to
  \`1\`.

- caption:

  Optional argument passed to caption in \`labs()\`. If no argument is
  given, defaults to a few descriptive sentences on the lines shown in
  the plot.

- title:

  Optional argument passed to title in \`labs()\`.

- subtitle:

  Optional argument passed to subtitle in \`labs()\`.

- x_axis:

  Optional argument passed to x in \`labs()\`.

- y_axis:

  Optional argument passed to y in \`labs()\`.

## Value

A ggplot object
