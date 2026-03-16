# transform_data

Transforms a data frame ready for input into
[fit_its_model](https://herts-phei.github.io/multipleITScontrol/reference/fit_its_model.md),
creating relevant columns for slope and level effects of interventions.

Requires a minimum of three time points in each intervention period and
pre-intervention period.

## Usage

``` r
transform_data(df, time_var, group_var, outcome_var, intervention_dates)
```

## Arguments

- df:

  A data frame containing the initial time series data.

- time_var:

  A variable indicating the time index in the data frame. It must be a
  sequential time-series of equal intervals in numeric or a
  date/POSIXct/POSIXlt class.

- group_var:

  A character or factor variable indicating treatment and control
  groups. Only 'treatment' and 'control' are valid elements.

- outcome_var:

  A numeric variable indicating outcome.response variable.

- intervention_dates:

  A vector of time points (matching type of `time_var`) when
  interventions start. These time points are mutually exclusive and
  should not overlap. The argument accepts up to three values
  representing the intervention start times.

## Value

A transformed data frame to be passed to `transformed_df` in
[fit_its_model](https://herts-phei.github.io/multipleITScontrol/reference/fit_its_model.md).
