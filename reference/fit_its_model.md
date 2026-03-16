# fit_its_model

Fits an interrupted time series model using the
[nlme](https://CRAN.R-project.org/package=nlme) package, defaulting to
an autocorrelation-moving average correlation structure of order (p, q)

## Usage

``` r
fit_its_model(
  transformed_data,
  impact_model,
  num_interventions,
  method = "REML",
  grid_search = TRUE,
  p = NULL,
  q = NULL,
  return_grid_search = FALSE,
  ...
)
```

## Arguments

- transformed_data:

  Am unmodified data frame created from \`transform_data()\`.

- impact_model:

  The hypothesized impact model from interventions. Available options
  are 'level', 'slope', or 'levelslope'.

- num_interventions:

  The number of interventions in your transformed data. Should be the
  vector length of \`intervention_dates\` passed in
  \`transform_data()\`.

- method:

  The estimation method for \`nlme::gls()\`, either "REML" (default) or
  "ML".

- grid_search:

  logical for whether to perform a grid search for determining lag
  parameters (p = AR, q = MA). By default, a grid up to values of 5 for
  each parameter is searched.

- p:

  The order of the autoregressive component. Defaults to \`NULL\`. If
  \`grid_search is enabled\`, this argument is ignored.

- q:

  The order of the moving average component. Defaults to \`NULL\`. If
  \`grid_search is enabled\`, this argument is ignored.

- return_grid_search:

  Logical flag returns the result of the grid search instead of the
  model. 'FALSE' by default.

- ...:

  Additional arguments passed to \`nlme::gls()\`.

## Value

A \`gls\` object of the fitted model.
