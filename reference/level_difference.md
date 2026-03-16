# level_difference

Ascertains whether there is a statistically significant difference in a
level during an intervention period between the pilot group and the
control group. The estimated difference is relative to the control
group. I.e. a positive coefficient means the slope. Returns

## Usage

``` r
level_difference(model, intervention, return = TRUE)
```

## Arguments

- model:

  model output object from
  [`multipleITScontrol::fit_its_model`](https://herts-phei.github.io/multipleITScontrol/reference/fit_its_model.md)

- intervention:

  which intervention to test significance in difference. Integer value
  of 1, 2, or 3.

- return:

  Logical for returning pretty output in console. TRUE by default

## Value

A transformed data frame to be passed to `transformed_df` in
[fit_its_model](https://herts-phei.github.io/multipleITScontrol/reference/fit_its_model.md).
