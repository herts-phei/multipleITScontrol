
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multipleITScontrol <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/multipleITScontrol)](https://CRAN.R-project.org/package=multipleITScontrol)
[![Codecov test
coverage](https://codecov.io/gh/herts-phei/multipleITScontrol/graph/badge.svg)](https://app.codecov.io/gh/herts-phei/multipleITScontrol)
[![R-CMD-check](https://github.com/herts-phei/multipleITScontrol/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/herts-phei/multipleITScontrol/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The goal of multipleITScontrol is to provide functions to easily wrangle
data collected for an intervention and to perform interrupted time
series statistical analysis, involving tracking a long-term period
before and after a intervention at a defined point in time to assess the
intervention’s effects, along with methods allowing for comparing the
outcomes for the treated units with those of a control/comparison unit,
and to allow for multiple successive interventions.

Interrupted-time series (ITS) is a quasi-experimental study design for
evaluating the impact of an intervention or successive interventions
that are implemented at defined moments in time. This is much more
informative than an end-point analysis which only allows an
understanding of differences between the final situations, rather than
establishing how the different interventions influenced an outcome. In
practice, ITS models are useful to establish an underlying trend over
time and understand the effect of “interrupting” this trend with the
interventions that are implemented. The trend that would have been
expected to continue occurring without the intervention is called the
“counterfactual” scenario, and the impact of introducing the
intervention is typically determined by comparing whether there was a
difference in this counterfactual trend, and the trend that arose after
the intervention.

In a more complex situation however, a second counterfactual scenario
can be considered by introducing a “control” group that is not subject
to the intervention. A controlled ITS study helps in understanding the
effect of interventions when the underlying trend is expected to change
in a time-varied manner that has not been, or cannot be, accounted for.
For example, a possible scenario could be that the underlying trend will
change due to seasonality.

More information on ITS models and controlled ITS models can be found in
Lopez Bernal, Cummins, and Gasparrini (2016, 2018).

A lot of the code is derived and inspired from the rpub article [A
pragmatic Introduction to Interrupted Time
Series](https://rpubs.com/chrissyhroberts/1006858) by Chrissy Roberts,
many thanks goes to him.

## Installation

You can install the development version of multipleITScontrol from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("herts-phei/multipleITScontrol")
```

## Pre-requisites

In order for the package functions to work properly, there are several
requirements to be met for the data frame containing the time series
data:

- To successfully implement an ITS model, data must be collected
  consistently before and after the intervention, which must be
  implemented at a defined point in time. For this package, a minimum of
  3 equal interval time points in each intervention period and in the
  pre-intervention period are needed

- Defined impact model whether the interventions were expected to result
  in a sudden change (a “level” or “step” change, or both); currently,
  the package only supports the same impact model for each intervention.

- Power calculations to ensure sufficient statistical power in the
  intervention of interest should be performed, but this is beyond the
  scope of the package and should be performed independently.

Examples of how to use the package can be found in the vignette here.
