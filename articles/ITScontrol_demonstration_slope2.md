# Multiple ITS control introduction for slope change 2nd example (two-stage)

### Usage

This is a basic example which shows you how to solve a common problem
with two stage interrupted time series with a control for a slope
hypothesis:

**Background**: *Albridge Medical Practice* and *Hollybush Medical
Practice* are two medical practices within the same PCN, with similar
populations of people, and prevalence of disease.

*Albridge Medical Practice* wants to try a new intervention to improve
wellbeing in people diagnosed with depression in their practice.

This example is for scenarios where there is a statistically significant
slope change for both interventions, but no level change.

**Intervention 1: Implementing a new Mental Health Support programme**

- **Objective:** Improve mental wellbeing in patients with low-to-mid
  level depression.
- **Start Date:** April 4, 2022
- **Duration:** 2 months
- **Description:** The practice introduced weekly Mindfulness Workshops,
  teaching meditation and breathing techniques to improve
  self-regulation.
- **Measurement:** Self-reported wellbeing scores measured at start and
  end of intervention.

**Intervention 2: Introducing AI led CBT session**

- **Objective:** Further increase self-reported wellbeing scores.
- **Start Date:** June 6, 2022 (immediately after the intervention 1
  program ends)
- **Duration:** 6 months
- **Description:** The practice implements cognitive behavioural therapy
  (CBT) sessions, aimed at changing negative thought patterns and
  behaviours.
- **Measurement:** Self-reported wellbeing scores measured at start and
  end of intervention.

#### Controlled Interrupted Time Series Design (2 stage)

**Step 1: Baseline Period**

- **Duration:** 3 months (Jan 1, 2022 - April 3, 2022)
- **Data Collection:** Collect self-reported wellbeing scores.

**Step 2: Intervention 1 Period**

- **Duration:** 2 months (April 4, 2022 - June 5, 2022)
- **Data Collection:** Continue collecting self-reported wellbeing
  scores at end of workshops.

**Step 3: Intervention 2 Period**

- **Duration:** 6 months (June 6, 2022 - Dec 31, 2022)
- **Data Collection:** Continue collecting self-reported wellbeing
  scores at end of CBT.

The calendar plot below summarises the timeline of the interventions:

![](ITScontrol_demonstration_slope2_files/figure-html/calendar-1.png)

## Step 1) Loading data

Sample data can be loaded from the package for this scenario through the
bundled dataset `its_data_medical_practice`.

  

  

This sample dataset demonstrates the format your own data should be in.

You can observe that in the `Date` column, that the dates are of equal
distance between each element, and that there are two rows for each
date, corresponding to either `control` or `treatment` in the
`group_var` variable. `control` and `treatment` each have three periods,
a `Pre-intervention period` detailing measurements of the outcome prior
to any intervention, the first intervention detailed by
`Intervention 1) Implementing a new Mental Health Support programme`,
and the second intervention, detailed by
`Intervention 2) Introducing CBT session`.

  

## Step 2) Transforming the data

The data frame should be passed to `multipleITScontrol::tranform_data()`
with suitable arguments selected, specifying the names of the columns to
the required variables and starting intervention time points.

``` r
intervention_dates <- c(as.Date("2022-04-04"), as.Date("2022-06-06"))
transformed_data <- 
  multipleITScontrol::transform_data(df = tibble_data,
               time_var = "Date",
               group_var = "group_var",
               outcome_var =  "score",
               intervention_dates = intervention_dates)
```

Returns the initial data frame with a few transformed variables needed
for interrupted time series.

    #> # A tibble: 104 × 13
    #> # Groups:   category [2]
    #>    time       category  Period   outcome     x time_index level_pre_intervention
    #>    <date>     <chr>     <chr>      <dbl> <dbl>      <int>                  <dbl>
    #>  1 2022-01-03 treatment Pre-int…    20       1          1                      1
    #>  2 2022-01-03 control   Pre-int…    15       0          1                      1
    #>  3 2022-01-10 treatment Pre-int…    20.7     1          2                      1
    #>  4 2022-01-10 control   Pre-int…    15.7     0          2                      1
    #>  5 2022-01-17 treatment Pre-int…    21.4     1          3                      1
    #>  6 2022-01-17 control   Pre-int…    16.4     0          3                      1
    #>  7 2022-01-24 treatment Pre-int…    22.1     1          4                      1
    #>  8 2022-01-24 control   Pre-int…    17.1     0          4                      1
    #>  9 2022-01-31 treatment Pre-int…    22.8     1          5                      1
    #> 10 2022-01-31 control   Pre-int…    17.8     0          5                      1
    #> # ℹ 94 more rows
    #> # ℹ 6 more variables: level_1_intervention <dbl>,
    #> #   level_1_intervention_internal <dbl>, slope_1_intervention <dbl>,
    #> #   level_2_intervention <dbl>, level_2_intervention_internal <dbl>,
    #> #   slope_2_intervention <dbl>

## Step 3) Fitting ITS model

The transformed data is then fit using
[`multipleITScontrol::fit_its_model()`](https://herts-phei.github.io/multipleITScontrol/reference/fit_its_model.md).
Required arguments are `transformed_data`, which is simply an unmodified
object created from
[`multipleITScontrol::transform_data()`](https://herts-phei.github.io/multipleITScontrol/reference/transform_data.md)
in the step above; a defined impact model, with current options being
either ‘*slope*’, \`*level*, or ‘*levelslope*’, and the number of
interventions.

``` r
fitted_ITS_model <-
  multipleITScontrol::fit_its_model(transformed_data = transformed_data,
                                    impact_model = "slope",
                                    num_interventions = 2)

fitted_ITS_model
```

Gives a conventional model output from
[`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html).

    #> Generalized least squares fit by REML
    #>   Model: reformulate(termlabels = termlabels, response = "outcome") 
    #>   Data: transformed_data 
    #>   Log-restricted-likelihood: -11.16832
    #> 
    #> Coefficients:
    #>            (Intercept)                      x             time_index 
    #>           1.430000e+01           5.117500e+00           7.000000e-01 
    #>   slope_1_intervention   slope_2_intervention           x:time_index 
    #>          -2.220446e-16           0.000000e+00          -2.766134e-02 
    #> x:slope_1_intervention x:slope_2_intervention 
    #>           1.147794e+00           2.358428e+00 
    #> 
    #> Correlation Structure: ARMA(0,1)
    #>  Formula: ~time_index | x 
    #>  Parameter estimate(s):
    #>    Theta1 
    #> 0.5718797 
    #> Degrees of freedom: 104 total; 96 residual
    #> Residual standard error: 0.2513081

## Step 4) Analysing ITS model

However, the coefficients given do not make intuitive sense to a lay
person. We can call the package’s internal
[`multipleITScontrol::summary_its()`](https://herts-phei.github.io/multipleITScontrol/reference/summary_its.md)
which modifies the summary output by renaming the coefficients to make
them easier to interpret in the context of interrupted time series (ITS)
analysis.

``` r
my_summary_its_model <- multipleITScontrol::summary_its(fitted_ITS_model)

my_summary_its_model
```

    #> Generalized least squares fit by REML
    #>   Model: reformulate(termlabels = termlabels, response = "outcome") 
    #>   Data: transformed_data 
    #>   Log-restricted-likelihood: -11.16832
    #> 
    #> Coefficients:
    #>                           A) Control y-axis intercept 
    #>                                          1.430000e+01 
    #>       B) Pilot y-axis intercept difference to control 
    #>                                          5.117500e+00 
    #>                     C) Control pre-intervention slope 
    #>                                          7.000000e-01 
    #>                       E) Control intervention 1 slope 
    #>                                         -2.220446e-16 
    #>                       I) Control intervention 2 slope 
    #>                                          0.000000e+00 
    #> D) Pilot pre-intervention slope difference to control 
    #>                                         -2.766134e-02 
    #>                         F) Pilot intervention 1 slope 
    #>                                          1.147794e+00 
    #>                         J) Pilot intervention 2 slope 
    #>                                          2.358428e+00 
    #> 
    #> Correlation Structure: ARMA(0,1)
    #>  Formula: ~time_index | x 
    #>  Parameter estimate(s):
    #>    Theta1 
    #> 0.5718797 
    #> Degrees of freedom: 104 total; 96 residual
    #> Residual standard error: 0.2513081

``` r
summary(my_summary_its_model)
```

    #> Generalized least squares fit by REML
    #>   Model: reformulate(termlabels = termlabels, response = "outcome") 
    #>   Data: transformed_data 
    #>        AIC      BIC    logLik
    #>   42.33664 67.98012 -11.16832
    #> 
    #> Correlation Structure: ARMA(0,1)
    #>  Formula: ~time_index | x 
    #>  Parameter estimate(s):
    #>    Theta1 
    #> 0.5718797 
    #> 
    #> Coefficients:
    #>                                                           Value  Std.Error
    #> A) Control y-axis intercept                           14.300000 0.18192499
    #> B) Pilot y-axis intercept difference to control        5.117500 0.25728079
    #> C) Control pre-intervention slope                      0.700000 0.02064355
    #> E) Control intervention 1 slope                        0.000000 0.03780751
    #> I) Control intervention 2 slope                        0.000000 0.02505155
    #> D) Pilot pre-intervention slope difference to control -0.027661 0.02919439
    #> F) Pilot intervention 1 slope                          1.147794 0.05346789
    #> J) Pilot intervention 2 slope                          2.358428 0.03542825
    #>                                                        t-value p-value
    #> A) Control y-axis intercept                           78.60382  0.0000
    #> B) Pilot y-axis intercept difference to control       19.89072  0.0000
    #> C) Control pre-intervention slope                     33.90890  0.0000
    #> E) Control intervention 1 slope                        0.00000  1.0000
    #> I) Control intervention 2 slope                        0.00000  1.0000
    #> D) Pilot pre-intervention slope difference to control -0.94749  0.3458
    #> F) Pilot intervention 1 slope                         21.46697  0.0000
    #> J) Pilot intervention 2 slope                         66.56915  0.0000
    #> 
    #>  Correlation: 
    #>                                                       A)Cy-i BPyidtc C)Cp-s
    #> B) Pilot y-axis intercept difference to control       -0.707               
    #> C) Control pre-intervention slope                     -0.880  0.622        
    #> E) Control intervention 1 slope                        0.661 -0.467  -0.907
    #> I) Control intervention 2 slope                       -0.286  0.203   0.573
    #> D) Pilot pre-intervention slope difference to control  0.622 -0.880  -0.707
    #> F) Pilot intervention 1 slope                         -0.467  0.661   0.641
    #> J) Pilot intervention 2 slope                          0.203 -0.286  -0.405
    #>                                                       E)Ci1s I)Ci2s DPpsdtc
    #> B) Pilot y-axis intercept difference to control                            
    #> C) Control pre-intervention slope                                          
    #> E) Control intervention 1 slope                                            
    #> I) Control intervention 2 slope                       -0.856               
    #> D) Pilot pre-intervention slope difference to control  0.641 -0.405        
    #> F) Pilot intervention 1 slope                         -0.707  0.605 -0.907 
    #> J) Pilot intervention 2 slope                          0.605 -0.707  0.573 
    #>                                                       F)Pi1s
    #> B) Pilot y-axis intercept difference to control             
    #> C) Control pre-intervention slope                           
    #> E) Control intervention 1 slope                             
    #> I) Control intervention 2 slope                             
    #> D) Pilot pre-intervention slope difference to control       
    #> F) Pilot intervention 1 slope                               
    #> J) Pilot intervention 2 slope                         -0.856
    #> 
    #> Standardized residuals:
    #> numeric(0)
    #> attr(,"label")
    #> [1] "Standardized residuals"
    #> 
    #> Residual standard error: 0.2513081 
    #> Degrees of freedom: 104 total; 96 residual

``` r
sjPlot::tab_model(
  my_summary_its_model,
  dv.labels = "Average School Result",
  show.se = TRUE,
  collapse.se = TRUE,
  linebreak = FALSE,
  string.est = "Estimate (std. error)",
  string.ci = "95% CI",
  p.style = "numeric_stars"
)
```

[TABLE]

The predictor coefficients elucidate a few things:

### **Pre-intervention period:**

At the start of the pre-intervention period, ***A)*** ***Control y-axis
intercept*** represents the modelled starting mark of Forest Tiger
School, 14.3.

***C) Control pre-intervention slope*** describes the pre-intervention
slope in the control group (0.7).

***D) Pilot pre-intervention slope difference to control*** describes
the difference in the pre-intervention slope in the control group with
the pilot group. This coefficient is additive to C) ***Control
pre-intervention slope***. I.e. 0.7 (C) + -0.03 (D) = 0.67 is the
pre-intervention slope per x-axis unit in the pilot data.

### **First intervention**:

***E) Control intervention 1 slope*** describes the slope change that
occurs at the intervention break point in the control group at the start
of the first intervention, compared to it’s pre-intervention period (0).

***F) Pilot intervention 1 slope*** describes the difference in the
slope change that occurs at the intervention timepoint in the control
group for the first intervention compared to the pilot (1.15).

These slope changes are pertinent to the slope gradients given in the
pre-intervention period. Thus, we add the coefficients ***E)***
***Control intervention 1 slope** to **C)*** ***Control pre-intervention
slope***: 0 + 0.7 = 0.7 is the average increase for each x-axis unit
during the first intervention for the control data.

To ascertain the slope for the pilot data, we add to the
pre-intervention slope of the pilot data, the coefficients ***E)***
***Control intervention 1 slope*** and ***F)*** ***Pilot intervention 1
slope***. ***E*** (0) + ***F*** (1.15) + ***(C)*** 0.7 + ***D*** -0.03
(D) = 1.82 is the average increase for each x-axis unit during the first
intervention for the pilot data.

To ascertain statistical significance with the first intervention slope,
we call the function’s
[`multipleITScontrol::slope_difference()`](https://herts-phei.github.io/multipleITScontrol/reference/slope_difference.md).

``` r
slope_difference(model = my_summary_its_model, intervention = 1)
```

    #> ## INTERVENTION  1 ## 
    #> 
    #>  Slope for treatment per x-axis unit: 1.82 
    #>  Slope for control per x-axis unit: 0.7 
    #>  Slope difference: 1.12 
    #>  95% CI: 1.06 to 1.18 
    #>  p-value: <0.001 
    #>  Slope control coefficients: E+C 
    #>  Slope treatment coefficients: E+C+D+F 
    #> 
    #> # A tibble: 9 × 3
    #>   Variable                     Value_Raw Value_Formatted
    #>   <chr>                            <dbl> <chr>          
    #> 1 Intervention                  1   e+ 0 1              
    #> 2 Slope for treatment           1.82e+ 0 1.82           
    #> 3 Slope for control             7   e- 1 0.7            
    #> 4 Slope difference              1.12e+ 0 1.12           
    #> 5 Lower 95% CI                  1.06e+ 0 1.06           
    #> 6 Upper 95% CI                  1.18e+ 0 1.18           
    #> 7 p.value                       2.03e-59 <0.001         
    #> 8 Slope treatment coefficients NA        E+C+D+F        
    #> 9 Slope control coefficients   NA        E+C

This brings up the key coefficients and values needed to compare the
slopes of the pilot and control during the first intervention.

We identify that the slope difference between the treatment (Alpine
Meadow School) and the control (Forest Tiger School) for the first
intervention (Reading Programme) has a slope difference of 0.31 (95% CI:
0.29 - 0.32) per x-axis unit, with a p-value below 0.05, indicating
statistical significance.

### **Second intervention:**

***I) Control intervention 2 slope*** describes the slope change that
occurs at the intervention break point in the control group at the start
of the second intervention (0).

Thus, the modelled slope change in the second intervention is ***C)
Control pre-intervention slope*** (0.7) + **E) Control intervention 1
slope** (0) + ***I) Control intervention 2 slope*** (0) = 0.7 is the
average cumulative uptake increase for each x-axis unit during the
second intervention for the control data.

***J) Pilot intervention 2 slope*** describes the difference in the
slope change that occurs at the intervention timepoint in the control
group for the second intervention. (2.36).

These slope changes are pertinent to the slope gradients given in the
pre-intervention and first intervention period. Thus, we add the
coefficients ***C*** (0.7) + ***D*** (-0.03) + ***E*** (0) + ***F***
(1.15) + ***I*** (0) + ***J*** (2.36) = 4.18 is the average cumulative
increase for each x-axis unit during the second intervention for the
pilot data.

To ascertain statistical significance with the second intervention
slope, we call the function’s
[`multipleITScontrol::slope_difference()`](https://herts-phei.github.io/multipleITScontrol/reference/slope_difference.md)
again, but change the intervention parameter.

``` r
slope_difference(model = my_summary_its_model, intervention = 2)
```

    #> ## INTERVENTION  2 ## 
    #> 
    #>  Slope for treatment per x-axis unit: 4.18 
    #>  Slope for control per x-axis unit: 0.7 
    #>  Slope difference: 3.48 
    #>  95% CI: 3.46 to 3.5 
    #>  p-value: <0.001 
    #>  Slope control coefficients: E+C+I 
    #>  Slope treatment coefficients: E+C+D+F+I+J 
    #> 
    #> # A tibble: 9 × 3
    #>   Variable                      Value_Raw Value_Formatted
    #>   <chr>                             <dbl> <chr>          
    #> 1 Intervention                  2   e+  0 2              
    #> 2 Slope for treatment           4.18e+  0 4.18           
    #> 3 Slope for control             7   e-  1 0.7            
    #> 4 Slope difference              3.48e+  0 3.48           
    #> 5 Lower 95% CI                  3.46e+  0 3.46           
    #> 6 Upper 95% CI                  3.50e+  0 3.5            
    #> 7 p.value                       4.50e-156 <0.001         
    #> 8 Slope treatment coefficients NA         E+C+D+F+I+J    
    #> 9 Slope control coefficients   NA         E+C+I

We identify that the slope difference between the treatment (Alpine
Meadow School) and the control (Forest Tiger School) for the first
intervention (Reading Programme) has a slope difference of 0.23 (95% CI:
0.22 - 0.25) per x-axis unit, with a p-value below 0.05, indicating
statistical significance. The effect has been attenuated compared to the
first intervention, and this is evident from the plot in step 6.

## Step 5) Fitting Predictions

We can fit predictions with the created model which project the
pre-intervention period into the post-intervention period by using the
model coefficients using
[`multipleITScontrol::generate_predictions()`](https://herts-phei.github.io/multipleITScontrol/reference/generate_predictions.md).

``` r
transformed_data_with_predictions <- generate_predictions(transformed_data, fitted_ITS_model)

transformed_data_with_predictions
```

### Step 6) Plotting the results

We can use the predicted values and map the segmented regression lines
which compare whether an intervention had a statistically significant
difference.

``` r
its_plot(model = my_summary_its_model,
         data_with_predictions = transformed_data_with_predictions, 
         time_var = "time",
         intervention_dates = intervention_dates)
```

![](ITScontrol_demonstration_slope2_files/figure-html/unnamed-chunk-18-1.png)

In this example, the treatment variable is for *Albridge Medical
Practice*, whilst the control is for *Hollybush Medical Practice*. The
treatment slope shows there was a significant slope change immediately
after the first intervention in April 2022, and in the second
intervention in June 2022.
