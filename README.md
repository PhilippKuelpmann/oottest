
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oottest: Out-of-Treatment Testing in R

<!-- badges: start -->

[![R-CMD-check](https://github.com/PhilippKuelpmann/oottest/workflows/R-CMD-check/badge.svg)](https://github.com/PhilippKuelpmann/oottest/actions)
[![Codecov test
coverage](https://codecov.io/gh/PhilippKuelpmann/oottest/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PhilippKuelpmann/oottest?branch=master)
<!-- badges: end -->

oottest implements the out-of-treatment testing from Kuelpmann and
Kuzmics (2020). Out-of treatment testing allows for a direct, pairwise
likelihood comparison of theories, calibrated with pre-existing data.

## Installation

You can install the development version of oottest from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PhilippKuelpmann/oottest")
```

## Example

Input data should be structured in the following way: *columns represent
different treatments *rows represent actions \*cells record the number
of subjects who chose each action on each treatment

Prediction data should be structured in the following way: - columns
represent different treatments - rows represent the predicted
probability of each action - the different tables represent the
different theories - cells record the probability of choosing an action
on each treatment depending on the theory

Here is a basic example on how you can use the vuong_statistic using
predictions from two theories:

``` r
library(oottest)
data_experiment <- c(1,2,3)
prediction_theory_1 <- c(1/3,1/3,1/3)
prediction_theory_2 <- c(1/4,1/4,1/2)
vuong_statistic(data_experiment, pred_I = prediction_theory_1, pred_J = prediction_theory_2)
```

Here is a basic example how to compare three theories, using data from
two treatments:

``` r
library(oottest)
treatment_1 <- c(1,2,3)
treatment_2 <- c(3,2,1)
data_experiment <- data.frame(treatment_1, treatment_2)
theory_1 <- matrix(c(1/3,1/3,1/3, 1/3, 1/3, 1/3), nrow = 3, ncol=2)
theory_2 <- matrix(c(1/4,1/4,1/2,1/2,1/4,1/4), nrow = 3, ncol=2)
theory_3 <- matrix(c(1/3,1/3,1/3, 1/4,1/4,1/2), nrow = 3, ncol=2)
theories <- array(c(theory_1,theory_2,theory_3), dim=c(3,2,3))
vuong_matrix(data_experiment, theories)
```
