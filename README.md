
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oottest

<!-- badges: start -->
<!-- badges: end -->

The goal of oottest is to

## Installation

You can install the development version of oottest from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PhilippKuelpmann/oottest")
```

## Example

This is a basic example on how you can use the package:

(Simple test for 2 theories)

``` r
library(oottest)
data_experiment <- c(1,2,3)
prediction_theory_1 <- c(1/3,1/3,1/3)
prediction_theory_2 <- c(1/4,1/4,1/2)
vuong_statistic(data_experiment, pred_I = prediction_theory_1, pred_J = prediction_theory_2)
```

Multiple theories, creating a Vuong matrix:

TODO: add example

## Citation

Here’s how to cite my package:
