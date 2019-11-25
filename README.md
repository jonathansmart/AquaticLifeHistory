# AquaticLifeHistory

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/jonathansmart/AquaticLifeHistory.svg?branch=master)](https://travis-ci.org/jonathansmart/AquaticLifeHistory)
<!-- badges: end -->

AquaticLifeHistory allows users to undertake fisheries life history analyses using contemporary approaches as simple R functions. These analyses currently include length-at-age modelling using a multi-model approach, estimating age-at-maturity and length-at-maturity. The main package functions are:
* `Estimate_Growth()` performs length-at-age analyses using a von Bertalanffy growth model, Gompertz model and/or Logistic model.
* `Estimate_Age_Maturity()` Estimates age-at-maturity using either binomial maturity or proportion mature data and age.
* `Estimate_Len_Maturity()` Estimates age-at-maturity using either binomial maturity or proportion mature data and length.

## Installation

You can install the released version of AquaticLifeHistory from [Github](https://github.com/jonathansmart/AquaticLifeHistory) with:

```{r, eval = FALSE}
devtools::install_github("jonathansmart/AquaticLifeHistory")
```
## Usage

Perform growth analysis

```{r, message = FALSE}
library(AquaticLifeHistory)

# load example dataset
data("growth_data")

# Run fuction with three default model candidates.
# Model parameters, AIC values are returned and a plot with bootstrapped CI's is printed to examine fits.


Estimate_Growth(growth_data)

# For further examples of this and others functions use the vignette

browseVignettes("AquaticLifeHistory")
```

Perform age-at-maturity analysis

```{r, message = FALSE}
library(AquaticLifeHistory)

# load example dataset
data("maturity_data")

# Run fuction to estimate age-at-maturity parameters
Estimate_Age_Maturity(maturity_data)

# A plot can also be returned with bootstrapped CI's.
Estimate_Age_Maturity(maturity_data, return = "plot")

# For further examples of this and others functions use the vignette

browseVignettes("AquaticLifeHistory")
```
