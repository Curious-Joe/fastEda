
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastEda

<!-- badges: start -->

[![R-CMD-check](https://github.com/Curious-Joe/fastEda/workflows/R-CMD-check/badge.svg)](https://github.com/Curious-Joe/fastEda/actions)

<!-- badges: end -->

`fastEda` is intended to make repeated plotting tasks easier and faster.
Using the available functions users will be able to quickly create
multiple plots on similar data to perform common tasks such as:

  - Plot individual feature/variable,
  - Visualize plots to explore bi-variate relations between target
    feature and input features.
    
<img src="https://curious-joe.net/img/fastEda_demo.gif" width="60%" height="60%" />

## Installation

`fastEda` is currently only available in GitHub. Install the package
with:

``` r
# install.packages("devtools")
devtools::install_github("Curious-Joe/fastEda")
```

## Example

  - Plot barplots showing ratio of the target labels/categories in all
    the available categorical features in the dataset:

<!-- end list -->

``` r
library(fastEda)
library(dplyr)
biv_bar_plot(dataset = iris %>%
               mutate(Sepal_Width_Categorie = ifelse(Sepal.Width < 
                                              mean(iris$Sepal.Width), 'Low', 'High')),
             classVar = Species, order = c("setosa", "virginica", "versicolor"), 
             colors = c("#5a4fcf", "#9890f0", "#d3d0f5"),
             barType = "fill")
```
