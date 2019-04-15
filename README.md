
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/Aariq/holodeck.svg?branch=master)](https://travis-ci.org/Aariq/holodeck)[![codecov](https://codecov.io/gh/Aariq/holodeck/branch/master/graph/badge.svg)](https://codecov.io/gh/Aariq/holodeck)[![DOI](https://zenodo.org/badge/167047376.svg)](https://zenodo.org/badge/latestdoi/167047376)

# holodeck: A Tidy Interface For Simulating Multivariate Data

`holodeck` allows quick and simple creation of simulated multivariate
data with variables that co-vary or discriminate between levels of a
categorical variable. The resulting simulated multivariate dataframes
are useful for testing the performance of multivariate statistical
techniques under different scenarios, power analysis, or just doing a
sanity check when trying out a new multivariate method.

## Installation

`holodeck` is currently not on CRAN, but you can install it from github
with the following R code:

``` r
# install.packages("devtools")
devtools::install_github("Aariq/holodeck")
```

## Load packages

`holodeck` is built to work with `dplyr` functions, including
`group_by()` and the pipe (`%>%`). `purrr` is helpful for iterating
simulated data. For these examples I’ll use `ropls` for PCA and PLS-DA.

``` r
library(holodeck)
library(dplyr)
library(tibble)
library(purrr)
library(ropls)
```

## Example 1: Investigating PCA and PLS-DA

Let’s say we want to learn more about how principal component analysis
(PCA) works. Specifically, what matters more in terms of creating a
principal component—variance or covariance of variables? To this end,
you might create a dataframe with a few variables with high covariance
and low variance and another set of variables with low covariance and
high variance

### Generate data

``` r
set.seed(925)
df1 <- 
  sim_covar(n_obs = 20, n_vars = 5, cov = 0.9, var = 1, name = "high_cov") %>%
  sim_covar(n_vars = 5, cov = 0.1, var = 2, name = "high_var") 
```

Explore covariance structure visually. The diagonal is variance.

``` r
df1 %>% 
  cov() %>%
  heatmap(Rowv = NA, Colv = NA, margins = c(6,6), main = "Covariance")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
#for interactive heatmap, try iheatmapr package:
  # iheatmap(row_labels = TRUE, col_labels = TRUE, name = "cov or var")
```

Now let’s make this dataset a little more complex. We can add a factor
variable, some variables that discriminate between the levels of that
factor, and add some missing values.

``` r
set.seed(501)
df2 <-
  df1 %>% 
  sim_cat(n_groups = 3, name = "factor") %>% 
  group_by(factor) %>% 
  sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(-1.3, 0, 1.3), name = "discr") %>% 
  sim_discr(n_vars = 5, var = 1, cov = 0, group_means = c(0, 0.5, 1), name = "discr2") %>% 
  sim_missing(prop = 0.1) %>% 
  ungroup()
df2
#> # A tibble: 20 x 21
#>    factor high_cov_1 high_cov_2 high_cov_3 high_cov_4 high_cov_5 high_var_1
#>    <chr>       <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1 a           0.472   -0.362       0.253      0.281       0.247    -1.54  
#>  2 a          -1.50    -1.65       NA         -1.93       -1.27      1.00  
#>  3 a           1.13     0.655       0.980      1.41        0.345    -1.90  
#>  4 a           0.982    0.740       1.16       1.14        0.866     1.71  
#>  5 a          -0.773   -1.31       -1.22      -1.21       -1.25     -0.576 
#>  6 a           0.302   NA          -0.309      0.0725      0.725     2.25  
#>  7 a          NA        0.00163     0.0596    -0.542      -0.269     2.87  
#>  8 b           2.16     2.47        1.38      NA          NA         0.146 
#>  9 b          NA       NA          -0.529     -0.842      -1.04     NA     
#> 10 b           0.609    0.195       0.720      0.930       0.595     0.0765
#> 11 b           1.81     1.15        1.43       1.09        1.39     -0.927 
#> 12 b           0.954    0.234       0.247      0.248       0.751     2.77  
#> 13 b          -1.03    -1.24       -1.70      -1.27       -1.64      1.34  
#> 14 b          -0.180   NA           0.177      0.433      NA         1.20  
#> 15 c          -0.214   -0.390      -0.476     -0.878      -0.328     3.18  
#> 16 c           0.827    0.556       0.620      0.491       0.814     1.91  
#> 17 c          -0.399   -0.862      -0.385     -0.935      NA        -0.787 
#> 18 c          -1.09    NA          -0.720     -1.88       -1.76     -1.76  
#> 19 c          -0.181   -0.155      -0.774      0.0395     NA         0.741 
#> 20 c           0.882    0.366       0.758      1.24        0.838     0.182 
#> # … with 14 more variables: high_var_2 <dbl>, high_var_3 <dbl>,
#> #   high_var_4 <dbl>, high_var_5 <dbl>, discr_1 <dbl>, discr_2 <dbl>,
#> #   discr_3 <dbl>, discr_4 <dbl>, discr_5 <dbl>, discr2_1 <dbl>,
#> #   discr2_2 <dbl>, discr2_3 <dbl>, discr2_4 <dbl>, discr2_5 <dbl>
```

### PCA

``` r
pca <- opls(select(df2, -factor), printL = FALSE, plotL = FALSE)
  
plot(pca, parAsColFcVn = df2$factor, typeVc = "x-score")
#> Warning: Character 'parAsColFcVn' set to a factor

getLoadingMN(pca) %>%
  as_tibble(rownames = "variable") %>% 
  arrange(desc(abs(p1)))
#> # A tibble: 20 x 4
#>    variable         p1       p2        p3
#>    <chr>         <dbl>    <dbl>     <dbl>
#>  1 high_cov_3  0.444   -0.0223  -0.0178  
#>  2 high_cov_4  0.412    0.0233   0.0301  
#>  3 high_cov_1  0.403    0.0335  -0.0248  
#>  4 high_cov_5  0.389    0.00518 -0.00792 
#>  5 high_cov_2  0.372    0.00949 -0.000838
#>  6 discr2_1    0.225   -0.0295   0.487   
#>  7 high_var_2 -0.201   -0.140   -0.0215  
#>  8 discr_1    -0.180   -0.309    0.0888  
#>  9 discr2_4    0.124   -0.230   -0.235   
#> 10 discr_5     0.121   -0.418   -0.00171 
#> 11 high_var_3  0.121    0.0872  -0.208   
#> 12 discr_4    -0.0761  -0.402    0.0376  
#> 13 high_var_5 -0.0535  -0.00698 -0.393   
#> 14 high_var_4  0.0457  -0.110   -0.544   
#> 15 discr2_3    0.0330  -0.198   -0.00447 
#> 16 discr_2     0.0207  -0.373    0.111   
#> 17 high_var_1 -0.0199   0.0715  -0.0182  
#> 18 discr2_2   -0.00666  0.182   -0.419   
#> 19 discr2_5    0.00402 -0.324   -0.0749  
#> 20 discr_3     0.00217 -0.389   -0.0967
```

It looks like PCA mostly picks up on the variables with high covariance,
**not** the variables that discriminate among levels of `factor`. This
makes sense, as PCA is an unsupervised
analysis.

### PLS-DA

``` r
plsda <- opls(select(df2, -factor), df2$factor, printL = FALSE, plotL = FALSE, predI = 2)

plot(plsda, typeVc = "x-score")

getVipVn(plsda) %>% 
  tibble::enframe(name = "variable", value = "VIP") %>% 
  arrange(desc(VIP))
#> # A tibble: 20 x 2
#>    variable     VIP
#>    <chr>      <dbl>
#>  1 discr_5    1.76 
#>  2 discr_2    1.64 
#>  3 discr_4    1.58 
#>  4 discr_3    1.50 
#>  5 discr_1    1.30 
#>  6 discr2_5   1.16 
#>  7 discr2_4   1.05 
#>  8 high_cov_2 0.995
#>  9 high_var_5 0.989
#> 10 high_cov_1 0.872
#> 11 discr2_3   0.819
#> 12 discr2_2   0.727
#> 13 high_var_2 0.680
#> 14 high_var_3 0.494
#> 15 discr2_1   0.469
#> 16 high_cov_3 0.450
#> 17 high_cov_4 0.406
#> 18 high_var_4 0.203
#> 19 high_var_1 0.202
#> 20 high_cov_5 0.105
```

PLS-DA, a supervised analysis, finds discrimination among groups and
finds that the discriminating variables we generated are most
responsible for those differences.
