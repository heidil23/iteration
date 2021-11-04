Writing Functions
================

Load libraries that you will need:

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.31869997  0.47916346  1.90969295  1.02774958 -1.87191493 -0.34073155
    ##  [7] -0.19727954 -0.16300081  0.11668891 -1.22035594  0.18263878 -0.13422028
    ## [13] -1.73224764  0.53343049  0.96309696 -0.36648259  0.29056087 -1.13583909
    ## [19]  0.42461814  0.05482881  1.80243353 -1.44276475 -0.15255630  0.44731025
    ## [25] -0.79351929

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  1.31869997  0.47916346  1.90969295  1.02774958 -1.87191493 -0.34073155
    ##  [7] -0.19727954 -0.16300081  0.11668891 -1.22035594  0.18263878 -0.13422028
    ## [13] -1.73224764  0.53343049  0.96309696 -0.36648259  0.29056087 -1.13583909
    ## [19]  0.42461814  0.05482881  1.80243353 -1.44276475 -0.15255630  0.44731025
    ## [25] -0.79351929

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  2.28410441 -0.28702017  0.09718118 -0.29410934 -0.05277528  0.71031801
    ##  [7]  1.57466493  0.99066726 -0.42598247 -0.86103142  1.00484193  0.26785048
    ## [13] -0.95674343  0.41373803 -1.30833524 -1.90666513  0.47045747 -1.48913526
    ## [19]  0.63512543  0.39000657 -2.40079546 -1.60141750 -0.07805880  1.23147885
    ## [25] -1.00357682  0.23648498  1.76845056 -1.26686474  0.15546177  0.23830186
    ## [31] -0.13450237 -0.35854482  1.02268094 -0.19270898  0.42002780  0.20099801
    ## [37] -0.29830910  0.40794859  0.78794246 -0.39215519

How great is this?

Let’s try again - add conditions

``` r
z_scores = function(x) {
  
  if (~is.numeric(x)) {
    stop("x needs to be numeric")
    }
  
  if (length(x) <3) {
    stop("x should have at least 3 numbers")
  }

  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

Use error = TRUE to allow it to continue to knit despite errors

``` r
z_scores(3)
```

    ## Warning in if (~is.numeric(x)) {: the condition has length > 1 and only the
    ## first element will be used

    ## Error in if (~is.numeric(x)) {: argument is not interpretable as logical

``` r
z_scores(c("my", "name", "is", "Heidi"))
```

    ## Warning in if (~is.numeric(x)) {: the condition has length > 1 and only the
    ## first element will be used

    ## Error in if (~is.numeric(x)) {: argument is not interpretable as logical

``` r
mtcars
```

    ##                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    ## Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    ## Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    ## Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    ## Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    ## Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    ## Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    ## Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    ## Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    ## Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    ## Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    ## Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

``` r
z_scores(mtcars)
```

    ## Warning in if (~is.numeric(x)) {: the condition has length > 1 and only the
    ## first element will be used

    ## Error in if (~is.numeric(x)) {: argument is not interpretable as logical

``` r
z_scores(x = y_vec)
```

    ## Warning in if (~is.numeric(x)) {: the condition has length > 1 and only the
    ## first element will be used

    ## Error in if (~is.numeric(x)) {: argument is not interpretable as logical
