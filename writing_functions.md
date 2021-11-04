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

    ##  [1]  0.51372672  1.18240765 -1.11804500  1.32213627  0.18015272 -1.69920348
    ##  [7]  0.05795153 -0.36744829 -1.06657494 -0.73882026  1.16498339  0.57542762
    ## [13] -1.26838951  1.77360987 -0.68359033 -0.42037199  0.17456730  0.69521343
    ## [19]  1.17279496  0.58599978  0.68186441 -0.74927130  0.34832435 -2.00900473
    ## [25] -0.30844016

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.51372672  1.18240765 -1.11804500  1.32213627  0.18015272 -1.69920348
    ##  [7]  0.05795153 -0.36744829 -1.06657494 -0.73882026  1.16498339  0.57542762
    ## [13] -1.26838951  1.77360987 -0.68359033 -0.42037199  0.17456730  0.69521343
    ## [19]  1.17279496  0.58599978  0.68186441 -0.74927130  0.34832435 -2.00900473
    ## [25] -0.30844016

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -1.38795096 -0.25625471 -0.63981376 -0.06487860 -0.90282777 -0.01602177
    ##  [7] -0.76729238 -0.51392521  0.35362982 -0.12205752 -0.39116067  0.41547580
    ## [13]  2.28635149  0.52901940  0.49453224  0.40598023 -2.01088963  2.01638565
    ## [19] -1.40808944  0.26959783  0.63681606  0.19390138  0.27107776  0.39403239
    ## [25] -0.12778864  1.33417017 -0.43780760  1.91274990  0.02229651 -1.84628510
    ## [31]  0.14279503 -0.32458441 -0.18615248 -1.13936515  1.13163140 -1.61330263
    ## [37] -0.02453029  1.42502064 -0.82618056  0.77169559

How great is this?

Let’s try again - add conditions

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
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

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "Heidi"))
```

    ## Error in z_scores(c("my", "name", "is", "Heidi")): x needs to be numeric

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

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -1.38795096 -0.25625471 -0.63981376 -0.06487860 -0.90282777 -0.01602177
    ##  [7] -0.76729238 -0.51392521  0.35362982 -0.12205752 -0.39116067  0.41547580
    ## [13]  2.28635149  0.52901940  0.49453224  0.40598023 -2.01088963  2.01638565
    ## [19] -1.40808944  0.26959783  0.63681606  0.19390138  0.27107776  0.39403239
    ## [25] -0.12778864  1.33417017 -0.43780760  1.91274990  0.02229651 -1.84628510
    ## [31]  0.14279503 -0.32458441 -0.18615248 -1.13936515  1.13163140 -1.61330263
    ## [37] -0.02453029  1.42502064 -0.82618056  0.77169559

## Multiple outputs

``` r
mean_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
    }
  
  if (length(x) <3) {
    stop("x should have at least 3 numbers")
  }

  mean_x = mean(x)
  sd_x = sd(x)
        
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x)

  return(output_df)
  
}

mean_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.26  2.81

``` r
mean_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.282

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.15  3.31

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  # do checks on inputs
  
sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
    )

}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.73  3.85

``` r
sim_mean_sd = function(n, mu = 40, sigma = 3) {
  
  # do checks on inputs
  
sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
    )

}

sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.0  3.17
