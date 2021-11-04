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

    ##  [1] -0.479127991 -0.375116740  0.177481710  0.434379925 -1.135565071
    ##  [6]  0.867493217 -0.572866043 -1.169233624  0.227469841 -0.875392974
    ## [11]  0.000440966 -1.504978982  2.911015494 -1.101061245 -0.488305385
    ## [16]  1.346499022  0.928053395  0.139256816 -0.344913580  0.244432115
    ## [21]  0.537190849 -0.187939310  0.172797705 -1.181272260  1.429262150

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.479127991 -0.375116740  0.177481710  0.434379925 -1.135565071
    ##  [6]  0.867493217 -0.572866043 -1.169233624  0.227469841 -0.875392974
    ## [11]  0.000440966 -1.504978982  2.911015494 -1.101061245 -0.488305385
    ## [16]  1.346499022  0.928053395  0.139256816 -0.344913580  0.244432115
    ## [21]  0.537190849 -0.187939310  0.172797705 -1.181272260  1.429262150

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  1.38044851 -0.74240997 -1.02223030  0.28102806  0.22027017 -1.93532266
    ##  [7] -1.40251799 -0.27835557  0.02148907  1.04361750  0.59688577  0.04338207
    ## [13] -1.62077817 -0.71151404  0.19861229 -0.27041957 -0.49768189 -0.48663740
    ## [19] -0.25164879  1.92139526 -1.46661073  0.51117090 -0.69503315  0.27703555
    ## [25] -1.42310012  0.68605073  1.96344723 -0.54607260  0.29232207  1.26072345
    ## [31] -0.34963619  1.98622931 -1.05970405  2.00163569  0.21177805  0.14528877
    ## [37]  0.02322113  0.01598993 -0.02716235 -0.29518599

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

    ##  [1]  1.38044851 -0.74240997 -1.02223030  0.28102806  0.22027017 -1.93532266
    ##  [7] -1.40251799 -0.27835557  0.02148907  1.04361750  0.59688577  0.04338207
    ## [13] -1.62077817 -0.71151404  0.19861229 -0.27041957 -0.49768189 -0.48663740
    ## [19] -0.25164879  1.92139526 -1.46661073  0.51117090 -0.69503315  0.27703555
    ## [25] -1.42310012  0.68605073  1.96344723 -0.54607260  0.29232207  1.26072345
    ## [31] -0.34963619  1.98622931 -1.05970405  2.00163569  0.21177805  0.14528877
    ## [37]  0.02322113  0.01598993 -0.02716235 -0.29518599

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
    ## 1  5.07  3.80

``` r
mean_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.290

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
    ## 1  2.33  2.82

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
    ## 1  4.61  3.12

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
    ## 1  39.8  2.66

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are numerous pages with reviews

Write a function that gets reviews based on page url.

``` r
get_page_reviews = function(page_url){
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  return(reviews)
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                       stars       text                                 
    ##    <chr>                       <chr>       <chr>                                
    ##  1 I Just everyone to know th… 5.0 out of… "\n  VOTE FOR PEDRO !!!!! LOL\n"     
    ##  2 the cobweb in his hair dur… 5.0 out of… "\n  5 stars for being the best wors…
    ##  3 Best quirky movie ever      5.0 out of… "\n  You all know the awesome movie …
    ##  4 Classic Film                5.0 out of… "\n  Had to order this for our young…
    ##  5 hehehehe                    5.0 out of… "\n  goodjobboys\n"                  
    ##  6 Painful                     1.0 out of… "\n  I think I sneezed during the mo…
    ##  7 GRAND                       5.0 out of… "\n  GRAND\n"                        
    ##  8 Hello, 90s                  5.0 out of… "\n  So nostalgic movie\n"           
    ##  9 Cult Classic                5.0 out of… "\n  Watched it with my older grandc…
    ## 10 Format was inaccurate       4.0 out of… "\n  There was an option to choose b…

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                    stars       text                                    
    ##    <chr>                    <chr>       <chr>                                   
    ##  1 Good funny               3.0 out of… "\n  Would recommend\n"                 
    ##  2 Not available w/in 48 h… 1.0 out of… "\n  I couldn't watch it and there is n…
    ##  3 Your mom went to colleg… 5.0 out of… "\n  Classic funny movie. It has some o…
    ##  4 Very funny movie         5.0 out of… "\n  I watch this movie with my family.…
    ##  5 Watch it twice! Trust m… 5.0 out of… "\n  Nothing to dislike!  Cult Classic …
    ##  6 A classic                5.0 out of… "\n  If you don’t enjoy this movie, we …
    ##  7 Can't say how many time… 5.0 out of… "\n  Such a great movie. Will never get…
    ##  8 I pity the fool who doe… 5.0 out of… "\n  I love technology but not as much …
    ##  9 I don’t know why it’s s… 2.0 out of… "\n  My girlfriend loves it!\n"         
    ## 10 Okay                     3.0 out of… "\n  Okay\n"

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                     stars       text                                   
    ##    <chr>                     <chr>       <chr>                                  
    ##  1 A WHOLESOME comedic jour… 5.0 out of… "\n  Not a moment of this movie is spa…
    ##  2 Hilarious                 5.0 out of… "\n  Funny\n"                          
    ##  3 Love it                   5.0 out of… "\n  What of the funniest movies\n"    
    ##  4 WORTH IT!                 5.0 out of… "\n  It's the dry humor for me. I coul…
    ##  5 Funny movie.              5.0 out of… "\n  Great comedy\n"                   
    ##  6 Best movie ever!          5.0 out of… "\n  Got this for my sister who loves …
    ##  7 I was stuck in the oil p… 5.0 out of… "\n  I watched this serially. It was s…
    ##  8 Funny Dork humor          5.0 out of… "\n  Humor that is funnier when you wa…
    ##  9 Still funny!              5.0 out of… "\n  Still funny!\n"                   
    ## 10 Love it!! 💜              5.0 out of… "\n  Love it!! 💜\n"

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                       stars       text                                 
    ##    <chr>                       <chr>       <chr>                                
    ##  1 I Just everyone to know th… 5.0 out of… "\n  VOTE FOR PEDRO !!!!! LOL\n"     
    ##  2 the cobweb in his hair dur… 5.0 out of… "\n  5 stars for being the best wors…
    ##  3 Best quirky movie ever      5.0 out of… "\n  You all know the awesome movie …
    ##  4 Classic Film                5.0 out of… "\n  Had to order this for our young…
    ##  5 hehehehe                    5.0 out of… "\n  goodjobboys\n"                  
    ##  6 Painful                     1.0 out of… "\n  I think I sneezed during the mo…
    ##  7 GRAND                       5.0 out of… "\n  GRAND\n"                        
    ##  8 Hello, 90s                  5.0 out of… "\n  So nostalgic movie\n"           
    ##  9 Cult Classic                5.0 out of… "\n  Watched it with my older grandc…
    ## 10 Format was inaccurate       4.0 out of… "\n  There was an option to choose b…
    ## # … with 40 more rows
