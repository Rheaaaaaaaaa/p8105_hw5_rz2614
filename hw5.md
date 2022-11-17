hw5
================
Ruihan Zhang
2022-11-16

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(dplyr)
library(purrr)
library(broom)
```

``` r
homicide= read_csv("./homicide-data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(city_state = str_c(city,",",state))
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

There are 52179 observations. The variables are uid, reported_date,
victim_last, victim_first, victim_race, victim_age, victim_sex, city,
state, lat, lon, disposition, city_state. The dimension is 13 \* 52179.

``` r
homicides_number =  homicide %>% 
  group_by(city) %>% 
  summarise(homicides_no = n())
unsolved = homicide %>% 
  group_by(city)  %>% 
  filter(., disposition == "Closed without arrest" | disposition == "Open/No arrest" ) %>% summarise(unsolved_no = n())
sum = merge(homicides_number, unsolved, by = "city")
```

``` r
sink("test_output")
test = prop.test(x = 1825, n = 2827) 
sink()
tidy = broom::tidy(test)
tidy
```

    ## # A tibble: 1 × 8
    ##   estimate statistic  p.value parameter conf.low conf.high method        alter…¹
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>         <chr>  
    ## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample pro… two.si…
    ## # … with abbreviated variable name ¹​alternative

``` r
pt = sum %>% 
  mutate(value = map2(unsolved_no, homicides_no, ~ prop.test(.x, .y) %>% 
  tidy())) %>% 
  unnest(value) %>% 
  select(., city, homicides_no, unsolved_no, estimate, conf.low, conf.high)
```

``` r
p = pt %>% 
  mutate(city = fct_reorder(city, estimate)) %>% 
  ggplot(aes(x= city, y = estimate)) +geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high),width = 0.2) +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + labs(title = "the estimates and CIs for each city",
    x = "esitimates",
    y = "confidential interval")
```

``` r
sim_mean_sd = function(size = 30, mu , sigma = 5) {
  
  sim_data = tibble(
    x = rnorm(n = size, mean = mu, sd = sigma),
  )
  
  tt = t.test(pull(sim_data,x), conf.level = 0.95) %>% 
    broom::tidy() %>% 
    select(estimate,p.value)
  tt
   sim_data %>% 
     summarise(
      mu_hat = mean(x),
      reject = case_when(
        pull(tt, p.value) < 0.05 ~ mean(x),
        pull(tt, p.value) >= 0.05 ~ as.numeric("")
      )
    )
}
```

``` r
sim_results_df = function(size = 30, mu, sigma = 5) {
  output = vector(mode = "list", length = 5000)
  mu = c(0, 1, 2, 3, 4, 5, 6)
  for (i in 1:5000) {
     output[[i]] = sim_mean_sd(size = 30, mu, sigma = 5)
  }
  output %>% 
  bind_rows() %>% 
    summarize(
      sample = mean(mu_hat, na.rm = TRUE),
      reject_sa = mean(reject, na.rm = TRUE)
    )
}
```

``` r
muhat =
  tibble(
    mu = c(0, 1, 2, 3, 4, 5, 6),
    hat = map(mu, sim_results_df)
  ) %>%  
  unnest(hat) %>% 
  pivot_longer(
    sample:reject_sa,
    names_to = "sample",
    values_to = "estimate_mean"
  )
```
