Cross Validation
================
Adam Whalen
11/23/2020

## Simulate data

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )
```

Look at the data

``` r
nonlin_df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

## Cross validation: by hand

Get training and testing datasets

``` r
train_df = sample_n(nonlin_df, size = 80)
test_df = anti_join(nonlin_df, train_df, by = "id")
```

Fit three models.

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = gam(y ~ s(x), data = train_df)
wiggly_mod = gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

Can I see what I just did?

``` r
train_df %>% 
  add_predictions(linear_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
train_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-2.png" width="90%" />

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-3.png" width="90%" />

``` r
train_df %>% 
  gather_predictions(linear_mod, smooth_mod, wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  facet_grid(. ~ model)
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-5-4.png" width="90%" />

Look at prediction accuracy.

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.7052956

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

## Cross validation using `modelr`

``` r
cv_df = 
  crossv_mc(nonlin_df, 100)
```

What is happening here?

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as_tibble()
```

    ## # A tibble: 79 x 3
    ##       id      x       y
    ##    <int>  <dbl>   <dbl>
    ##  1     1 0.266   1.11  
    ##  2     2 0.372   0.764 
    ##  3     3 0.573   0.358 
    ##  4     4 0.908  -3.04  
    ##  5     6 0.898  -1.99  
    ##  6     7 0.945  -3.27  
    ##  7     8 0.661  -0.615 
    ##  8     9 0.629   0.0878
    ##  9    10 0.0618  0.392 
    ## 10    11 0.206   1.63  
    ## # … with 69 more rows

``` r
cv_df %>% pull(test) %>% .[[1]] %>% as_tibble()
```

    ## # A tibble: 21 x 3
    ##       id      x      y
    ##    <int>  <dbl>  <dbl>
    ##  1     5 0.202   1.33 
    ##  2    12 0.177   0.836
    ##  3    19 0.380   0.982
    ##  4    22 0.212   0.710
    ##  5    28 0.382   0.932
    ##  6    31 0.482   0.498
    ##  7    37 0.794  -1.12 
    ##  8    42 0.647   0.158
    ##  9    47 0.0233 -0.148
    ## 10    56 0.0995  1.13 
    ## # … with 11 more rows

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
```

Let’s try to fit models and get RMSEs for them.

``` r
cv_df = 
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(y ~ x, data = .x)),
    smooth_mod = map(.x = train, ~gam(y ~ s(x), data = .x)),
    wiggly_mod = map(.x = train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))
  ) %>% 
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2_dbl(.x = wiggly_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

What do these results say about the model choice?

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

Compute averages

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  group_by(model) %>% 
  summarize(avg_rmse = mean(rmse))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 3 x 2
    ##   model  avg_rmse
    ##   <chr>     <dbl>
    ## 1 linear    0.718
    ## 2 smooth    0.289
    ## 3 wiggly    0.354

## Try on a real dataset

Import data

``` r
child_growth_df = 
  read_csv("./data/nepalese_children.csv") %>% 
  mutate(
    weight_cp = (weight > 7) * (weight < 7)
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   age = col_double(),
    ##   sex = col_double(),
    ##   weight = col_double(),
    ##   height = col_double(),
    ##   armc = col_double()
    ## )

weight vs arm circumference

``` r
 child_growth_df %>% 
  ggplot(aes(x = weight, y = armc)) +
  geom_point(alpha = 0.3)
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

Fit the models I care about.

``` r
linear_mod = lm(armc ~ weight, data = child_growth_df)
pwlin_mod = lm(armc ~ weight + weight_cp, data = child_growth_df)
smooth_mod = gam(armc ~ s(weight), data = child_growth_df)
```

``` r
child_growth_df %>% 
  gather_predictions(linear_mod, pwlin_mod, smooth_mod) %>% 
  ggplot(aes(x = weight, y = armc)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), color = "red") +
  facet_grid(. ~ model)
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

<img src="cross_validation_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

Try to understand model fit using cross validation.

``` r
cv_df = 
  crossv_mc(child_growth_df, 100) %>% 
  mutate(
    train = map(train, as_tibble),
    train = map(test, as_tibble)
  )
```

See if I can fit the models to the splits.

``` r
cv_df = 
  cv_df %>% 
  mutate(
    linear_mod = map(.x = train, ~lm(armc ~ weight, data = .x)),
    smooth_mod = map(.x = train, ~gam(armc ~ s(weight), data = .x)),
    pwlin_mod = map(.x = train, ~lm(armc ~ weight + weight_cp, data = .x))
  ) %>% 
  mutate(
    rmse_linear = map2_dbl(.x = linear_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y)),
    rmse_pwlin = map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))
  )
```

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_smooth`.
    ## ℹ not all required variables have been supplied in  newdata!
    ## 
    ## ℹ Input `rmse_smooth` is `map2_dbl(.x = smooth_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.gam(model, data): not all required variables have been supplied in  newdata!

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

    ## Warning: Problem with `mutate()` input `rmse_pwlin`.
    ## ℹ prediction from a rank-deficient fit may be misleading
    ## ℹ Input `rmse_pwlin` is `map2_dbl(.x = pwlin_mod, .y = test, ~rmse(model = .x, data = .y))`.

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit may be
    ## misleading

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" />

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_"
  ) %>% 
  group_by(model) %>% 
  summarize(avg_rmse = mean(rmse))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 3 x 2
    ##   model  avg_rmse
    ##   <chr>     <dbl>
    ## 1 linear    0.793
    ## 2 pwlin     0.793
    ## 3 smooth    0.764
