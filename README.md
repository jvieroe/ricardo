
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ricardo <img src='man/figures/logo.png' align="right" height="189" />

<!-- badges: start -->

[![CodeFactor](https://www.codefactor.io/repository/github/jvieroe/ricardo/badge)](https://www.codefactor.io/repository/github/jvieroe/ricardo)
[![R-CMD-check](https://github.com/jvieroe/ricardo/workflows/R-CMD-check/badge.svg)](https://github.com/jvieroe/ricardo/actions)
<!-- badges: end -->

| Country | Leage          | Code |
|---------|----------------|------|
| England | Premier League | E0   |
| England | Championship   | E1   |
| England | League 1       | E2   |
| England | League 2       | E3   |
| England | Conference     | E4   |

``` r
library(ggplot2)

df <- mtcars

ggplot(data = df, aes(x = wt, y = mpg)) + 
  geom_point()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="85%" style="display: block; margin: auto;" />
