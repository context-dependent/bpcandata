
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bpcandata

The goal of bpcandata is to make StatCan public use microdata files
easier to access and work with in R.

## Installation

You can install the development version of bpcandata from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("context-dependent/bpcandata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bpcandata)
library(tidyverse)
#> Warning: package 'ggplot2' was built under R version 4.3.3
#> Warning: package 'purrr' was built under R version 4.3.3
#> Warning: package 'stringr' was built under R version 4.3.2
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

## Loading the data

`fetch_lfs_pumf` downloads the LFS PUMF files for a given year (or
vector of multiple years) and extracts them to a specfied `cache_dir`.
In this case, we download the 2015 LFS PUMF files to the `"./local"`
directory. `read_lfs_pumf` reads the LFS PUMF files from the specified
directory, returning a list of two data frames: `records` and
`codebook`.

``` r
fetch_lfs_pumf(2015, cache_dir = "local")
#> Skipping 2015 as it is already cached, set refresh_cache = TRUE to force download.
d15 <- read_lfs_pumf("local/2015")

d15$records |> head()
#> # A tibble: 6 × 60
#>   REC_NUM SURVYEAR SURVMNTH LFSSTAT  PROV   CMA AGE_12 AGE_6   SEX MARSTAT  EDUC
#>     <int>    <int>    <int>   <int> <int> <int>  <int> <int> <int>   <int> <int>
#> 1       1     2015        1       1    35     0      4    NA     2       1     4
#> 2       2     2015        1       1    47     0      5    NA     1       1     3
#> 3       3     2015        1       1    24     2      9    NA     2       2     2
#> 4       4     2015        1       1    35     0      7    NA     1       1     5
#> 5       5     2015        1       4    47     0      1     2     1       6     2
#> 6       6     2015        1       1    35     0      8    NA     2       6     4
#> # ℹ 49 more variables: MJH <int>, EVERWORK <int>, FTPTLAST <int>,
#> #   COWMAIN <int>, IMMIG <int>, NAICS_21 <int>, NOC_10 <int>, NOC_43 <int>,
#> #   YABSENT <int>, WKSAWAY <int>, PAYAWAY <int>, UHRSMAIN <dbl>,
#> #   AHRSMAIN <dbl>, FTPTMAIN <int>, UTOTHRS <dbl>, ATOTHRS <dbl>,
#> #   HRSAWAY <dbl>, YAWAY <int>, PAIDOT <int>, UNPAIDOT <int>, XTRAHRS <dbl>,
#> #   WHYPT <int>, TENURE <int>, PREVTEN <int>, HRLYEARN <dbl>, UNION <int>,
#> #   PERMTEMP <int>, ESTSIZE <int>, FIRMSIZE <int>, DURUNEMP <int>, …

d15$codebook |> head()
#> # A tibble: 6 × 6
#>   field_id var_name var_label               var_universe is_factor factor_labels
#>      <dbl> <chr>    <chr>                   <chr>        <lgl>     <list>       
#> 1        1 REC_NUM  Order of record in file All respond… FALSE     <NULL>       
#> 2        2 SURVYEAR Survey year             All respond… FALSE     <NULL>       
#> 3        3 SURVMNTH Survey month            All respond… TRUE      <tibble>     
#> 4        4 LFSSTAT  Labour force status     All respond… TRUE      <tibble>     
#> 5        5 PROV     Province                All respond… TRUE      <tibble>     
#> 6        6 CMA      Nine largest CMAs       All respond… TRUE      <tibble>
```

To view the codebook in a human readable format, run
`?bpcandata::read_lfs_pumf` in the R console.

## Applying the codebook

``` r
d15_labelled <- encode_lfs_factors(d15$records, d15$codebook)

d15_labelled |> head()
#> # A tibble: 6 × 63
#>   REC_NUM SURVYEAR SURVMNTH LFSSTAT PROV  CMA   AGE_12 AGE_6 SEX   MARSTAT EDUC 
#>     <int>    <int> <fct>    <fct>   <fct> <fct> <fct>  <fct> <fct> <fct>   <fct>
#> 1       1     2015 January  Employ… Onta… "Oth… 30 to… <NA>  Fema… Married Post…
#> 2       2     2015 January  Employ… Sask… "Oth… 35 to… <NA>  Male  Married Some…
#> 3       3     2015 January  Employ… Queb… "Mon… 55 to… <NA>  Fema… Living… High…
#> 4       4     2015 January  Employ… Onta… "Oth… 45 to… <NA>  Male  Married Bach…
#> 5       5     2015 January  Not in… Sask… "Oth… 15 to… 17 t… Male  Single… High…
#> 6       6     2015 January  Employ… Onta… "Oth… 50 to… <NA>  Fema… Single… Post…
#> # ℹ 52 more variables: MJH <fct>, EVERWORK <fct>, FTPTLAST <fct>,
#> #   COWMAIN <fct>, IMMIG <fct>, NAICS_21 <fct>, NOC_10 <fct>, NOC_43 <fct>,
#> #   YABSENT <fct>, WKSAWAY <int>, PAYAWAY <fct>, UHRSMAIN <dbl>,
#> #   AHRSMAIN <dbl>, FTPTMAIN <fct>, UTOTHRS <dbl>, ATOTHRS <dbl>,
#> #   HRSAWAY <dbl>, YAWAY <fct>, PAIDOT <int>, UNPAIDOT <int>, XTRAHRS <dbl>,
#> #   WHYPT <fct>, TENURE <int>, PREVTEN <int>, HRLYEARN <dbl>, UNION <fct>,
#> #   PERMTEMP <fct>, ESTSIZE <fct>, FIRMSIZE <fct>, DURUNEMP <int>, …
```

`encode_lfs_factors` applies the codebook to the records data frame,
converting the factor codes to their corresponding labels.

## Point estimates using FINALWT

If you aren’t concerned with sampling variability, you can use the
`FINALWT` column as frequency weights to calculate totals, means, and
proportions.

``` r
d15_labelled |>
  group_by(SURVMNTH, LFSSTAT) |>
  summarize(
    n = sum(FINALWT)
  ) |>
  pivot_wider(names_from = LFSSTAT, values_from = n)
#> `summarise()` has grouped output by 'SURVMNTH'. You can override using the
#> `.groups` argument.
#> # A tibble: 12 × 5
#> # Groups:   SURVMNTH [12]
#>    SURVMNTH  `Employed, at work` `Employed, absent from work` Unemployed
#>    <fct>                   <int>                        <int>      <int>
#>  1 January              16195012                      1316472    1358135
#>  2 February             16103924                      1475163    1380739
#>  3 March                15713623                      1892118    1412204
#>  4 April                16411223                      1265165    1375486
#>  5 May                  16831618                      1231144    1363788
#>  6 June                 17018908                      1227734    1261098
#>  7 July                 15706899                      2466187    1398522
#>  8 August               15332438                      2851254    1485246
#>  9 September            16763420                      1258502    1238407
#> 10 October              16758742                      1282076    1217614
#> 11 November             16755255                      1178192    1250002
#> 12 December             16668916                      1166873    1295898
#> # ℹ 1 more variable: `Not in labour force` <int>
```

## Estimating sampling variability with bootstrap weights

If sampling variability is important, generate appropriately calibrated
bootstrap replicate weights with `generate_lfs_bootstrap_weights`.

``` r
repwt15 <- generate_lfs_bootstrap_weights(d15_labelled, 100)
```

## Tabulating bootstrap estimates

`bpcandata` provides functions for tabulating grouped totals and means
with bootstrap replicate weights.

``` r
d15_labelled$REPWT <- repwt15

d15_labelled |>
  group_by(SURVMNTH, LFSSTAT) |>
  summarize(
    n = lfs_bs_total(REPWT, FINALWT)
  ) |>
  unpack(n, names_sep = "_") |>
  head()
#> `summarise()` has grouped output by 'SURVMNTH'. You can override using the
#> `.groups` argument.
#> # A tibble: 6 × 5
#> # Groups:   SURVMNTH [2]
#>   SURVMNTH LFSSTAT                       n_est       n_var   n_se
#>   <fct>    <fct>                         <int>       <dbl>  <dbl>
#> 1 January  Employed, at work          16195012 2906828247. 53915.
#> 2 January  Employed, absent from work  1316472  632345157. 25146.
#> 3 January  Unemployed                  1358135  769634165. 27742.
#> 4 January  Not in labour force        10102410 2577011925. 50764.
#> 5 February Employed, at work          16103924 3806993027. 61701.
#> 6 February Employed, absent from work  1475163  857402687. 29281.

d15_labelled |>
  group_by(SURVMNTH) |>
  filter(
    LFSSTAT %in% c("Unemployed", "Not in labour force"),
    EVERWORK != "No, never worked"
  ) |>
  summarize(mean_wks_away = lfs_bs_mean(DURJLESS, REPWT, FINALWT)) |>
  unpack(mean_wks_away, names_sep = "_") |>
  head()
#> # A tibble: 6 × 4
#>   SURVMNTH mean_wks_away_est mean_wks_away_var mean_wks_away_se
#>   <fct>                <dbl>             <dbl>            <dbl>
#> 1 January               98.7             0.288            0.536
#> 2 February              99.4             0.320            0.566
#> 3 March                 99.0             0.345            0.587
#> 4 April                 99.6             0.267            0.517
#> 5 May                  104.              0.424            0.651
#> 6 June                 105.              0.405            0.636
```
