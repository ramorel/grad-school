Analysis of group racial theat in the Anti-Testing Movement
================

### Setup - Data and libraries

The data were previously imported from SQLITE databases provided by [the
New York state Department of
Education](https://data.nysed.gov/downloads.php) and from the Common
Core of Data provided by\[ [the National Center for Education
Statistics](https://nces.ed.gov/ccd/pubschuniv.asp)

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
    ## ✔ tibble  1.4.2          ✔ dplyr   0.7.5     
    ## ✔ tidyr   0.8.1          ✔ stringr 1.3.1     
    ## ✔ readr   1.1.1          ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(plm)
```

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'plm'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lag, lead

``` r
library(sandwich)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(broom)
options(scipen=0)

load(here::here("/databases/ny_cleaned_data1807.RData"))

# NOTE: Conflict between plm and dplyr--both have `lag` and `lead`
```

### Preparing the panel data

I need to clean the data, keeping only the relevant variables and
observations. Following that, I need to do some feature engineering to
create overall school-level accountability data.

First, I will select the relevant variables. These are variables related
to testing and student demographics.

``` r
ny_panel <- 
  nydata %>% 
  select(entity_cd:math_white_num_enroll, 
         total_enroll:district_name,
         county_cd:per_multi,
         black_ccd_district, am_ccd_district,
         asian_ccd_district, hisp_ccd_district, 
         white_ccd_district, pacific_ccd_district,
         matches("black|hisp|white|asian|_all"),
         contains("total"),
         contains("enroll"),
         contains("partic"),
         contains("ayp"),
         -matches("economically|limited|disabil|multir"),
         num_teach:per_mas_plus, 
         type:ulocal,
         fte:stitli,
         member:tr,
         per_frpl:per_non_wh08,
         district_under18:resolution
         ) 
```

Oddly, there are some duplicates. I need to remove these.

``` r
ny_panel <- 
  ny_panel %>% 
  unite(id, entity_cd, year, remove = F) %>% 
  filter(!duplicated(id)) %>% 
  select(-id)
```

Now, I need to exclude charter schools and keep schools that are still
open (`type==`), are not secondary schools (`level != 3`), and has a
high grade of at least fifth grade (`gshi`).

``` r
ny_panel <- 
  ny_panel %>% 
  filter(charter != 1,
         type == 1,
         level != 3,
         !gshi %in% c("02", "01",
                      "03", "04",
                      "KG", "PK"))
```

#### Feature engineering

First, I want to determine *overall* proficiency levels. I hypothesize
that schools that experienced decreases in proficiency levels in the
previous year will have more students not participarting in annual
testing in the current year.

There is no variable for the overall proficiency rate of a school. So I
need to create it. Proficient means scoring a level 3 or 4.
Non-proficient means scoring a level 1 or 2. We have the number of
students scoring at each level for each grade, as well as the number of
students tested. I will use these to create two variables:
`ela_prof_rate` and `math_prof_rate`.

``` r
prof_rate <- function(df, n, subject) {
  for (i in n) {
    varname <- paste("overall",
                     "level", 
                     i, i + 1, 
                     subject,
                     "count", 
                     sep = "_")
    rgx <- paste0(subject,
                  "_(\\d)_all(\\S+)",
                  "(", i, "|", i + 1, ")",
                  "_count")
    df <- mutate(df, 
                 !!varname := 
                   rowSums(df %>% 
                             select(
                               matches(rgx)),
                           na.rm = T))
  }
  nc <- ncol(df)
  varname <- paste("overall",
                     "level", 
                     n[1], n[1] + 1, 
                     subject,
                     "count", 
                     sep = "_")
  newname <- paste(subject,
                   "non_prof_rate",
                   sep = "_")
  df <- 
    df %>% 
    mutate(!!newname :=
             select(df, 
                    one_of(!!varname)) %>% 
             pull() / 
             rowSums(df %>% 
                       select((nc-1):nc),
           na.rm = T)) %>% 
    select(-nc, -(nc-1))
}

for (i in c("ela", "math")) {
  ny_panel <- prof_rate(ny_panel, c(1, 3), i)
} 
```

I believe that the geographical setting of the school may matter for the
rates of non-participation, so I will use the `ulocal` variable to
create a dummy variable called `suburban` and one called `urban`. And I
want to relevel `ulocal` so suburban is the reference, just in case I
decided to use that variable.

``` r
table(ny_panel$ulocal)
```

    ## 
    ##     city    rural suburban     town 
    ##    11406     4059     8715     1989

``` r
ny_panel <- 
  ny_panel %>% 
  mutate(suburban = ifelse(ulocal %in% c("town", "suburban"), 
                           1, 0),
         urban = ifelse(ulocal == "city", 
                           1, 0)) %>% 
  mutate(ulocal = fct_relevel(as_factor(ulocal), "suburban"))
```

The last thing I need to do is to turn the participation variables into
*non-particpation* variables. This is more intuitive for the analysis I
will do.

``` r
ny_panel <-
  ny_panel %>% 
  rename_at(vars(ends_with("per_partic")),
            funs(. = str_replace(., 
                                 "(ela|math)(_[a-z]+_)(\\S+)(per_partic)$", 
                                 "\\1\\2\\4")
                 )
            ) %>% 
  mutate_at(vars(ends_with("per_partic")),
            funs("non_partic" = 1 - .)) %>% 
  rename_at(vars(ends_with("non_partic")),
            funs(. = str_remove(., "per_partic_"))
            )
```

#### Analysis

First, I will establish my base model and do an old-fashioned, run of
the mill OLS regression. Since the data cover several year, this is a
“pooled” ols regression.

``` r
base_ela_model <-
  as.formula(
    ela_all_non_partic ~
      per_black_hisp +
      lag(ela_non_prof_rate) +
      per_fewer_3yrs_exp +
      per_mas_plus +
      per_frpl +
      per_lep +
      st_ratio +
      urban
  )
pooled_ela <- plm(base_ela_model,
                  data = ny_panel %>% filter(year >= 2012),
                  index = c("entity_cd", "year"),
                  model = "pooling")
summary(pooled_ela,
        vcov = vcovHC(pooled_ela,
                      type = "HC1"))
```

    ## Pooling Model
    ## 
    ## Note: Coefficient variance-covariance matrix supplied: vcovHC(pooled_ela, type = "HC1")
    ## 
    ## Call:
    ## plm(formula = base_ela_model, data = ny_panel %>% filter(year >= 
    ##     2012), model = "pooling", index = c("entity_cd", "year"))
    ## 
    ## Unbalanced Panel: n = 2926, T = 1-4, N = 11270
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.255777 -0.070327 -0.022851  0.031223  0.872365 
    ## 
    ## Coefficients:
    ##                           Estimate  Std. Error  t-value  Pr(>|t|)    
    ## (Intercept)            -2.1699e-02  9.5817e-03  -2.2646   0.02355 *  
    ## per_black_hisp         -7.7362e-02  5.7550e-03 -13.4424 < 2.2e-16 ***
    ## lag(ela_non_prof_rate)  3.0035e-01  8.6097e-03  34.8850 < 2.2e-16 ***
    ## per_fewer_3yrs_exp      5.9284e-02  1.3483e-02   4.3968 1.108e-05 ***
    ## per_mas_plus            1.2318e-01  7.1577e-03  17.2090 < 2.2e-16 ***
    ## per_frpl               -1.3217e-01  8.4556e-03 -15.6305 < 2.2e-16 ***
    ## per_lep                -1.0687e-01  9.8184e-03 -10.8842 < 2.2e-16 ***
    ## st_ratio               -6.9624e-05  5.2801e-04  -0.1319   0.89510    
    ## urban                  -5.9184e-02  4.0567e-03 -14.5893 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    228.82
    ## Residual Sum of Squares: 166.67
    ## R-Squared:      0.27159
    ## Adj. R-Squared: 0.27107
    ## F-statistic: 358 on 8 and 2925 DF, p-value: < 2.22e-16

As we can see from the pooled OLS regression, schools with larger
populations for black or latinx students, low income students, and
English language learners had fewer students opting out of ELA testing.
Urban schools had fewer students opting out than non-urban schools.
Schools with more experienced teachers and larger in non-proficiency
rates in the pervious year had more students opting out. These estimates
are biased for all sorts of reasons, chief among them is omitted
variables bias. My guess is that schools with larger populations of
black or latinx students differ systematically from those with smaller
populations of black or latinx students in unobserved ways.

One method to deal with this is to use school fixed effects. This means
that we only identify the relationship between the outcome variable and
the independent variables based on changes that occur *within* schools.
This partials out any unobserved differences between schools.

``` r
entity_ela <- plm(base_ela_model,
                  data = ny_panel %>% filter(year >= 2012),
                  index = c("entity_cd", "year"),
                  model = "within")
summary(entity_ela,
        vcov = vcovHC(entity_ela,
                      type = "HC1"))
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Note: Coefficient variance-covariance matrix supplied: vcovHC(entity_ela, type = "HC1")
    ## 
    ## Call:
    ## plm(formula = base_ela_model, data = ny_panel %>% filter(year >= 
    ##     2012), model = "within", index = c("entity_cd", "year"))
    ## 
    ## Unbalanced Panel: n = 2926, T = 1-4, N = 11270
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.5460911 -0.0419367 -0.0065751  0.0372251  0.5120808 
    ## 
    ## Coefficients:
    ##                           Estimate  Std. Error t-value  Pr(>|t|)    
    ## per_black_hisp          1.3164e+00  9.6787e-02 13.6010 < 2.2e-16 ***
    ## lag(ela_non_prof_rate)  3.6301e-01  8.4415e-03 43.0032 < 2.2e-16 ***
    ## per_fewer_3yrs_exp      5.3320e-02  2.3822e-02  2.2383 0.0252309 *  
    ## per_mas_plus            9.6811e-02  2.9231e-02  3.3119 0.0009305 ***
    ## per_frpl                2.2789e-01  1.9804e-02 11.5071 < 2.2e-16 ***
    ## per_lep                 3.9766e-01  6.7362e-02  5.9033 3.702e-09 ***
    ## st_ratio               -3.7623e-05  1.2046e-03 -0.0312 0.9750849    
    ## urban                  -8.1638e-02  3.0376e-02 -2.6876 0.0072104 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    133.67
    ## Residual Sum of Squares: 100.77
    ## R-Squared:      0.24612
    ## Adj. R-Squared: -0.019133
    ## F-statistic: 284.33 on 8 and 2925 DF, p-value: < 2.22e-16

Now with fixed effects included, we have a different story. The
coefficients on `per_black_hisp`, `per_frpl`, and `per_lep` are now
positive and significant. The coefficient on `lag(ela_non_prof_rate)` is
basically the same–a bit larger, but still positive. So now we know that
schools that had increases in the share of black or hispanic, low
income, or English learning students had more opting out. Interesting.

But there is another thing. The school fixed effects take account for
unobserved variables that vary across schools but are fixed over time.
There may be year to year changes that impact all schools that are
relevant to our analysis. In other words, we need to account for
unobserved variables that vary across years but are fixed across
schools. For example, maybe New York state instituted changes to testing
that increased opting out.

To address this we can include year fixed effects.

``` r
year_ela <- plm(base_ela_model,
                  data = ny_panel %>% filter(year >= 2012),
                  index = c("entity_cd", "year"),
                  model = "within",
                  effect = "twoway")
summary(year_ela,
        vcov = vcovHC(year_ela,
                      type = "HC1"))
```

    ## Twoways effects Within Model
    ## 
    ## Note: Coefficient variance-covariance matrix supplied: vcovHC(year_ela, type = "HC1")
    ## 
    ## Call:
    ## plm(formula = base_ela_model, data = ny_panel %>% filter(year >= 
    ##     2012), effect = "twoway", model = "within", index = c("entity_cd", 
    ##     "year"))
    ## 
    ## Unbalanced Panel: n = 2926, T = 1-4, N = 11270
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.517028 -0.042415  0.000000  0.043559  0.423343 
    ## 
    ## Coefficients:
    ##                           Estimate  Std. Error t-value  Pr(>|t|)    
    ## per_black_hisp          0.91894894  0.07896507 11.6374 < 2.2e-16 ***
    ## lag(ela_non_prof_rate)  0.39043999  0.02443770 15.9770 < 2.2e-16 ***
    ## per_fewer_3yrs_exp      0.00058585  0.02348757  0.0249 0.9801010    
    ## per_mas_plus            0.04936898  0.02463562  2.0040 0.0451059 *  
    ## per_frpl                0.14142543  0.01430502  9.8864 < 2.2e-16 ***
    ## per_lep                 0.15270657  0.07069680  2.1600 0.0307995 *  
    ## st_ratio                0.00533692  0.00109843  4.8587 1.203e-06 ***
    ## urban                  -0.06992895  0.02061538 -3.3921 0.0006969 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    74.546
    ## Residual Sum of Squares: 65.689
    ## R-Squared:      0.11881
    ## Adj. R-Squared: -0.19166
    ## F-statistic: 86.4872 on 8 and 2925 DF, p-value: < 2.22e-16
