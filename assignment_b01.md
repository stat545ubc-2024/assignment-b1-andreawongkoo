Assignment B1 - Andrea Wong Koo
================
2024-10-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(dplyr)
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
#Data packages for examples and tests:
library(survival)
library(palmerpenguins)
```

# Exercise 1: Make a Function and Exercise 2: Document your Function

The following is the code for my created function
`summarize_category_count()`, with `roxygen2` tags for function
documentation.

*Extra coding explanations:* `deparse(substitute())` was used to convert
variable name into character string for verifying data class.

``` r
#' Title: Function for Summarizing Category Instances (summarize_category_count())
#' This function counts instances of a specific category within a categorical variable, grouped by a group variable. It supports both factor and discrete numeric variable types, and displays the results in a summary table. 
#' 
#' @param data An input data frame containing the variables to analyze.
#' @param group_var The grouping variable to summarize counts by. Named this way to clarify its role in grouping.
#' @param cat_var The categorical variable of interest. This name is a concise reference to "category variable".
#' @param cat The specific category value to be counted. This parameter directly refers to the target category.
#' @param na.rm A reusable variable indicating whether to ignore NA values in the data. Defined globally to allow flexibility in handling missing values, while reducing redundancy.
#' @param .groups A reusable variable to control the behavior of grouping (e.g., "drop"), as in `dplyr::summarize()`. Defined globally to allow flexibility and reduce redundancy if changing its assignment.
#'
#' @return A summary table with the counts for the chosen category per group.
#' @export
#'
#' @examples
#' 

summarize_category_count <- function(data, group_var, cat_var, cat, na.rm = TRUE, .groups = "drop") {
  
  # To capture the names of the variables for easier use in the plot: 
  group_var_name <- deparse(substitute(group_var)) 
  cat_var_name <- deparse(substitute(cat_var))

  cat_var_data <- data[[cat_var_name]] # Accessing variable from the data
  
  # To ensure that the input variables are part of the dataset:  
  if (!(group_var_name %in% names(data))) {
    stop(paste("Error: The grouping variable", group_var_name, "is not a column in the dataset.")) 
  }
  
  if (!(cat_var_name %in% names(data))) {
    stop(paste("Error: The category variable", cat_var_name, "is not a column in the dataset."))
  }

  # To ensure that the category variable is only a factor or integer numeric type:
  if (!is.factor(cat_var_data) && 
      !(is.numeric(cat_var_data) && 
        all(cat_var_data == as.integer(cat_var_data), na.rm = na.rm))) {
    stop("Sorry, this function only works for factor or discrete numeric (integer) variables. You have provided an object of class: ", 
         class(cat_var_data)[1], ", or a non-integer numeric type.") 
  }
    
  # To group by the specified variable and count instances of the category: 
  summary_data <- data %>% 
    group_by({{group_var}}) %>% 
    summarize(count = sum({{cat_var}} == cat, na.rm = na.rm), .groups = .groups)
    
  print(summary_data) # To print a summary result table. 
  
}
```

# Exercise 3: Include examples

### Examples with `lung` data:

To use an example with a data set containing a categorical variable with
discrete numbers as categories, we can use the `lung` dataset from the
`survival` package.

``` r
#To explore the `lung` dataframe variables and its documentation: 
glimpse(lung)
```

    ## Rows: 228
    ## Columns: 10
    ## $ inst      <dbl> 3, 3, 3, 5, 1, 12, 7, 11, 1, 7, 6, 16, 11, 21, 12, 1, 22, 16…
    ## $ time      <dbl> 306, 455, 1010, 210, 883, 1022, 310, 361, 218, 166, 170, 654…
    ## $ status    <dbl> 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ age       <dbl> 74, 68, 56, 57, 60, 74, 68, 71, 53, 61, 57, 68, 68, 60, 57, …
    ## $ sex       <dbl> 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, …
    ## $ ph.ecog   <dbl> 1, 0, 0, 1, 0, 1, 2, 2, 1, 2, 1, 2, 1, NA, 1, 1, 1, 2, 2, 1,…
    ## $ ph.karno  <dbl> 90, 90, 90, 90, 100, 50, 70, 60, 70, 70, 80, 70, 90, 60, 80,…
    ## $ pat.karno <dbl> 100, 90, 90, 60, 90, 80, 60, 80, 80, 70, 80, 70, 90, 70, 70,…
    ## $ meal.cal  <dbl> 1175, 1225, NA, 1150, NA, 513, 384, 538, 825, 271, 1025, NA,…
    ## $ wt.loss   <dbl> NA, 15, 15, 11, 0, 0, 10, 1, 16, 34, 27, 23, 5, 32, 60, 15, …

``` r
?lung
```

``` r
#To see a preview of the `lung` data:
head(lung)
```

    ##   inst time status age sex ph.ecog ph.karno pat.karno meal.cal wt.loss
    ## 1    3  306      2  74   1       1       90       100     1175      NA
    ## 2    3  455      2  68   1       0       90        90     1225      15
    ## 3    3 1010      1  56   1       0       90        90       NA      15
    ## 4    5  210      2  57   1       1       90        60     1150      11
    ## 5    1  883      2  60   1       0      100        90       NA       0
    ## 6   12 1022      1  74   1       1       50        80      513       0

**Example 1:** An example that counts the instances of an ECOG
performance rating (`ph.ecog`) by the sex group variable (`sex`).

For the group variable, `sex` is either 1 (Male) or 2 (Female). For the
category variable input, the possible `ph.ecog` categories are the
following: 0=asymptomatic, 1= symptomatic but completely ambulatory, 2=
in bed \<50% of the day, 3= in bed \> 50% of the day but not bedbound, 4
= bedbound. As seen in `?lung` and `glimpse()` above, the categorical
variable (`ph.ecog`) is an integer numeric (`dbl`subtype) class, where
each number corresponds to a rating category.

``` r
# Example that counts the instances of ECOG rating category '0' (asymptomatic) by sex: 
summarize_category_count(data = lung, group_var = sex, cat_var = ph.ecog, cat = 0) 
```

    ## # A tibble: 2 × 2
    ##     sex count
    ##   <dbl> <int>
    ## 1     1    36
    ## 2     2    27

**Example 2:** An example that counts the instances of a censoring
status category (`status`) by the sex group variable (`sex`).

For the group variable, `sex` is either 1 (Male) or 2 (Female). For the
category variable input, the `status` can be 1 (censored) or 2(dead). As
seen in `?lung` and `glimpse()` above, the categorical variable
(`status`) is an integer numeric (`dbl`subtype) class, where each number
corresponds to a status category.

``` r
# Example that counts the instances of status category '1' (censored) by sex: 
summarize_category_count(data = lung, group_var = sex, cat_var = status, cat = 2) 
```

    ## # A tibble: 2 × 2
    ##     sex count
    ##   <dbl> <int>
    ## 1     1   112
    ## 2     2    53

### Example with `mtcars` data:

To create examples using another dataset, we can use the `mtcars`
dataframe.

``` r
#To explore the `mtcars` dataframe variables and its documentation: 
glimpse(mtcars)
```

    ## Rows: 32
    ## Columns: 11
    ## $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,…
    ## $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,…
    ## $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16…
    ## $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180…
    ## $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,…
    ## $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.…
    ## $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18…
    ## $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,…
    ## $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,…
    ## $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,…
    ## $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,…

``` r
?mtcars
```

``` r
#To see a preview of the `lung` data:
head(mtcars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

**Example 3:** An example that counts the instances of an engine type
category (`vs`) by the transmission group variable (`am`).

For the group variable, transmission (`am`) is either 0 (automatic) or 2
(manual). For the categorical variable, the engine type (`vs`) can
either be 0 (V-shaped) or 1 (straight). As seen in `?mtcars` and
`glimpse()` above, the categorical variable (`vs`) is an integer numeric
(`dbl`subtype) class, where each number corresponds to a status
category.

``` r
# Example that counts the instances of engine category '1' (straight) by transmission type:
summarize_category_count(data = mtcars, group_var = am, cat_var = vs, cat = 1)  
```

    ## # A tibble: 2 × 2
    ##      am count
    ##   <dbl> <int>
    ## 1     0     7
    ## 2     1     7

### Example with `penguins` data:

Another dataset we can use is the `mtcars` dataframe.

``` r
#To explore the `penguins` dataframe variables and its documentation: 
glimpse(penguins)
```

    ## Rows: 344
    ## Columns: 8
    ## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
    ## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
    ## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
    ## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
    ## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
    ## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
    ## $ sex               <fct> male, female, female, NA, female, male, female, male…
    ## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…

``` r
?penguins
```

``` r
#To see a preview of the `penguins` data:
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

**Example 4:** An example that counts the instances of an species type
category (`species`) by the sex group variable (`sex`).

For the group variable, sex (`sex`) is either female or male. For the
categorical variable, the species (`species`) can either be Adélie,
Chinstrap and Gentoo. As seen in `?penguins` and `glimpse()` above, the
categorical variable (`species`) is factor type, with different levels
as categories.

``` r
# Example using factor as the categorical variable, counting the instances of the species category "Gentoo" by sex: 
summarize_category_count(data = penguins, group_var = sex, cat_var = species, cat = "Gentoo")
```

    ## # A tibble: 3 × 2
    ##   sex    count
    ##   <fct>  <int>
    ## 1 female    58
    ## 2 male      61
    ## 3 <NA>       5

# Exercise 4: Test the Function

For the test for handling NA values in the data, here is a synthesized
tibble to be used later on:

``` r
modified_lung <- tibble(
  status = c(1, 2, 1, 2, 1, 2, 1, 2, NA, 1),  # Status (1 = alive, 2 = dead)
  age = c(71, 68, 65, 74, 58, 62, 77, 60, 55, 80),  # Age in years
  sex = c(1, 2, 1, 2, 1, 2, 1, NA, 1, 2),  # Sex (1 = male, 2 = female)
  ph.ecog = c(1, 2, NA, 0, 1, 3, 2, 0, NA, 4)  # ECOG performance scores
)
```

The following are test cases to validate the behaviour of the function.

``` r
test_that("Testing category summarization function", {
  # Test for the lung dataset with expected category count
  expect_equal(
    summarize_category_count(data = penguins, group_var = sex, cat_var = species, cat = "Gentoo"), 
    penguins %>%
      group_by(sex) %>%
      summarize(count = sum(species == "Gentoo", na.rm = TRUE), .groups = "drop")
  )
  
  # Test for the starwars dataset expecting an error for the wrong input type (decimal instead of integer):
 expect_error(
        summarize_category_count(data = starwars, group_var = sex, cat_var = birth_year, cat = 41.9), 
        regexp = "Sorry, this function only works for factor or discrete numeric \\(integer\\) variables\\. You have provided an object of class: numeric, or a non-integer numeric type."
    )
 
  # Expects the count to match the summarized data while handling NA values
  expect_equal(
    summarize_category_count(data = modified_lung, group_var = sex, cat_var = ph.ecog, cat = 1, na.rm = TRUE), 
    modified_lung %>%
      group_by(sex) %>%
      summarize(count = sum(ph.ecog == 1, na.rm = TRUE), .groups = "drop")
  ) 
  
  # Expects an error when an input category name is not a column in the data set:
  expect_error(
    summarize_category_count(data = mtcars, group_var = transmission, cat_var = vs, cat = 1),
    regexp = paste("Error: The grouping variable", "transmission", "is not a column in the dataset."))
})
```

    ## # A tibble: 3 × 2
    ##   sex    count
    ##   <fct>  <int>
    ## 1 female    58
    ## 2 male      61
    ## 3 <NA>       5
    ## # A tibble: 3 × 2
    ##     sex count
    ##   <dbl> <int>
    ## 1     1     2
    ## 2     2     0
    ## 3    NA     0
    ## Test passed 🥳
