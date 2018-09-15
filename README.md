
# rakeR

[![Build
Status](https://travis-ci.org/philmikejones/rakeR.svg?branch=master)](https://travis-ci.org/philmikejones/rakeR)
[![codecov](https://codecov.io/gh/philmikejones/rakeR/branch/master/graph/badge.svg)](https://codecov.io/gh/philmikejones/rakeR)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rakeR)](https://cran.r-project.org/package=rakeR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.821506.svg)](https://doi.org/10.5281/zenodo.821506)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Create a spatial microsimulated data set in R using iterative
proportional fitting (‘raking’).

## Install

Install the latest stable version from CRAN:

``` r
install.packages("rakeR")
```

Or install the development version with `devtools`:

``` r
# Obtain devtools if you don't already have it installed
# install.packages("devtools")

# Install rakeR development version from GitHub
devtools::install_github("philmikejones/rakeR")
```

Load the package with:

``` r
library("rakeR")
#> 
#> Attaching package: 'rakeR'
#> The following object is masked from 'package:stats':
#> 
#>     simulate
```

## Overview

`rakeR` has three main functions. The first stage is always to use
`weight()` to produce a matrix of fractional weights. This matrix stores
weights for each individual in each zone.

From this weight matrix, `rakeR` has functions to create fractional
weights (`extract()`) or integerised cases (`integerise()`), depending
on your needs and use cases. Fractional (`extract()`ed) weights are
generally more accurate, while integer cases are probably the most
intuitive to use and are useful as inputs for further modeling.

To create fractional weights use `weight()` then `extract()`, and to
produce integerised weights use `weight()` then `integerise()`.

## Inputs

To perform the weighting you should supply two data frames. One data
frame should contain the constraint information (`cons`) with counts per
category for each zone (e.g. census counts). The other data frame should
contain the individual–level data (`inds`), i.e. one row per individual.

In addition, it is necessary to supply a character vector with the names
of the constraint variables in `inds` (`vars`). This is so that `rakeR`
knows which are the contraint variables and which variables it should be
simulating as an output.

Below are examples of `cons`, `inds`, and `vars`.

``` r
cons <- data.frame(
  "zone"      = letters[1:3],
  "age_0_49"  = c(8, 2, 7),
  "age_gt_50" = c(4, 8, 4),
  "sex_f"     = c(6, 6, 8),
  "sex_m"     = c(6, 4, 3),
  stringsAsFactors = FALSE
)

inds <- data.frame(
  "id"     = LETTERS[1:5],
  "age"    = c("age_gt_50", "age_gt_50", "age_0_49", "age_gt_50", "age_0_49"),
  "sex"    = c("sex_m", "sex_m", "sex_m", "sex_f", "sex_f"),
  "income" = c(2868, 2474, 2231, 3152, 2473)
)

vars <- c("age", "sex")
```

It is *essential* that the unique levels in the constraint variables in
the `inds` data set match the variables names in the `cons` data set.
For example, `age_0_49` and `age_gt_50` are variable names in `cons` and
the unique levels of the `age` variable in `inds` precisely match these:

``` r
all.equal(
  levels(inds$age), colnames(cons[, 2:3])  # cons[, 1] is the id column
)
#> [1] TRUE
```

Without this, the functions do not know how to match the `inds` and
`cons` data and will fail so as not to return spurious results.

## `weight()`

(Re-)weighting is done with `weight()` which returns a data frame of raw
weights.

``` r
weights <- weight(cons = cons, inds = inds, vars = vars)
weights
#>          a         b         c
#> A 1.227998 1.7250828 0.7250828
#> B 1.227998 1.7250828 0.7250828
#> C 3.544004 0.5498344 1.5498344
#> D 1.544004 4.5498344 2.5498344
#> E 4.455996 1.4501656 5.4501656
```

The raw weights tell you how frequently each individual (`A`-`E`) should
appear in each zone (`a`-`c`). The raw weights are useful when
validating and checking performance of the model, so it can be necessary
to save these separately. They aren’t very useful for analysis however,
so we can `extract()` or `integerise()` them into a useable form.

## `extract()`

`extract()` produces aggregated totals of the simulated data for each
category in each zone. `extract()`ed data is generally more accurate
than `integerise()`d data, although the user should be careful this
isn’t spurious precision based on context and knowledge of the domain.
Because `extract()` creates a column for each level of each variable,
numerical variables (e.g. income) must be removed or `cut()` (otherwise
the result would include a new column for each unique numerical value):

``` r
inds$income <- cut(inds$income, breaks = 2, include.lowest = TRUE,
                   labels = c("low", "high"))

ext_weights <- extract(weights, inds = inds, id = "id")
ext_weights
#>   code total age_0_49 age_gt_50 sex_f sex_m     high      low
#> 1    a    12        8         4     6     6 2.772002 9.227998
#> 2    b    10        2         8     6     4 6.274917 3.725083
#> 3    c    11        7         4     8     3 3.274917 7.725083
```

`extract()` returns one row per zone, and the total of each category
(for example female and male, or high and low income) will match the
known population.

## `integerise()`

The `integerise()` function produces a simulated data frame populated
with simulated individuals. This is typically useful when:

  - You need to include numerical variables, such as income in the
    example.
  - You want individual cases to use as input to a dynamic or
    agent-based model.
  - You want ‘case studies’ to illustrate characteristics of individuals
    in an area.
  - Individual-level data is more intuitive to work with.

<!-- end list -->

``` r
int_weights <- integerise(weights, inds = inds)
int_weights[1:6, ]
#>     id       age   sex income zone
#> 1    A age_gt_50 sex_m   high    a
#> 1.1  A age_gt_50 sex_m   high    a
#> 2    B age_gt_50 sex_m    low    a
#> 3    C  age_0_49 sex_m    low    a
#> 3.1  C  age_0_49 sex_m    low    a
#> 3.2  C  age_0_49 sex_m    low    a
```

`integerise()` returns one row per case, and the number of rows will
match the known population (taken from `cons`).

## `rake()`

`rake()` is a wrapper for `weight() %>% extract()` or `weight() %>%
integerise()`. This is useful if the raw weights (from `weight()`) are
not required. The desired output is specified with the `output`
argument, which can be specified with `"fraction"` (the default) or
`"integer"`. The function takes the following arguments in all cases:

  - `cons`
  - `inds`
  - `vars`
  - `output` (default `"fraction"`)
  - `iterations` (default 10)

Additional arguments are required depending on the output requested. For
`output = "fraction"`:

  - `id`

For `output = "integer"`:

  - `method` (default `"trs"`)
  - `seed` (default 42)

Details of these context-specific arguments can be found in the
respective documentation for `integerise()` or `extract()`.

``` r
rake_int <- rake(cons, inds, vars, output = "integer",
                 method = "trs", seed = 42)
rake_int[1:6, ]
#>     id       age   sex income zone
#> 1    A age_gt_50 sex_m   high    a
#> 1.1  A age_gt_50 sex_m   high    a
#> 2    B age_gt_50 sex_m    low    a
#> 3    C  age_0_49 sex_m    low    a
#> 3.1  C  age_0_49 sex_m    low    a
#> 3.2  C  age_0_49 sex_m    low    a
```

``` r
rake_frac <- rake(cons, inds, vars, output = "fraction", id = "id")
rake_frac
#>   code total age_0_49 age_gt_50 sex_f sex_m     high      low
#> 1    a    12        8         4     6     6 2.772002 9.227998
#> 2    b    10        2         8     6     4 6.274917 3.725083
#> 3    c    11        7         4     8     3 3.274917 7.725083
```

## Contributing

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

## Issues and feedback

Feedback on the API, [bug
reports/issues](https://github.com/philmikejones/rakeR/issues), and pull
requests are very welcome.

## Acknowledgements

Many of the functions in this package are based on code written by
[Robin Lovelace](https://github.com/Robinlovelace) and [Morgane
Dumont](https://github.com/modumont) for their book [*Spatial
Microsimulation with R* (2016), Chapman and Hall/CRC
Press](https://www.crcpress.com/Spatial-Microsimulation-with-R/Lovelace-Dumont/p/book/9781498711548).

Their book is an excellent resource for learning about spatial
microsimulation and understanding what’s going on under the hood of this
package.

The book and code are licensed under the terms below:

Copyright (c) 2014 Robin Lovelace

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

The [rewighting (ipfp) algorithm](https://github.com/awblocker/ipfp) is
written by Andrew Blocker.

The [`wrswoR` package](http://krlmlr.github.io/wrswoR/) used for fast
sampling without replacement is written by Kirill Müller.

Thanks to [Tom
Broomhead](http://mhs.group.shef.ac.uk/members/tom-broomhead/) for his
feedback on error handling and suggestions on function naming, to
[Andrew Smith](https://github.com/virgesmith) for bug fixes, and Derrick
Atherton for suggestions, feedback, and testing.

Data used in some of the examples and tests (‘cakeMap’) are anonymised
data from the [Adult Dental Health
Survey](https://data.gov.uk/dataset/adult_dental_health_survey), used
under terms of the [Open Government
Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

## Contact

philmikejones at gmail dot com

## License

Copyright 2016-18 Phil Mike Jones.

rakeR is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

rakeR is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along
with rakeR. If not, see <http://www.gnu.org/licenses/>.
