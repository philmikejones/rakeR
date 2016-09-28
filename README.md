
[![Build Status](https://travis-ci.org/philmikejones/rakeR.svg?branch=master)](https://travis-ci.org/philmikejones/rakeR) [![codecov](https://codecov.io/gh/philmikejones/rakeR/branch/master/graph/badge.svg)](https://codecov.io/gh/philmikejones/rakeR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rakeR)](https://cran.r-project.org/package=rakeR)

rakeR
=====

Create a spatial microsimulated data set in R using iterative proportional fitting ('raking').

Install
-------

Install the stable version from CRAN:

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

Raking
------

To perform the raking you should supply two data frames, one with the constraint information with counts per category for each zone (e.g. census counts) and one with individual--level data (i.e. one row per individual). In addition supply a character vector with constraint variable names.

``` r
cons <- data.frame(
  "zone"   = letters[1:3],
  "a0_49"  = c(8, 2, 7),
  "a_gt50" = c(4, 8, 4),
  "f"      = c(6, 6, 8),
  "m"      = c(6, 4, 3)
)

inds <- data.frame(
  "id"     = LETTERS[1:5],
  "age"    = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
  "sex"    = c("m", "m", "m", "f", "f"),
  "income" = c(2868, 2474, 2231, 3152, 2473),
  stringsAsFactors = FALSE
)

vars <- c("age", "sex")
```

-   (Re-)weighting is done with `weight()` which returns a data frame of fractional weights.
-   Integerisation is performed with `integerise()` which returns a data frame of integerised weights.
-   `simulate()` takes care of creating the final microsimulated data and returns a data frame of simulated cases in zones.

These functions can be combined with pipes:

``` r
# obtain magrittr if not already installed
# install.packages("magrittr")
library("magrittr")

sim_df <- weight(cons, inds, vars) %>% integerise() %>% simulate(inds = inds)
head(sim_df)
#>     id    age sex income zone
#> 1    A a_gt50   m   2868    a
#> 2    B a_gt50   m   2474    a
#> 3    C  a0_49   m   2231    a
#> 3.1  C  a0_49   m   2231    a
#> 3.2  C  a0_49   m   2231    a
#> 3.3  C  a0_49   m   2231    a
```

Alternatively use the `rake()` function, which is a wrapper for `weight() %>% integerise() %>% simulate()`:

``` r
sim_df <- rake(cons, inds, vars)
head(sim_df)
#>     id    age sex income zone
#> 1    A a_gt50   m   2868    a
#> 2    B a_gt50   m   2474    a
#> 3    C  a0_49   m   2231    a
#> 3.1  C  a0_49   m   2231    a
#> 3.2  C  a0_49   m   2231    a
#> 3.3  C  a0_49   m   2231    a
```

Contributions
-------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Feedback on the API, [bug reports/issues](https://github.com/philmikejones/rakeR/issues), and pull requests are very welcome.

**Do not** push to `origin/master`! `origin/master` is a protected branch and expects CI tests to have been successfully completed before it will merge code.

Develop in a new branch, check your changes with `devtools::check()`, and submit a pull request which will be checked by Travis CI.

Acknowledgements
----------------

Many of the functions in this package are based on code written by [Robin Lovelace](https://github.com/Robinlovelace) and [Morgane Dumont](https://github.com/modumont) for their book [*Spatial Microsimulation with R* (2016), Chapman and Hall/CRC Press](https://www.crcpress.com/Spatial-Microsimulation-with-R/Lovelace-Dumont/p/book/9781498711548), licensed under the terms below:

Copyright (c) 2014 Robin Lovelace

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Their book is also an excellent resource for learning about spatial microsimulation and understanding what's going on under the hood of this package.

The rewighting (ipfp) algorithm itself is [written by Andrew Blocker](https://github.com/awblocker/ipfp) and is written in `C` for maximum speed and efficiency.

Thanks to [Tom Broomhead](http://mhs.group.shef.ac.uk/members/tom-broomhead/) for his feedback on error handling and suggestions on function naming.

Contact
-------

philmikejones at gmail dot com

License
-------

Copyright 2016 Phil Mike Jones.

rakeR is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

rakeR is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with rakeR. If not, see <http://www.gnu.org/licenses/>.
