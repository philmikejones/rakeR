[![Build Status](https://travis-ci.org/philmikejones/rakeR.svg?branch=master)](https://travis-ci.org/philmikejones/rakeR)

Developer note
==============

Do not push to **`origin/master`**!

**`origin/master`** is a protected branch and expects CI tests to have been
successfully completed before it will merge code.

Develop in a new branch (suggest `origin/develop`), check your changes with
`devtools::check()`, and submit a pull request to be checked by Travis CI.


rakeR
=====

Create a spatial microsimulated data set in R using iterative proportional 
fitting ('raking').


Install
-------

Install the development version with `devtools`:

```r
# Obtain devtools if you don't already have it installed
install.packages("devtools")

# Install rakeR development version from GitHub
devtools::install_github("philmikejones/rakeR")
library("rakeR")
```


Raking
------

To perform the raking you should supply two data frames, one with the constraint
information with counts per category for each zone (e.g. census counts) and one
with individual--level data (i.e. one row per individual).
In addition supply a character vector with constraint variable names.

Basic raking is done with `weight()`. Integerisation is performed with
`integerise()`, for now using the truncate, replicate, sample method.
Finally, `simulate()` takes care of creating the final microsimulated data set.
They can be combined with pipes:

```r
# obtain magrittr if not already installed
install.packages("magrittr")

library("magrittr")
sim_df <- weight(cons, inds, vars) %>% integerise() %>% simulate(cases = inds)
sim_df
```


Acknowledgements
----------------

Many of the functions in this package are based on code written by 
[Robin Lovelace](https://github.com/Robinlovelace) and 
[Morgane Dumont](https://github.com/modumont) for their book 
[*Spatial Microsimulation with R* (2016), Chapman and Hall/CRC Press](https://www.crcpress.com/Spatial-Microsimulation-with-R/Lovelace-Dumont/p/book/9781498711548).

The ipfp algorithm itself is 
[written by Andrew Blocker](https://github.com/awblocker/ipfp) 
and is written in `C` for maximum speed and efficiency.


Contact
-------

philmikejones at gmail dot com


Licenses
--------

Copyright 2016 Phil Mike Jones.

rakeR is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

rakeR is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with rakeR. If not, see <http://www.gnu.org/licenses/>.

See [LICENSE](https://github.com/philmikejones/rakeR/blob/master/LICENSE)

This package includes source code available under open source licenses:

Copyright (C) 2014 Robin Lovelace:
[spatial-microsim-book](https://github.com/Robinlovelace/spatial-microsim-book)
