[![Build Status](https://travis-ci.org/philmikejones/rakeR.svg?branch=master)](https://travis-ci.org/philmikejones/rakeR)

Developer warning
=================

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

Basic raking is done with `rake()` and integerisation is done with `trs()`
(truncate, replicate, sample). They can be combined with pipes:

```r
# obtain magrittr if not already installed
install.packages("magrittr")

library("magrittr")
weights <- rake(cons, inds, vars) %>% trs()
weights
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

License
-------

GPLv3. see LICENSE
