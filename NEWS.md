v 0.2.2
=======

* check_constraint() and check_ind() are now deprecated. These checks are carried out by the weight() and/or integerise()/extract() functions automatically.
* Update README.md documentation
* Add additional unit tests - thanks Derrick Atherton for feedback
* Add appropriate acknowledgements for source of data set used for examples and testing.

v 0.2.1
=======

Patch release:

* Fixes to examples and tests to use correct variable labels and names (thanks Derrick Atherton for pointing this out).
* Fix version number in NEWS

v 0.2.0
=======

* integerise() now uses the wrswoR package for sampling without replacement.
This is in the order of 100s of times faster, reducing the time taken for the function to return from hours to minutes.
I have implemented the sample_int_crank() function as this has given results
similar to that of base R's sample() in testing, so this should minimise changes
between the two.
See https://stackoverflow.com/questions/15113650/faster-weighted-sampling-without-replacement or https://cran.r-project.org/package=wrswoR for details of the
implementation.
* simulate() is deprecated. Instead of weight() %>% integerise() %>% simulate(),
just use weight() %>% integerise(). This is to improve consistency with the 
steps to produce fractional weights (weight() %>% extract()).
* extract_weights() has been deprecated. Use extract() instead.
* extract() (previously extract_weights()) now stops if it encounters a numeric
variable. See https://github.com/philmikejones/rakeR/issues/49
* integerise() now returns weights unmodified with a note if they are
already integers. See https://github.com/philmikejones/rakeR/issues/42 and https://github.com/philmikejones/rakeR/issues/46
* set.seed() is no longer hard-coded in the integerise() function and can be 
specified as a function argument. See:
https://github.com/philmikejones/rakeR/issues/41

v 0.1.2
=======

Patch release

New functions
-------------

* check_ind() checks individual level data for common errors


Fixes:
------

* Fix weight() #33: https://github.com/philmikejones/rakeR/issues/33
* check functions now named prep functions to prevent conflicts with validation
* Add date to DESCRIPTION for bibliography building


v 0.1.1
=======

Initial CRAN release

Fixes:
------

* Fix license issue: remove LICENSE, state GPL-3


v 0.1.0
=======

New functions
-------------

* weight(),
* integerise(),
* simulate(),
* rake(), and
* check_constraint()
