v 0.1.2.90000
=============

* integerise() now returns weights unmodified with a note if weights are
already integers (issues [#42](https://github.com/philmikejones/rakeR/issues/42) and [#46](https://github.com/philmikejones/rakeR/issues/46))
* set.seed() is no longer hard-coded in the integerise() function and can be 
specified as a function argument
([#41](https://github.com/philmikejones/rakeR/issues/41))

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
