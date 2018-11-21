#' weight
#'
#' Deprecated. Use rk_weight()
#'
#' @param cons A data frame containing all the constraints. This
#'   should be in the format of one row per zone, one column per constraint
#'   category. The first column should be a zone code; all other columns must be
#'   numeric counts.
#' @param inds A data frame containing individual-level (survey) data. This
#'   should be in the format of one row per individual, one column per
#'   constraint. The first column should be an individual ID.
#' @param vars A character vector of variables that constrain the simulation
#'   (i.e. independent variables)
#' @param iterations The number of iterations the algorithm should complete.
#'   Defaults to 10
#'
#' @return A data frame of fractional weights for each individual in each zone
#' with zone codes recorded in column names and individual id recorded in row
#' names.
#' @export
#'
#' @examples
#' # Deprecated. Use rk_weight()
weight <- function(cons, inds, vars = NULL, iterations = 10) {

  .Deprecated("rk_weight")

  rk_weight(cons = cons, inds = inds, vars = vars, iterations = iterations)

}

#' extract
#'
#' Deprecated. Use rk_extract() instead.
#'
#' @param weights A weight table, typically produced by rakeR::weight()
#' @param inds The individual level data
#' @param id The unique id variable in the individual level data (inds),
#' usually the first column
#'
#' @return A data frame with zones and aggregated simulated values for each
#' variable
#' @export
#'
#' @examples
#' \dontrun{
#' Deprecated. Use rk_extract()
#' }
extract <- function(weights, inds, id) {

  .Deprecated("rk_extract")

  rk_extract(weights = weights, inds = inds, id = id)

}


#' extract_weights
#'
#' Deprecated: use rakeR::rk_extract()
#'
#' @examples
#' \dontrun{
#' extract_weights() is deprecated, use rk_extract() instead
#' }
extract_weights <- function(weights, inds, id) {

  stop("extract_weights() is deprecated. Use rk_extract()")

}


#' integerise
#'
#' Deprecated. Use rk_integerise()
#'
#' @param weights A matrix or data frame of fractional weights, typically
#' provided by \code{rakeR::weight()}
#' @param inds The individual--level data (i.e. one row per individual)
#' @param method The integerisation method specified as a character string.
#' Defaults to \code{"trs"}; currently other methods are not implemented.
#' @param seed The seed to use, defaults to 42.
#'
#' @return A data frame of integerised cases
#' @aliases integerize
#' @export
#'
#' @examples
#' \dontrun{
#' Deprecated. Use rk_integerise()
#' }
integerise <- function(weights, inds, method = "trs", seed = 42) {

  .Deprecated("rk_integerise")

  rk_integerise(weights = weights, inds = inds, method = method, seed = seed)

}


#' rake
#'
#' Deprecated. Use rk_rake()
#'
#' @param cons A data frame of constraint variables
#' @param inds A data frame of individual--level (survey) data
#' @param vars A character string of variables to iterate over
#' @param output A string specifying the desired output, either "fraction"
#' (rk_extract()) or "integer" (rk_integerise())
#' @param iterations The number of iterations to perform. Defaults to 10.
#' @param ... Additional arguments to pass to depending on desired output:
#'   \itemize{
#'     \item{if "fraction" specify 'id' (see rk_extract() documentation)}
#'     \item{if "integer" specify 'method' and 'seed' (see rk_integerise()
#'   documentation)}
#'   }
#'
#' @return A data frame with extracted weights (if output == "fraction", the
#' default) or integerised cases (if output == "integer")
#' @export
#'
#' @examples
#' \dontrun{
#' Deprecated. Use rk_rake()
#' }
rake <- function(cons, inds, vars,
                 output = "fraction",
                 iterations = 10, ...) {

  .Deprecated("rk_rake")

  rk_rake(cons, inds, vars)

}
