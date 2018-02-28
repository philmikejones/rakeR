#' check_constraint
#'
#' Deprecated: these checks are now done as part of the weight() and/or
#' extract()/integerise() steps automatically
#'
#' Checks a constraint table for the following common errors:
#' \itemize{
#'   \item Ensures all zone codes are unique
#'   \item Ensures there are the expected number of zones
#'   \item Ensures all but the zone column are numeric (integer or double)
#' }
#'
#' @param constraint_var The constraint table to check, usually a data frame
#' @param num_zones The number of zones that should be present in the table
#'
#' @return If no errors are detected the function returns silently. Any errors
#' will stop the function or script to be investigated.
#' @export
#'
#' @examples
#' Not run
#' ## check_constraint() is deprecated. These checks are automatically
#' ## carried out as part of the weight() and/or extract()/integerise()
#' ## functions
check_constraint <- function(constraint_var, num_zones) {

  .Deprecated("")

  stopifnot(
    all.equal(constraint_var[[1]], unique(constraint_var[[1]])),
    all.equal(nrow(constraint_var), num_zones),
    all(apply(constraint_var[, 2:ncol(constraint_var)], 2, is.numeric))
  )

}


#' check_ind
#'
#' Checks an individual (survey) variable for common errors.
#'
#' Checks an individual (survey) variable for the following common errors:
#' \itemize{
#'   \item That each row sums to 1 (i.e. correctly converted to a dummy
#'   variable)
#' }
#'
#' @param ind_var the individual (survey) variable you want to check
#'
#' @return If no errors are detected the function returns silently. Any errors
#' will stop the function or script to be investigated.
#' @export
#'
#' @examples
#' ## check_ind(ind_var)
check_ind <- function(ind_var) {

  if (!(all(rowSums(ind_var) == 1))) {
    stop("Each row must sum to 1. Have you converted your individual data into
         dummy variables (using model.matrix())?")
  }

}
