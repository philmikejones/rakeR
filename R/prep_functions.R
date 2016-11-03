#' check_constraint
#'
#' Checks a contraint table for common errors.
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
#' cons <- data.frame(
#' "zone"  = letters[1:3],
#' "a0_49" = c(8, 2, 7),
#' "a_gt50" = c(4, 8, 4),
#' "f"    = c(6, 6, 8),
#' "m"    = c(6, 4, 3)
#' )
#' check_constraint(cons, 3)  # no errors
check_constraint <- function(constraint_var, num_zones) {

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

  if (!(all(rowSums(ind_var)) == 1)) {
    stop("Each row must sum to 1. Have you converted your individual data into
         dummy variables (using model.matrix())?")
  }

}
