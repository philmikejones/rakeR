#' check_constraint
#'
#' @param constraint_var The constraint table to check, usually a data frame
#' @param num_zones The number of zones that should be present in the table
#'
#' @return An error status. 0 (silent) if no errors; 1 if errors detected
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
#' check_constraint(cons, 3)
check_constraint <- function(constraint_var, num_zones) {

  stopifnot(
    all.equal(constraint_var[[1]], unique(constraint_var[[1]])),
    all.equal(nrow(constraint_var), num_zones),
    all(apply(constraint_var[, 2:ncol(constraint_var)], 2, is.numeric))
  )

}
