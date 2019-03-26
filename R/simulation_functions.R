#' rk_weight
#'
#' Produces fractional weights using the iterative proportional fitting
#' algorithm.
#'
#' rk_weight() requires three arguments:
#' \itemize{
#'   \item A data frame of constraints (e.g. census tables)
#'   \item A data frame of individual data (e.g. a survey)
#'   \item A character vector of constraint variable names
#' }
#'
#' The first column of each data frame should be an ID. The first column of
#' \code{cons} should contain the zone codes. The first column of \code{inds}
#' should contain the individual unique identifier.
#'
#' Both data frames should only contain:
#' \itemize{
#'   \item an ID column (zone ID \code{cons} or individual ID \code{inds}).
#'   \item constraints \code{inds} or constraint category \code{cons}.
#'   \item \code{inds} can optionally contain additional dependent variables
#'   that do not influence the weighting process.
#' }
#'
#' No other columns should be present (the user can merge these back in later).
#'
#' It is essential that the levels in each \code{inds} constraint (i.e. column)
#' match exactly with the column names in \code{cons}. In the example below see
#' how the column names in cons (\code{'age_0_49', 'sex_f', ...}) match exactly
#' the levels in the appropriate \code{inds} variables.
#'
#' The columns in \code{cons} must be arranged in alphabetical order because
#' these are created alphabetically when they are 'spread' in the
#' individual-level data.
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
#' # SimpleWorld
#' cons <- data.frame(
#'   "zone"      = letters[1:3],
#'   "age_0_49"  = c(8, 2, 7),
#'   "age_gt_50" = c(4, 8, 4),
#'   "sex_f"     = c(6, 6, 8),
#'   "sex_m"     = c(6, 4, 3),
#'   stringsAsFactors = FALSE
#' )
#' inds <- data.frame(
#'   "id"     = LETTERS[1:5],
#'   "age"    = c(
#'     "age_gt_50", "age_gt_50", "age_0_49", "age_gt_50", "age_0_49"
#'   ),
#'   "sex"    = c("sex_m", "sex_m", "sex_m", "sex_f", "sex_f"),
#'   "income" = c(2868, 2474, 2231, 3152, 2473),
#'   stringsAsFactors = FALSE
#' )
#' # Set variables to constrain over
#' vars <- c("age", "sex")
#' weights <- rk_weight(cons = cons, inds = inds, vars = vars)
#' print(weights)
rk_weight <- function(cons, inds, vars = NULL, iterations = 10) {

  # Check arguments are the correct class
  if (!is.data.frame(cons)) {
    stop("Error in cons: cons is not a data frame")
  }

  if (!is.data.frame(inds)) {
    stop("Error in inds: inds is not a data frame")
  }

  if (!(is.atomic(vars) || is.list(vars))) {
    stop("Error in vars: vars is not a vector")
  }

  # Check for any missing values
  if (any(is.na(cons))) {
    stop("Error in cons: Missing value(s) (`NA`) in cons")
  } else if (any(is.na(inds))) {
    stop("Error in inds: Missing value(s) (`NA`) in inds")
  }

  # Ensure there aren't any duplicate zone or individual codes
  if (any(duplicated(cons[, 1]))) {
    stop("Error in cons: duplicate zone codes present (first column treated as code)")
  } else if (any(duplicated(inds[, 1]))) {
    stop("Error in inds: duplicate ids present (first column treated as ID)")
  }

  # weight() will error if 1 or more zones are completely empty (i.e. the
  # population is 0; rowSums == 0). See issue #64
  if (any(rowSums(cons[, 2:ncol(cons)]) == 0)) {  # col 1 is ID
    stop("Error in cons: one or more zones only contain 0")
  }

  if (!all(unlist(lapply(
    inds[, vars],
    function(x) unique(x) %in% colnames(cons)
  )))) {
    stop("Error in inds: inds variable levels do not match cons colnames")
  }


  # Save and drop first column of cons (zone codes)
  # unlist() is needed in case the data is provided as a tibble
  zones <- as.vector(unlist(cons[, 1]))
  cons <- cons[, -1]
  cons <- as.matrix(cons)
  # cons must be a numeric (i.e. double, not int) matrix
  cons[] <- as.numeric(cons[])


  # Save IDs from inds
  # unlist() is needed in case the data is provided as a tibble
  ids  <- as.vector(unlist(inds[, 1]))
  inds <- inds[, 2:ncol(inds)]  # issue 33

  # Create a list of survey based matrices to match cons matrices
  # Easiest way is to create 'dummy variables' (i.e. 0, 1) using model.matrix.
  # The '-1' drops the intercept, and puts the first variable back in
  # I hate it because it doesn't seem to be documented anywhere, but it works
  inds <- lapply(as.list(vars), function(x) {
    stats::model.matrix( ~ inds[[x]] - 1)
  })

  # Fix colnames
  for (i in seq_along(vars)) {  # for loop ok; typically only <= 12 columns
    colnames(inds[[i]]) <- gsub("inds\\[\\[x\\]\\]", "", colnames(inds[[i]]))
  }
  rm(i)

  # one ind table based on unique levels in inds is easier to check and use
  ind_cat <- do.call(cbind, inds)

  # give ind_cat sequential column names to ensure they're entered into the
  # model in the correct order
  colnames(ind_cat) <-
    paste0(
      seq_along(colnames(ind_cat)),
      "_",
      colnames(ind_cat)
    )
  colnames(cons) <- colnames(ind_cat)


  # Calculate weights
  weights <- apply(cons, 1, function(x) {

    weights <- ipfp::ipfp(
      x,
      t(ind_cat),
      x0 = rep(1, nrow(ind_cat)),
      maxit = iterations
    )

    weights

  })

  rownames(weights) <- ids
  colnames(weights) <- zones
  weights <- as.data.frame(weights)

  weights

}


#' rk_extract
#'
#' Extract aggregate weights from individual weight table
#'
#' Extract aggregate weights from individual weight table, typically produced
#' by rakeR::rk_weight()
#'
#' Extract cannot operate with numeric variables because it creates a new
#' variable for each unique factor of each variable
#' If you want numeric information, like income, you need to cut() the
#' numeric values, or use integerise() instead.
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
#' ## Not run
#' ## Use weights object from rk_weight()
#' ## ext_weights <- rk_extract(weights = weights, inds = inds, id = "id")
rk_extract <- function(weights, inds, id) {

  # variables to loop over (dropping id/code)
  variables <- colnames(inds)
  variables <- variables[-grep(id, variables)]

  # check if any columns are class numeric or integer
  # have to use loop as class() returns class of the overall d.f.
  # have to use class() because typeof() for factor returns integer (as
  # it uses integers with attributes under the hood)
  # same for is()
  lapply(inds[, variables], function(x) {
    if (class(x) == "numeric" | class(x) == "integer") {
      stop("rakeR::extract() cannot work with numeric (i.e. integer or double)
           variables because by design it creates a new variable for each
           unique level in each variable\n
           Consider cut()ing your numeric data, removing your
           numeric data, or integerise() instead.")
    }
  })

  levels <- lapply(as.list(variables), function(x) {
    sort(unique(as.character(inds[[x]])))
  })

  result <- lapply(variables, function(y) {

    lapply(as.list(sort(unique(as.character(inds[[y]])))), function(x) {

      match_id <- inds[[id]][inds[[y]] == x]

      matched_weights <- weights[row.names(weights) %in% match_id, ]
      matched_weights <- colSums(matched_weights)

      matched_weights

    })

  })

  result           <- as.data.frame(result)
  colnames(result) <- unlist(levels)

  df <- data.frame(
    code  = colnames(weights),
    total = colSums(weights),
    row.names = NULL, stringsAsFactors = FALSE
  )

  df <- cbind(df, result)
  row.names(df) <- NULL

  df

}


#' rk_integerise
#'
#' Generate integer cases from numeric weights matrix.
#'
#' Extracted weights (using rakeR::rk_extract()) are more 'precise' than
#' integerised weights (although the user should be careful this is not
#' spurious precision based on context) as they return fractions.
#' Nevertheless, integerised weights are useful in cases when:
#'   \itemize{
#'     \item{Numeric information (such as income) is required, as this needs
#'     to be cut() to work with rakeR::rk_extract()}
#'     \item{Simulated 'individuals' are required for case studies of key
#'     areas.}
#'     \item{Input individual-level data for agent-based or dynamic models are
#'     required}
#'   }
#'
#' The default integerisation method uses the 'truncate, replicate, sample'
#' method developed by Robin Lovelace and Dimitris Ballas
#' \url{http://www.sciencedirect.com/science/article/pii/S0198971513000240}
#'
#' Other methods (for example proportional probabilities) may be implemented
#' at a later date.
#'
#' @param weights A matrix or data frame of fractional weights, typically
#' provided by \code{rakeR::rk_weight()}
#' @param inds The individual--level data (i.e. one row per individual)
#' @param method The integerisation method specified as a character string.
#' Defaults to \code{"trs"}; currently other methods are not implemented.
#' @param seed The seed to use, defaults to 42.
#'
#' @return A data frame of integerised cases
#' @aliases rk_integerize
#' @export
#'
#' @examples
#' cons <- data.frame(
#'   "zone"      = letters[1:3],
#'   "age_0_49"  = c(8, 2, 7),
#'   "age_gt_50" = c(4, 8, 4),
#'   "sex_f"     = c(6, 6, 8),
#'   "sex_m"     = c(6, 4, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' inds <- data.frame(
#'   "id"     = LETTERS[1:5],
#'   "age"    = c(
#'     "age_gt_50", "age_gt_50", "age_0_49", "age_gt_50", "age_0_49"
#'   ),
#'   "sex"    = c("sex_m", "sex_m", "sex_m", "sex_f", "sex_f"),
#'   "income" = c(2868, 2474, 2231, 3152, 2473),
#'   stringsAsFactors = FALSE
#' )
#' vars <- c("age", "sex")
#'
#' weights     <- rk_weight(cons = cons, inds = inds, vars = vars)
#' weights_int <- rk_integerise(weights, inds = inds)
rk_integerise <- function(weights, inds, method = "trs", seed = 42) {

  # Ensures the output of the function is reproducible (uses sample())
  set.seed(seed)

  # Check structure of inputs
  if (!is.data.frame(inds)) {
    stop("inds is not a data frame")
  }

  # Number of observations should be the same in weights and inds
  if (!isTRUE(all.equal(nrow(weights), nrow(inds)))) {
    stop("Number of observations in weights does not match inds")
  }

  if (!method == "trs") {
    stop("Currently this function only supports the truncate, replicate,
         sample method.
         Proportional probabilities may be added at a later date.
         For now use the default method (trs).")
  }

  # Weights must be a numeric matrix to reduce to a vector
  weights <- as.matrix(weights)

  weights_vec <- as.vector(weights)

  # Separate the integer and decimal part of the weight
  weights_int <- floor(weights_vec)
  weights_dec <- weights_vec - weights_int
  deficit <- round(sum(weights_dec))

  # if weights are already integers return them unchanged
  if (!sum(weights_dec %% 1) > 0) {
    message("weights already integers. Returning unmodified")
    return(weights)
  }

  # the weights be 'topped up' (+ 1 applied)
  topup <- wrswoR::sample_int_crank(n = length(weights),
                                    size = deficit,
                                    prob = weights_dec)

  weights_int[topup] <- weights_int[topup] + 1


  # Return as a data frame with correct dimnames
  dim(weights_int)      <- dim(weights)
  dimnames(weights_int) <- dimnames(weights)
  weights_int           <- apply(weights_int, 2, as.integer)
  weights_int           <- as.data.frame(weights_int)

  weights_int <- as.matrix(weights_int)

  # Create indices to subset/replicate against the survey
  indices <- apply(weights_int, 2, function(x) {
    rep.int(seq_along(x), x)
  })

  indices <- as.numeric(unlist(indices))

  # Create zones
  zone <- rep(colnames(weights), times = colSums(weights_int))

  sim_df <- inds[indices, ]
  sim_df$zone <- zone

  sim_df

}


#' rk_rake
#'
#' A convenience function wrapping \code{rk_weight()} and \code{rk_extract()} or
#' \code{rk_weight()} and \code{rk_integerise()}
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
#' frac_weights <- rake(cons, inds, vars, output = "fraction",
#'                      id = "id")
#'
#' int_weight <- rake(cons, inds, vars, output = "integer",
#'                    method = "trs", seed = "42")
#' }
rk_rake <- function(
  cons, inds, vars, output = "fraction", iterations = 10, ...
) {

  arguments <- list(...)

  weights <- rk_weight(
    cons = cons, inds = inds, vars = vars, iterations = iterations
  )

  if (output == "fraction") {

    frac_out <- rk_extract(
      weights = weights, inds = inds, id = arguments[["id"]]
    )

    frac_out

  } else if (output == "integer") {

    int_out <- rk_integerise(
      weights = weights, inds = inds,
      method = arguments[["method"]],
      seed = arguments[["seed"]]
    )

    int_out

  }

}
