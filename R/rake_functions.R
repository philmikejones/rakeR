#' rake
#'
#' Produces fractional weights using the iterative proportional fitting
#' ('raking') algorithm.
#'
#'
#' The first column of each data frame should be an ID. The first column of
#' \code{cons} should contain the zone codes. The first column of \code{ind}
#' should contain the individual unique identifier.
#'
#' Both data frames should only contain:
#' \itemize{
#'   \item an ID column (zone ID \code{cons} or individual ID \code{ind}).
#'   \item constraints \code{ind} or constraint category \code{cons}.
#'   \item \code{ind} can optionally contain additional dependent variables
#'   that do not influence the weighting process.
#' }
#'
#' No other columns should be present (the user can merge these back in later).
#'
#' It is essential that the levels in each \code{ind} constraint (i.e. column)
#' match exactly with the column names in \code{cons}. In the example below see
#' how the column names in cons (\code{'a0_49', 'f', ...}) match exactly the
#' levels in \code{ind} variables.
#'
#' The columns in \code{cons} must be in alphabetical order because these are
#' created alphabetically when they are 'spread' in the individual--level data.
#'
#' @param cons A data frame containing all the constraints. This
#'   should be in the format of one row per zone, one column per constraint
#'   category. The first column should be a zone code; all other columns must be
#'   numeric counts.
#' @param inds A data frame containing individual--level (survey) data. This
#'   should be in the format of one row per individual, one column per
#'   constraint. The first column should be an individual ID.
#' @param vars A character vector of variables that constrain the simulation
#'   (i.e. independent variables)
#' @param iterations The number of iterations the algorithm should complete.
#'   Defaults to 10
#'
#' @return A data frame of fractional weights for each individual in each zone.
#' @export
#'
#' @examples
#' # SimpleWorld
#' cons <- data.frame(
#' "zone"   = letters[1:3],
#' "a0_49"  = c(8, 2, 7),
#' "a_gt50" = c(4, 8, 4),
#' "f"      = c(6, 6, 8),
#' "m"      = c(6, 4, 3)
#' )
#' survey <- data.frame(
#' "id"     = LETTERS[1:5],
#' "age"    = c("a_gt50", "a_gt50", "a0_49", "a_gt50", "a0_49"),
#' "sex"    = c("m", "m", "m", "f", "f"),
#' "income" = c(2868, 2474, 2231, 3152, 2473),
#' stringsAsFactors = FALSE
#' )
#' # Set variables to constrain over
#' vars <- c("age", "sex")
#' weights <- rake(cons = cons, inds = survey, vars = vars)
#' print(weights)
rake <- function(cons, inds, vars = NULL, iterations = 10) {

  # Check arguments are the correct class
  if (!is.data.frame(cons)) {

    stop("cons is not a data frame")

  }

  if (!is.data.frame(inds)) {

    stop("inds is not a data frame")

  }

  if (!(is.atomic(vars) || is.list(vars))) {

    stop("vars is not a vector")

  }

  # Prepare constraints

  # Save and drop first column of cons (zone codes)
  zones <- cons[, 1]
  cons  <- cons[, -1]
  cons <- as.matrix(cons)

  # Save IDs from inds
  ids <- inds[, 1]

  # cons must be a numeric (i.e. double, not int) matrix
  cons[] <- as.numeric(cons[])


  # Prepare individual-level data (survey)

  # Create a list of survey based matrices to match cons matrices
  # Easiest way is to create 'dummy variables' (i.e. 0, 1) using model.matrix.
  # The '-1' drops the intercept, and puts the first variable back in
  # I hate it because it doesn't seem to be documented anywhere, but it works
  inds <- lapply(as.list(vars), function(x) {

    stats::model.matrix( ~ inds[[x]] - 1)

  })

  # Fix colnames
  for (i in seq_along(vars)) {

    colnames(inds[[i]]) <- gsub("inds\\[\\[x\\]\\]", "", colnames(inds[[i]]))

  }
  rm(i)

  # Create one ind_ table
  ind_cat <- do.call(cbind, inds)

  if (!isTRUE(all.equal(colnames(ind_cat), colnames(cons)))) {

    stop("Column names don't match.\n
         Are the first columns in cons and inds a zone code/unique ID?
         Check the unique levels in inds and colnames in cons match EXACTLY.
         Unique levels identified by rake():\n\n",
         vapply(seq_along(colnames(ind_cat)), function(x)
           paste0(colnames(ind_cat)[x], " "), "")
    )

  }

  weights <- apply(cons, 1, function(x) {

    ipfp::ipfp(x, t(ind_cat), x0 = rep(1, nrow(ind_cat)),
               maxit = iterations)

  })

  if (!isTRUE(all.equal(sum(weights), (sum(cons) / length(vars))))) {

    stop("Column names don't match.\n
         Are the first columns in cons and inds a zone code/unique ID?
         Check the unique levels in inds and colnames in cons match EXACTLY.
         Unique levels identified by rake():\n\n",
         vapply(seq_along(colnames(ind_cat)), function(x)
           paste0(colnames(ind_cat)[x], " "), "")
    )

  }

  if (!isTRUE(all.equal(colSums(weights), (rowSums(cons) / length(vars))))) {

    stop("Zone populations (cons) do not match simulated populations.\n
         Are the first columns in cons and inds a zone code/unique ID?
         Check the unique levels in inds and colnames in cons match EXACTLY.
         Unique levels identified by rake():\n\n",
         vapply(seq_along(colnames(ind_cat)), function(x)
           paste0(colnames(ind_cat)[x], " "), "")
    )

  }

  # Put column and row names back
  rownames(weights) <- ids
  colnames(weights) <- zones

  weights

}


#' trs
#'
#' Truncate, replicate, sample: a helper function to generate integer cases
#' from numeric weights.
#'
#' Truncate, replicate, sample is a method of integerisation developed by Robin
#' Lovelace and Dimitris Ballas (2013) *Truncate, replicate, sample: a method
#' for creating integer weights for spatial microsimulation*. Computers,
#' Environment and Urban Systems, vol. 41, pp. 1--11.
#' \url{http://www.sciencedirect.com/science/article/pii/S0198971513000240}
#'
#' @param weights a weights matrix, typically provided by \code{rake()}
#'
#' @return A matrix of integer weights
#' @export
#'
#' @examples # not run
trs <- function(weights) {

  # For generalisation purpose, weights becomes a vector
  # This allow the function to work with matrices
  weights_vec <- as.vector(weights)

  # Separate the integer and decimal part of the weight
  weights_int <- floor(weights_vec)
  weights_dec <- weights_vec - weights_int

  deficit <- round(sum(weights_dec))

  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(weights), size = deficit, prob = weights_dec)

  weights_int[topup] <- weights_int[topup] + 1

  # Return as a matrix with correct dimnames
  dim(weights_int)      <- dim(weights)
  dimnames(weights_int) <- dimnames(weights)

  weights_int

}


expand <- function(x) {

  index <- seq_along(x)
  out <- rep(index, round(x))

  out

}


simulate <- function(inds, x) {

  out <- data.frame(inds[x, ])

  out

}


simulate_df <- function(weights, cases) {

  simdf <- NULL
  simdf <- lapply(ints, function(x) cases[x, ])
  simdf <- dplyr::rbind_all(simdf)
  zone  <- rep(1:ncol(weights), sapply(ints, length))
  simdf$zone <- zone

  context("Check simdf")
  test_that("nrow simdf == census population", {
    expect_that(nrow(simdf), equals(sum(weights)))
  })
  test_that("correct number of zones in simdf", {
    expect_that(max(simdf[["zone"]]), equals(ncol(weights)))
  })

  simdf

}


# # stop("what does agg_ind do?")
# agg_ind <- function(weights, constraints) {
#   ind_agg <- apply(weights, 2, function(x)
#     colSums(x * ind_cat)
#   )
#
#   ind_agg <- t(ind_agg)
#
#   context("Check ind_agg")
#   test_that("ind_agg is correct", {
#     expect_that(ncol(ind_agg), equals(ncol(constraints)))
#     expect_that(nrow(ind_agg), equals(ncol(weights)))
#     expect_that(sum(ind_agg[, grep("age_[[:digit:]]", colnames(ind_agg))]),
#                 equals(sum(weights)))
#   })
#
#   ind_agg
#
# }
#
# tae <- function(observed, simulated) {
#   obs_vec <- as.numeric(observed)
#   sim_vec <- as.numeric(simulated)
#   return(
#     sum(abs(obs_vec - sim_vec))
#   )
# }
#
#
#
# # # int_validate <- function(constraints, ind_agg) {
# # #
# # #   correlation <- cor(as.numeric(constraints), as.numeric(ind_agg))
# # #
# # #   max_abs_diff <- max(abs(ind_agg - constraints))
# # #
# # #   tae <- tae(constraints, ind_agg)
# # #
# # #   sae <- tae(constraints, ind_agg) / sum(constraints)
# # #
# # #   cor_vec <- rep(0, dim(constraints)[1])
# # #   for (i in 1:dim(constraints)[1]) {
# # #     cor_vec[i] <- cor(as.numeric(constraints[i, ]), as.numeric(ind_agg[i, ]))
# # #   }
# # #   ## cor_vec <- summary(cor_vec)
# # #
# # #   tae_vec <- rep(0, nrow(constraints))
# # #   sae_vec <- rep(0, nrow(constraints))
# # #   for (i in 1:nrow(constraints)) {
# # #     tae_vec[i] <- tae(constraints[i, ], ind_agg[i, ])
# # #     sae_vec[i] <- tae_vec[i] / sum(constraints[i, ])
# # #   }
# # #
# # #   worst_zone <- which.max(tae_vec)
# # #   ## worst_zone <- tae_vec[worst_zone] / sum(tae_vec))
# # #
# # #   out <- list(
# # #     "correlation"  = correlation,
# # #     "max_abs_diff" = max_abs_diff,
# # #     "tae"          = tae,
# # #     "sae"          = sae,
# # #     "cor_vec"      = cor_vec,
# # #     "tae_vec"      = tae_vec,
# # #     "sae_vec"      = sae_vec,
# # #     "worst_zone"   = worst_zone
# # #   )
# # #
# # #   out
# # #
# # # }
# # #
# # # int_val_vars <- function(constraint, simdf) {
# # #
# # #   constraint <- tbl_df(as.data.frame(constraint))
# # #
# # #   ## Create an empty data frame to hold the results:
# # #   ## zone | variable | real count | sim count |
# # #   ##    1 |        1 |          x |         a |
# # #   ##    2 |        1 |          y |         b |
# # #   ##    3 |        1 |          z |         c |
# # #   ##    1 |        2 |          i |         s |
# # #   ##    2 |        2 |          j |         t |
# # #   ##    3 |        2 |          k |         u |
# # #
# # #   constraint[["zone"]] <- NA
# # #   constraint[["zone"]] <- 1:nrow(constraint)
# # #
# # #   ## Create the validation object (real data)
# # #   int_val <- constraint %>% gather(zone)
# # #   colnames(int_val) <- c("zone", "variable", "real")
# # #
# # #   context("Check int_val is correct")
# # #   test_that("int_val zones are correct", {
# # #     expect_equal(max(int_val$zone), nrow(constraint))
# # #     expect_equal(unique(int_val$zone), 1:nrow(constraint))
# # #   })
# # #   test_that("Population (i.e. colsum value for age_) is correct", {
# # #     expect_equal(
# # #       sum(int_val[grep("age_[[:digit:]]", int_val[["variable"]]), "real"]),
# # #       sum(constraint[, grep("age_[[:digit:]]", colnames(constraint))]))
# # #   })
# # #
# # #   ## Add sim data
# # #   ## dplyr::count() drops counts == 0 so add these back in where NAs
# # #   ## https://github.com/hadley/dplyr/issues/assigned/romainfrancois
# # #   int_val[["key"]] <- with(int_val, paste0(zone, variable))
# # #
# # #   context("Check int_val key is unique")
# # #   test_that("int_val key is unique", {
# # #     expect_equal(int_val$key, unique(int_val$key))
# # #   })
# # #
# # #   tmp_sex <- count(simdf, c_sex,          zone)
# # #   tmp_eth <- count(simdf, race_reduced,   zone)
# # #   tmp_hiq <- count(simdf, hiqual,         zone)
# # #   tmp_car <- count(simdf, car,            zone)
# # #   tmp_ten <- count(simdf, tenure_reduced, zone)
# # #   tmp_age <- count(simdf, age_cut,        zone)
# # #
# # #   colnames(tmp_sex)[1] <- "variable"
# # #   colnames(tmp_eth)[1] <- "variable"
# # #   colnames(tmp_hiq)[1] <- "variable"
# # #   colnames(tmp_car)[1] <- "variable"
# # #   colnames(tmp_ten)[1] <- "variable"
# # #   colnames(tmp_age)[1] <- "variable"
# # #
# # #   tmp <- rbind(tmp_sex, tmp_eth)
# # #   tmp <- rbind(tmp, tmp_hiq)
# # #   tmp <- rbind(tmp, tmp_car)
# # #   tmp <- rbind(tmp, tmp_ten)
# # #   tmp <- rbind(tmp, tmp_age)
# # #
# # #   tmp[["key"]] <- with(tmp, paste0(zone, variable))
# # #   context("Check tmp key is unique")
# # #   test_that("tmp key is unique", {
# # #     expect_equal(tmp$key, unique(tmp$key))
# # #   })
# # #
# # #   int_val <- left_join(int_val, tmp, by = "key")
# # #   int_val <- int_val %>% select(zone.x, variable.x, real, n)
# # #   colnames(int_val) <- c("zone", "variable", "real", "sim")
# # #   int_val$sim[is.na(int_val$sim)] <- 0
# # #
# # #   context("Check int_val object")
# # #   test_that("sum simulated population = sum real populaton", {
# # #     expect_equal(
# # #       sum(int_val[grep("age_[[:digit:]]", int_val$variable), "real"]),
# # #       sum(int_val[grep("age_[[:digit:]]", int_val$variable), "sim"]))
# # #   })
# # #
# # #   ## To order facets
# # #   int_val$var <- factor(int_val$var, levels = unique(int_val$var))
# # #
# # #   ## To facet by variable types
# # #   int_val$var_type <- NA
# # #   var_types <- unique(int_val$var)
# # #
# # #   int_val$var_type[int_val$var == "male"]    <- "Sex"
# # #   int_val$var_type[int_val$var == "female"]  <- "Sex"
# # #   int_val$var_type[grep("british", int_val$var)]           <- "Ethnicity"
# # #   int_val$var_type[int_val$var == "irish"]                 <- "Ethnicity"
# # #   int_val$var_type[int_val$var == "other_white"]           <- "Ethnicity"
# # #   int_val$var_type[int_val$var == "mixed_multiple_ethnic"] <- "Ethnicity"
# # #   int_val$var_type[int_val$var == "other_ethnicity"]       <- "Ethnicity"
# # #   int_val$var_type[grep("qual_", int_val$var)]             <- "Qualifications"
# # #   int_val$var_type[grep("car_", int_val$var)]              <- "Amenities"
# # #   int_val$var_type[grep("owned_", int_val$var)]            <- "Amenities"
# # #   int_val$var_type[int_val$var == "rented"]                <- "Amenities"
# # #   int_val$var_type[grep("age_[[:digit:]]", int_val$var)]   <- "Age"
# # #
# # #   int_val
# # #
# # # }
# # #
# # # plot_sae <- function(shapefile, int_val) {
# # #
# # #   tmp_codes <- shapefile@data$code
# # #   tmp_codes <- tmp_codes[order(tmp_codes)]
# # #
# # #   sae_vec <- data.frame(
# # #     tmp_codes,
# # #     int_val[["sae_vec"]]
# # #   )
# # #   colnames(sae_vec) <- c("code", "sae_vec")
# # #
# # #   ## Don't cbind() because zone codes_oa not in correct order
# # #   shapefile@data <- left_join(shapefile@data, sae_vec, by = "code")
# # #
# # #   ## Fortify
# # #   shapefile_f <- fortify(shapefile, region = "code")
# # #   shapefile@data$code <- as.character(shapefile@data$code)
# # #   shapefile_f <- inner_join(shapefile_f, shapefile@data, by = c("id" = "code"))
# # #
# # #   ggplot(shapefile_f) +
# # #     geom_polygon(aes(long, lat, group = group, fill = sae_vec),
# # #                  colour = "black") +
# # #     coord_equal() + mapl +
# # #     scale_fill_gradient(low = "white", high = "dark grey", limits = c(0, 0.2),
# # #                         name = "Standardised\nAbsolute Error")
# # # }
# # #
# # # ext_validate <- function(llid_val, constraint, simdf, code_geo) {
# # #
# # #   llid_val <- llid_val %>%
# # #     select(GEOGRAPHY_CODE, C_DISABILITY_NAME, OBS_VALUE) %>%
# # #     spread(C_DISABILITY_NAME, OBS_VALUE)
# # #
# # #   llid_val$llid <- rowSums(llid_val[, 2:3])
# # #
# # #   colnames(llid_val) <- c("code", "little", "lot",
# # #                           "llid_no_census", "llid_census")
# # #   llid_val <- select(llid_val, -little, -lot)
# # #
# # #   context("Check llid_val object")
# # #   test_that("Population of llid_val matches constraint", {
# # #     expect_equal(sum(llid_val[, 2:3]),
# # #                  sum(constraint[, grep("age_[[:digit:]]", colnames(constraint))]))
# # #   })
# # #
# # #   ## Add sim_llid to zone_simdf_oa
# # #   llid_sim <- simdf %>%
# # #     select(c_llid, zone) %>%
# # #     count(zone, c_llid) %>%
# # #     spread(c_llid, n)
# # #
# # #   colnames(llid_sim) <- c("zone", "llid_no_sim", "llid_sim")
# # #   llid_sim$code <- NA
# # #   llid_sim$code <- codes[[as.character(code_geo)]]
# # #
# # #   ## merge on codes (not rbind) because prison OAs removed!
# # #   llid_val <- left_join(llid_val, llid_sim, by = "code")
# # #
# # #   llid_val
# # #
# # # }
# # #
# # # calc_perr <- function(llid_val) {
# # #
# # #   total <- rowSums(llid_val[, c("llid_no_census", "llid_census")])
# # #   perr  <- llid_val$llid_census - llid_val$llid_sim
# # #   perr  <- abs(perr)
# # #   perr  <- perr / total
# # #   perr  <- perr * 100
# # #
# # #   perr
# # #
# # # }
# # #
# # # test_census <- function(census_var) {
# # #   test_that("Each zone code is unique", {
# # #     expect_that(census_var[["code"]], equals(unique(census_var[["code"]])))
# # #   })
# # #   test_that("Number of OAs is 978", {
# # #     expect_that(nrow(census_var), equals(978))
# # #   })
# # #   test_that("All columns numeric", {
# # #     expect_that(all(apply(census_var[, 2:ncol(census_var)], 2, is.numeric)),
# # #                 is_true())
# # #   })
# # # }
# # #
# # # test_ind <- function(ind_var) {
# # #   context("Check ind_ objects")
# # #   test_that("Only 0 or 1", {
# # #     expect_true(all(ind_var == 0 | ind_var == 1))
# # #   })
# # #   test_that("All rows must equal 1", {
# # #     expect_true(all(rowSums(ind_var) == 1))
# # #   })
# # #   test_that("Population should match nrow(us)", {
# # #     expect_equal(sum(ind_var), nrow(us))
# # #   })
# # # }
# # #
# # # test_colnames <- function(ind_var, census_var) {
# # #   context("Check colnames match")
# # #   test_that("colnames ind_ match census_", {
# # #     expect_equal(colnames(ind_var), colnames(census_var[2:ncol(census_var)]))
# # #   })
# # # }
# # #
# # # test_zone_simdf <- function(zone_simdf, constraint) {
# # #   testthat::context("Check zone_simdf")
# # #   test_that("Correct number of zones", {
# # #     expect_equal(nrow(zone_simdf), nrow(constraint))
# # #   })
# # #   test_that("Sum of each zone matches", {
# # #     expect_equal(rowSums(zone_simdf[, 2:3]),
# # #                  rowSums(constraint[, grep("age_[[:digit:]]",
# # #                                            colnames(constraint))]))
# # #   })
# # #   test_that("Total adds up to Doncaster pop", {
# # #     expect_equal(sum(zone_simdf[, 2:3]),
# # #                  sum(constraint[, grep("age_[[:digit:]]",
# # #                                        colnames(constraint))]))
# # #   })
# # #   test_that("Zone codes_oa all unique", {
# # #     expect_equal(zone_simdf_oa$code, unique(zone_simdf_oa$code))
# # #   })
# # #   test_that("Zone codes_oa in the correct order (i.e. not re-arranged)", {
# # #     expect_equal(zone_simdf_oa$code, codes[["oa"]])
# # #   })
# # # }
# # #
