
prepare_sim_results <- function(shapefile, zone_simdf) {

  shapefile@data$code <- as.character(shapefile@data$code)

  shapefile@data <- inner_join(shapefile@data, zone_simdf, by = "code")
  context("Check shapefile")
  test_that("shapefile@data has correct number of OAs", {
    expect_equal(nrow(shapefile@data), nrow(zone_simdf))
  })

  ## Calculate percentages
  shapefile@data$p_hcond_1_plus <- shapefile@data$hcond_1_plus /
    (shapefile@data$hcond_0 + shapefile@data$hcond_1_plus)

  ## Fortify for plotting
  shapefile_f <- fortify(shapefile, region = "code")
  shapefile_f <- left_join(shapefile_f, shapefile@data,
                           by = c("id" = "code"))

  ## Create bins for plotting
  shapefile_f[["hcond_cut"]] <- NA
  shapefile_f[["hcond_cut"]] <- cut(shapefile_f$p_hcond_1_plus, breaks = 5,
                                    include.lowest = TRUE, dig.lab = 2, ordered_result = TRUE) %>%
    factor(., levels = rev(levels(.)))

  shapefile_f

}

int_validate <- function(constraints, ind_agg) {

  correlation <- cor(as.numeric(constraints), as.numeric(ind_agg))

  max_abs_diff <- max(abs(ind_agg - constraints))

  tae <- tae(constraints, ind_agg)

  sae <- tae(constraints, ind_agg) / sum(constraints)

  cor_vec <- rep(0, dim(constraints)[1])
  for (i in 1:dim(constraints)[1]) {
    cor_vec[i] <- cor(as.numeric(constraints[i, ]), as.numeric(ind_agg[i, ]))
  }
  ## cor_vec <- summary(cor_vec)

  tae_vec <- rep(0, nrow(constraints))
  sae_vec <- rep(0, nrow(constraints))
  for (i in 1:nrow(constraints)) {
    tae_vec[i] <- tae(constraints[i, ], ind_agg[i, ])
    sae_vec[i] <- tae_vec[i] / sum(constraints[i, ])
  }

  worst_zone <- which.max(tae_vec)
  ## worst_zone <- tae_vec[worst_zone] / sum(tae_vec))

  out <- list(
    "correlation"  = correlation,
    "max_abs_diff" = max_abs_diff,
    "tae"          = tae,
    "sae"          = sae,
    "cor_vec"      = cor_vec,
    "tae_vec"      = tae_vec,
    "sae_vec"      = sae_vec,
    "worst_zone"   = worst_zone
  )

  out

}

int_val_vars <- function(constraint, simdf) {

  constraint <- tbl_df(as.data.frame(constraint))

  ## Create an empty data frame to hold the results:
  ## zone | variable | real count | sim count |
  ##    1 |        1 |          x |         a |
  ##    2 |        1 |          y |         b |
  ##    3 |        1 |          z |         c |
  ##    1 |        2 |          i |         s |
  ##    2 |        2 |          j |         t |
  ##    3 |        2 |          k |         u |

  constraint[["zone"]] <- NA
  constraint[["zone"]] <- 1:nrow(constraint)

  ## Create the validation object (real data)
  int_val <- constraint %>% gather(zone)
  colnames(int_val) <- c("zone", "variable", "real")

  context("Check int_val is correct")
  test_that("int_val zones are correct", {
    expect_equal(max(int_val$zone), nrow(constraint))
    expect_equal(unique(int_val$zone), 1:nrow(constraint))
  })
  test_that("Population (i.e. colsum value for age_) is correct", {
    expect_equal(
      sum(int_val[grep("age_[[:digit:]]", int_val[["variable"]]), "real"]),
      sum(constraint[, grep("age_[[:digit:]]", colnames(constraint))]))
  })

  ## Add sim data
  ## dplyr::count() drops counts == 0 so add these back in where NAs
  ## https://github.com/hadley/dplyr/issues/assigned/romainfrancois
  int_val[["key"]] <- with(int_val, paste0(zone, variable))

  context("Check int_val key is unique")
  test_that("int_val key is unique", {
    expect_equal(int_val$key, unique(int_val$key))
  })

  tmp_sex <- count(simdf, c_sex,          zone)
  tmp_eth <- count(simdf, race_reduced,   zone)
  tmp_hiq <- count(simdf, hiqual,         zone)
  tmp_car <- count(simdf, car,            zone)
  tmp_ten <- count(simdf, tenure_reduced, zone)
  tmp_age <- count(simdf, age_cut,        zone)

  colnames(tmp_sex)[1] <- "variable"
  colnames(tmp_eth)[1] <- "variable"
  colnames(tmp_hiq)[1] <- "variable"
  colnames(tmp_car)[1] <- "variable"
  colnames(tmp_ten)[1] <- "variable"
  colnames(tmp_age)[1] <- "variable"

  tmp <- rbind(tmp_sex, tmp_eth)
  tmp <- rbind(tmp, tmp_hiq)
  tmp <- rbind(tmp, tmp_car)
  tmp <- rbind(tmp, tmp_ten)
  tmp <- rbind(tmp, tmp_age)

  tmp[["key"]] <- with(tmp, paste0(zone, variable))
  context("Check tmp key is unique")
  test_that("tmp key is unique", {
    expect_equal(tmp$key, unique(tmp$key))
  })

  int_val <- left_join(int_val, tmp, by = "key")
  int_val <- int_val %>% select(zone.x, variable.x, real, n)
  colnames(int_val) <- c("zone", "variable", "real", "sim")
  int_val$sim[is.na(int_val$sim)] <- 0

  context("Check int_val object")
  test_that("sum simulated population = sum real populaton", {
    expect_equal(
      sum(int_val[grep("age_[[:digit:]]", int_val$variable), "real"]),
      sum(int_val[grep("age_[[:digit:]]", int_val$variable), "sim"]))
  })

  ## To order facets
  int_val$var <- factor(int_val$var, levels = unique(int_val$var))

  ## To facet by variable types
  int_val$var_type <- NA
  var_types <- unique(int_val$var)

  int_val$var_type[int_val$var == "male"]    <- "Sex"
  int_val$var_type[int_val$var == "female"]  <- "Sex"
  int_val$var_type[grep("british", int_val$var)]           <- "Ethnicity"
  int_val$var_type[int_val$var == "irish"]                 <- "Ethnicity"
  int_val$var_type[int_val$var == "other_white"]           <- "Ethnicity"
  int_val$var_type[int_val$var == "mixed_multiple_ethnic"] <- "Ethnicity"
  int_val$var_type[int_val$var == "other_ethnicity"]       <- "Ethnicity"
  int_val$var_type[grep("qual_", int_val$var)]             <- "Qualifications"
  int_val$var_type[grep("car_", int_val$var)]              <- "Amenities"
  int_val$var_type[grep("owned_", int_val$var)]            <- "Amenities"
  int_val$var_type[int_val$var == "rented"]                <- "Amenities"
  int_val$var_type[grep("age_[[:digit:]]", int_val$var)]   <- "Age"

  int_val

}

plot_sae <- function(shapefile, int_val) {

  tmp_codes <- shapefile@data$code
  tmp_codes <- tmp_codes[order(tmp_codes)]

  sae_vec <- data.frame(
    tmp_codes,
    int_val[["sae_vec"]]
  )
  colnames(sae_vec) <- c("code", "sae_vec")

  ## Don't cbind() because zone codes_oa not in correct order
  shapefile@data <- left_join(shapefile@data, sae_vec, by = "code")

  ## Fortify
  shapefile_f <- fortify(shapefile, region = "code")
  shapefile@data$code <- as.character(shapefile@data$code)
  shapefile_f <- inner_join(shapefile_f, shapefile@data, by = c("id" = "code"))

  ggplot(shapefile_f) +
    geom_polygon(aes(long, lat, group = group, fill = sae_vec),
                 colour = "black") +
    coord_equal() + mapl +
    scale_fill_gradient(low = "white", high = "dark grey", limits = c(0, 0.2),
                        name = "Standardised\nAbsolute Error")
}

ext_validate <- function(llid_val, constraint, simdf, code_geo) {

  llid_val <- llid_val %>%
    select(GEOGRAPHY_CODE, C_DISABILITY_NAME, OBS_VALUE) %>%
    spread(C_DISABILITY_NAME, OBS_VALUE)

  llid_val$llid <- rowSums(llid_val[, 2:3])

  colnames(llid_val) <- c("code", "little", "lot",
                          "llid_no_census", "llid_census")
  llid_val <- select(llid_val, -little, -lot)

  context("Check llid_val object")
  test_that("Population of llid_val matches constraint", {
    expect_equal(sum(llid_val[, 2:3]),
                 sum(constraint[, grep("age_[[:digit:]]", colnames(constraint))]))
  })

  ## Add sim_llid to zone_simdf_oa
  llid_sim <- simdf %>%
    select(c_llid, zone) %>%
    count(zone, c_llid) %>%
    spread(c_llid, n)

  colnames(llid_sim) <- c("zone", "llid_no_sim", "llid_sim")
  llid_sim$code <- NA
  llid_sim$code <- codes[[as.character(code_geo)]]

  ## merge on codes (not rbind) because prison OAs removed!
  llid_val <- left_join(llid_val, llid_sim, by = "code")

  llid_val

}

calc_perr <- function(llid_val) {

  total <- rowSums(llid_val[, c("llid_no_census", "llid_census")])
  perr  <- llid_val$llid_census - llid_val$llid_sim
  perr  <- abs(perr)
  perr  <- perr / total
  perr  <- perr * 100

  perr

}

test_census <- function(census_var) {
  test_that("Each zone code is unique", {
    expect_that(census_var[["code"]], equals(unique(census_var[["code"]])))
  })
  test_that("Number of OAs is 978", {
    expect_that(nrow(census_var), equals(978))
  })
  test_that("All columns numeric", {
    expect_that(all(apply(census_var[, 2:ncol(census_var)], 2, is.numeric)),
                is_true())
  })
}

test_ind <- function(ind_var) {
  context("Check ind_ objects")
  test_that("Only 0 or 1", {
    expect_true(all(ind_var == 0 | ind_var == 1))
  })
  test_that("All rows must equal 1", {
    expect_true(all(rowSums(ind_var) == 1))
  })
  test_that("Population should match nrow(us)", {
    expect_equal(sum(ind_var), nrow(us))
  })
}

test_colnames <- function(ind_var, census_var) {
  context("Check colnames match")
  test_that("colnames ind_ match census_", {
    expect_equal(colnames(ind_var), colnames(census_var[2:ncol(census_var)]))
  })
}

test_zone_simdf <- function(zone_simdf, constraint) {
  testthat::context("Check zone_simdf")
  test_that("Correct number of zones", {
    expect_equal(nrow(zone_simdf), nrow(constraint))
  })
  test_that("Sum of each zone matches", {
    expect_equal(rowSums(zone_simdf[, 2:3]),
                 rowSums(constraint[, grep("age_[[:digit:]]",
                                           colnames(constraint))]))
  })
  test_that("Total adds up to Doncaster pop", {
    expect_equal(sum(zone_simdf[, 2:3]),
                 sum(constraint[, grep("age_[[:digit:]]",
                                       colnames(constraint))]))
  })
  test_that("Zone codes_oa all unique", {
    expect_equal(zone_simdf_oa$code, unique(zone_simdf_oa$code))
  })
  test_that("Zone codes_oa in the correct order (i.e. not re-arranged)", {
    expect_equal(zone_simdf_oa$code, codes[["oa"]])
  })
}

