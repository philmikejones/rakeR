context("Check SimpleWorld simulation")

load("../../data/cons.RData"); load("../../data/ind.RData")

# '+' not saving correctly for some reason?
colnames(con_age)[3] <- "a_50+"

# cons variables MUST be supplied in alphabetical order to the function
# When turning individual level responses into a matrix it produces
# the levels in alphabetical order.
# Currently the function DOES NOT check the order of variables so it's
# easiest to prepare the constraints up front
# If necessary add a prefix (e.g. 'a_') if you need to group variables
con_sex <- con_sex[, c(1, 3, 2)]


# Prepare ind$age bands to match cons
ind$age <- cut(ind$age,
               breaks = c(0, 49, 120),
               labels = c("a0_49", "a50+"))

# Create a cons object will all con_ vars
cons <- merge(con_age, con_sex, by = "zone")

vars <- c("age", "sex")

weights <- rake(cons = cons, ind = ind, vars = vars)

test_that("Ncols should equal number of zones in cons", {
  expect_equal(ncol(weights), nrow(cons))
})
test_that("Nrows should equal number of individuals in ind", {
  expect_equal(nrow(weights), nrow(ind))
})
test_that("Populations match (i.e. sum weights == (sum cons / n vars))", {
  expect_equal(sum(weights), (sum(cons[, -1]) / length(vars)))
  # Drop first column because it contains zone numbers
})
