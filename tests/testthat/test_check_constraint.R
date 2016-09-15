context("Test check_constraint() function")

test_that("Correct data frame passes", {
  cons <- data.frame(
    "zone"   = letters[1:3],
    "a0_49"  = c(8L, 2L, 7L),
    "a_gt50" = c(4L, 8L, 4L),
    "f"      = c(6L, 6L, 8L),
    "m"      = c(6L, 4L, 3L)
  )
  expect_silent(check_constraint(cons, 3))
})

test_that("Character column fail test", {
  cons <- data.frame(
    "zone"   = letters[1:3],
    "a0_49"  = c(8L, 2L, 7L),
    "a_gt50" = c(4L, 8L, 4L),
    "f"      = c(6L, 6L, 8L),
    "m"      = c(6L, 4L, 3L)
  )
  cons[, 2] <- as.character(cons[, 2])
  expect_error(check_constraint(cons, 3))
})

test_that("More zones than expected", {
  cons <- data.frame(
    "zone"  = letters[1:4],
    "a0_49" = c(8, 2, 7, NA),
    "a_gt50" = c(4, 8, 4, NA),
    "f"    = c(6, 6, 8, NA),
    "m"    = c(6, 4, 3, NA)
  )
  expect_error(check_constraint(cons, 3))
})

test_that("Zone codes not unique", {
  cons <- data.frame(
    "zone"  = c(letters[1:2], letters[1]),
    "a0_49" = c(8, 2, 7),
    "a_gt50" = c(4, 8, 4),
    "f"    = c(6, 6, 8),
    "m"    = c(6, 4, 3)
  )
  expect_error(check_constraint(cons, 3))
})
