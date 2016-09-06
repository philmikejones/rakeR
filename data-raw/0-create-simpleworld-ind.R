ind <- data.frame(

  "id"     = 1:5,
  "age"    = c(59L, 54L, 35L, 73L, 49L),
  "sex"    = c("m", "m", "m", "f", "f"),
  "income" = c(2868, 2474, 2231, 3152, 2473),
  stringsAsFactors = FALSE

)

save(ind, file = "data/ind.RData", compress = "xz")
