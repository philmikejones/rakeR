con_age <- data.frame(

  "zone"  = 1:3,
  "young" = c(8L, 2L, 7L),
  "old"   = c(4L, 8L, 4L)

)

con_sex <- data.frame(

  "zone" = 1:3,
  "m"    = c(6L, 4L, 3L),
  "f"    = c(6L, 6L, 8L)

)

save(con_age, con_sex, file = "data/cons.RData", compress = "xz")
