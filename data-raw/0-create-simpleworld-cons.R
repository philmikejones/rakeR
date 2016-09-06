con_age <- data.frame(

  "zone"  = 1:3,
  "a0_49" = c(8, 2, 7),
  "a_50+" = c(4, 8, 4)

)

con_sex <- data.frame(

  "zone" = 1:3,
  "m"    = c(6, 4, 3),
  "f"    = c(6, 6, 8)

)

save(con_age, con_sex, file = "data/cons.RData", compress = "xz")
