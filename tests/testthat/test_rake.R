load("data/cons.RData"); load("data/ind.RData")

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

weights <- rake(cons, ind_cat)
