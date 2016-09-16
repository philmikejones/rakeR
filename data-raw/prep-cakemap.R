# Prep cons and inds for cakeMap

# Cons
cons <- readr::read_csv("inst/extdata/cakemap_cons.csv")
colnames(cons) <- gsub("X", "", colnames(cons))
colnames(cons) <- gsub("\\.", "_", colnames(cons))
colnames(cons)[13:14] <- c("car_yes", "car_no")
colnames(cons)[15:ncol(cons)] <- paste0("n_", colnames(cons)[15:ncol(cons)])
colnames(cons)[ncol(cons)] <- "n_97"
cons$code <- paste0("c", 1:nrow(cons))
cons <- cons[, c(ncol(cons), 13:14, 15:24, 7:12, 1:6)]

# population of nssec is 3 out compared to age/sex and car
cons[1, 6] <- 2775

save(cons, file = "data/cakemap_cons.RData", compress = "xz")


# inds
inds <- readr::read_csv("inst/extdata/cakemap_inds.csv")
inds$code <- paste0("i", 1:nrow(inds))
inds <- inds[, c(6, 2:5, 1)]

inds$ageband4 <- gsub("-", "_", inds$ageband4)
inds$NSSEC8 <- gsub("\\.", "_", inds$NSSEC8)
inds$NSSEC8 <- paste0("n_", inds$NSSEC8)
inds$Sex <- factor(inds$Sex, levels = 1:2,
                   labels = c("male", "female"))
inds$Car <- factor(inds$Car, levels = 1:2,
                   labels = c("car_yes", "car_no"))

inds$ageband4[inds$Sex == "male"] <-
  paste0("m", inds$ageband4[inds$Sex == "male"])
inds$ageband4[inds$Sex == "female"] <-
  paste0("f", inds$ageband4[inds$Sex == "female"])
inds <- inds[, -3]

save(inds, file = "data/cakemap_inds.RData", compress = "xz")
