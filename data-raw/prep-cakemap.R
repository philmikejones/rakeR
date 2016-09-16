# Prep cons and inds for cakeMap

# Cons
cons <- readr::read_csv("inst/extdata/cakemap_cons.csv")
colnames(cons)[1:6]   <- paste0("b_", colnames(cons)[1:6])
colnames(cons)[7:12]  <- paste0("a_", colnames(cons)[7:12])
colnames(cons)[13:14] <- paste0("c_", colnames(cons)[13:14])
colnames(cons)[15:ncol(cons)] <- paste0("n_", colnames(cons)[15:ncol(cons)])
colnames(cons) <- gsub("X", "", colnames(cons))
colnames(cons) <- gsub("\\.", "_", colnames(cons))
cons$code <- paste0("c", 1:nrow(cons))
cons <- cons[, c(ncol(cons), 7:12, 1:6, 13:ncol(cons) - 1)]

save(cons, file = "data/cakemap_cons.RData", compress = "xz")


# inds
inds <- readr::read_csv("inst/extdata/cakemap_inds.csv")
inds$code <- paste0("i", 1:nrow(inds))
inds <- inds[, c(6, 2:5, 1)]

inds$ageband4 <- gsub("-", "_", inds$ageband4)
inds$NSSEC8 <- gsub("\\.", "_", inds$NSSEC8)
inds$Sex <- factor(inds$Sex, levels = 1:2,
                   labels = c("male", "female"))
inds$Car <- factor(inds$Car, levels = 1:2,
                   labels = c("yes", "no"))

save(inds, file = "data/cakemap_inds.RData", compress = "xz")
