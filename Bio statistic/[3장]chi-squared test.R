sex_corneal <- matrix(c(40,43, 84,60), nrow = 2)

sex_corneal

tbl <- as.table(sex_corneal)
chisq.test(tbl, correct = F)
