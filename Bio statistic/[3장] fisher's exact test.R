library(DescTools)
tbl <- as.table(matrix(c(45,5,0,50),nrow=2))

fisher.test(tbl, alternative = 'greater')


