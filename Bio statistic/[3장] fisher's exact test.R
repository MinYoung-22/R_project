library(DescTools)
tbl <- as.table(matrix(c(45,0,5,50),nrow=2))

fisher.test(tbl, alternative = 'less')
