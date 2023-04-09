install.packages('DescTools')
library(DescTools)
tbl <- array(c(4,3,3,6,
               0,10,2,5,
               9,7,6,4,
               5,2,2,6,
               3,3,0,4,
               1,3,0,0,
               1,5,2,3,
               4,4,6,2,
               5,12,7,10,
               7,2,6,3,
               6,2,2,8), dim = c(2, 2, 11))

mantelhaen.test(tbl,correct = F)
BreslowDayTest(tbl)

#make sure to fist source/run breslowday_test.R





