# Code to create documentation and install
setwd("/Users/steve/Sites/datascibc/dsbc")
document()
install("../dsbc")
library(dsbc)
# Testing lookfor function
test_data <- data.frame(id=c(1:10), x=seq(1, 100, 10))
lookfor("x", data=test_data)
?lookfor