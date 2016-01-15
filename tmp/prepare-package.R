# Code to create documentation and install
document()
setwd("/Users/steve/Sites/datascibc/dsbc")
install("../dsbc")
library(dsbc)
# Testing lookfor function
test_data <- data.frame(id=c(1:10), x=seq(1, 100, 10))
lookfor("x", data=test_data)
?lookfor