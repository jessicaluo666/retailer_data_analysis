
----------#data preparation----------

farfetch <- read.csv(file.choose())
farfetch

install.packages("ggplot2")
library(ggplot2)

#brand
brand <- farfetch$brand

#price
price <- farfetch$price_without_currency_symbol

#gender
gender <- farfetch$gender

#category
category <- farfetch$category


farfetch1 <- data.frame(brand,price,gender,category)
head(farfetch1)
tail(farfetch1)
nrow(farfetch1)

options(max.print = 10000000)    -----------------------#NO MISSING VALUE#-----------------
row_na <- is.na(farfetch1)
row_na[1:317447,]

















