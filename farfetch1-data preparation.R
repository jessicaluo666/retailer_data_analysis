
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



-----------#define a graph theme function---------------------

graph.function <- function(data) {
  ggplot(data,aes(x=category, y=price)) +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          text = element_text(family = "Comic Sans MS"))
}











