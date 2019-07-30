
# Project Name
> Farfetch products analysis/visualization

## Table of contents
* [Software use](#software-use)
* [Packages use](#packages-use)
* [Program](#program)
* [How to use](#how-to-use)
* [Screenshot example](#screenshot-example)
* [Code examples](#code-examples)
* [Data](#data)



## Software use: 
R grogramming

## Packages use:
ggplot2

## Program:
R programming

## How to use:
Using R program to initially clear the date, delete unused rows and columns from the dateframe, only keep brand, price, category and gender in the new dataframe. And then analyse/visualize the following components: 
* By Category

*Clothing*
##### the most expensive clothings with its brand
##### the least expensive clothings with its brand
##### Top 10 the most expensive brands in clothing
##### Top 10 the least expensive brands in clothing
##### Price variance among clothing within a brand

*Bags*  
##### the most expensive bags with its brand
##### the least expensive bags with its brand 
##### Top 10 the most expensive brands in bag
##### Top 10 the least expensive brands in bag
##### Price variance among bags within a brand


*shoes*  
##### the most expensive shoes with its brand
##### the least expensive shoes with its brand
##### Top 10 the most expensive brands in shoes
##### Top 10 the least expensive brands in shoes
##### Price variance among shoes within a brand

* By gender
##### Male (Top men's brand ---product & price)
##### Female (Top women's brand ---product & price)



## Screenshot example: 
![Rplot49](./img/Rplot49.png)

## Code Examples

`menb_price <- gender_male2[order(gender_male2$price,decreasing = FALSE),]`
`menb_price1 <- (menb_price$brand=="Prada")|(menb_price$brand=="Thom Browne")|(menb_price$brand=="Bottega Veneta")|(menb_price$brand=="Fendi")|(menb_price$brand=="Brunello Cucinelli")|(menb_price$brand=="Valextra")|(menb_price$brand=="Loewe")|(menb_price$brand=="Tom Ford")|(menb_price$brand=="Alexander McQueen")|(menb_price$brand=="Saint Laurent")`

`menb_price2 <- menb_price[menb_price1,]`

`v11 <- ggplot(data=menb_price2, aes(x=brand, y=price))+`
       `geom_boxplot(aes(fill=brand), outlier.color = NA)+
       coord_cartesian(ylim = c(0,5000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in male bag")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
v11`

## Data: 
farfetch_2_20.csv 
3146 brands from farfetch 

