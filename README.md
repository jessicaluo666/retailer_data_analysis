# retailer_data_analysis
retailer data 


#price
price <- farfetch$price_without_currency_symbol
price

max(category_clothing$price)  ------------#63823
min(category_clothing$price)  ------------#12

category_clothing <- farfetch1[farfetch1$category =="Clothing",]



category_clothing$price <- unfactor(category_clothing$price)


expensive_clothing <- category_clothing[category_clothing$price == 63823,]
expensive_clothing          ------------#Cara Mila




filt1 <- category_clothing[category_clothing$price > 19000 & category_clothing$price <= 63823 ,]
filt1
nrow(filt1)

library(ggplot2)



-------------#point graph : the most expensive clothings with its brand----------------------------

v2 <- ggplot(data=filt1,aes(x=brand, y=price))+
  geom_point(aes(x=brand, y=price),size=2)+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("the most expensive clothings with its brand") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v2
