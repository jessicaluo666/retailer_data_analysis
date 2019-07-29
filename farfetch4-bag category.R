
category_bag <- farfetch1[farfetch1$category =="Bags",]



max(category_bag$price)  ------------#15900
min(category_bag$price)  ------------#18
  

  
  
expensive_bag <- category_bag[category_bag$price > 5900 & category_bag$price <= 15900 ,]
expensive_bag 
nrow(expensive_bag)


library(ggplot2)
----------#boxplot graph: the most expensive bags with its brand----------------------------

expensive_bag1 <- ggplot(data=expensive_bag,x=brand)+
  geom_bar(aes(x=brand), fill="Dark green")+
  xlab("Brand")+
  ggtitle("the most expensive bags with its brand ") +
  coord_cartesian(ylim=c(0,8)) +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
expensive_bag1

-------------#point graph: the most expensive bags with its brand----------------------------
expensive_bag2 <- ggplot(data=expensive_bag,aes(x=brand, y=price))+
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
expensive_bag2

---------------#point graph: The least expensive bags with its brand--------
cheap_bag <- category_bag[category_bag$price >= 18 &category_clothing$price < 40,]
cheap_bag

cheap_bag1 <- ggplot(data=cheap_bag,x=brand)+
  geom_point(aes(x=brand, y=price), size=3)+
  xlab("Brand")+
  ylab("Price")+
  coord_cartesian(ylim = c(0,40))+
  ggtitle("The least expensive bags with its brand") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=12),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
cheap_bag1

----------#boxplot praph:The least expensive bags with its brand--------

cheap_bag2 <- ggplot(data=cheap_bag,x=brand)+
  geom_bar(aes(x=brand), fill="Dark green")+
  xlab("Brand")+
  coord_cartesian(ylim = c(0,20))+
  ggtitle("The least expensive bags with its brand") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=12),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
cheap_bag2


#"Thom Browne","Oscar de la Renta","Fendi","Stalvey","Dolce & Gabbana","Tyler Ellis","The Row","Bottega Veneta","Philipp Plein","Prada","Readymade"
#"Adidas","Kappa","Nike","Coach","No Ka' Oi","Less Bore","Fila","Anya Hindmarch","Soci√©t√© Anonyme"

----------#boxplot graph: Top 10 the most expensive brands in bag with price--------------------------------

filt6 <- (category_bag$brand =="Thom Browne")|(category_bag$brand =="Oscar de la Renta")|(category_bag$brand =="Fendi")|(category_bag$brand =="Stalvey")|(category_bag$brand =="Dolce & Gabbana")|(category_bag$brand =="Tyler Ellis")|(category_bag$brand =="The Row")|(category_bag$brand =="Bottega Veneta")|(category_bag$brand =="Philipp Plein")|(category_bag$brand =="Prada")|(category_bag$brand =="Readymade")
filt6
expensive_bagbrand <- category_bag[filt6,]
expensive_bagbrand  

expensive_bagbrand1 <- ggplot(data=expensive_bagbrand, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,15000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in bag")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
expensive_bagbrand1





#"Adidas","Kappa","Nike","Coach","No Ka' Oi","Less Bore","Fila","Anya Hindmarch","Soci√©t√© Anonyme"
---------------#boxplot graph:Top 10 the lease expensive brands in bag--------------------------------


filt7 <- (category_bag$brand =="Adidas")|(category_bag$brand =="Kappa")|(category_bag$brand =="Nike")|(category_bag$brand =="Coach")|(category_bag$brand =="No Ka' Oi")|(category_bag$brand =="Less Bore")|(category_bag$brand =="Fila")|(category_bag$brand =="Soci√©t√© Anonyme")
filt7
cheap_bagbrand <- category_bag[filt7,]
cheap_bagbrand  

cheap_bagbrand1 <- ggplot(data=cheap_bagbrand, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,750))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top the least expensive brands in bag")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        text = element_text(family = "Comic Sans MS"))
cheap_bagbrand1


-------#Anya Hindmarch products' in different categories with price----------


ah2
 ah2 <- ggplot(data=ah1, aes(x=category, y=price))+
     geom_boxplot(aes(fill=category), outlier.color = NA)+
   coord_cartesian(ylim = c(0,2000))+
     xlab("Category")+
   ylab("Price")+
     ggtitle("Brand-Anya Hindmarch") +
     theme(axis.title.x = element_text(color="Black", size=30),
                     axis.title.y = element_text(color="Black", size=30),
                    axis.text.x = element_text(color="BLack", size=10),
                    axis.text.y = element_text(color="Black", size=15),
                   plot.title = element_text(size = 40),
                    legend.title = element_text(size=10),
                    legend.text = element_text(size=10),
                    text = element_text(family = "Comic Sans MS"))


