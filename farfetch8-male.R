
------------------#male----------------------

gender_male<- farfetch1[farfetch1$gender =="Male",]

gender_male

summary(gender_male)  #87572 products  

#prada          : 2494 
#Dsquared2      : 2389
#Dolce & Gabbana: 1727
#Diesel         : 1581
#Saint Laurent  : 1256 
#Philipp Plein  : 1206



-------#Brands with the most men products-----------

male <- (gender_male$brand =="Dsquared2")|(gender_male$brand =="Prada")|(gender_male$brand =="Diesel")|(gender_male$brand =="Dolce & Gabbana")|(gender_male$brand =="Saint Laurent")|(gender_male$brand =="Philipp Plein")

male2 <- gender_male[male,]

male3 <- ggplot(data=male2,aes(x=brand))+ 
  geom_bar(aes(fill=category)) +
  xlab("Top brands ")+
  ylab("the number of products") +
  ggtitle("Brands with the most men products") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
male3



----------------#clothings------------------

gender_male1<- farfetch1[farfetch1$gender =="Male"&farfetch1$category=="Clothing",]

gender_male1      #60041

summary(gender_male1)
#Dsquared2              : 1615
#Prada                  : 1231
#Diesel                 : 1176
#Dolce & Gabbana        : 1105
#Ami Alexandre Mattiussi:  998
#Neil Barrett           :  919 


-------------#Top brands have the most male clothing products-------------

clothing1<- (gender_male1$brand =="Dsquared2")|(gender_male1$brand =="Prada")|(gender_male1$brand =="Diesel")|(gender_male1$brand =="Dolce & Gabbana")|(gender_male1$brand =="Ami Alexandre Mattiussi")|(gender_male1$brand =="Neil Barrett")

clothing2 <- gender_male1[clothing1,]

options(max.print = 10000000)

men_clothing <- ggplot(data=clothing2,aes(x=brand))+ 
  geom_bar(fill="Dark green") +
  xlab("Top brands ")+
  ylab("the number of products") +
  ggtitle("Top brands have the most clothing products") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
men_clothing

--------------#boxplot graph: Top 10 the most expensive brands in male clothing------

menc_price <- gender_male1[order(gender_male1$price,decreasing = FALSE),]

menc_price1 <- (menc_price$brand=="Billionaire")|(menc_price$brand=="Thom Browne")|(menc_price$brand=="Philipp Plein")|(menc_price$brand=="Takahiromiyashita The Soloist")|(menc_price$brand=="Versace")|(menc_price$brand=="Balmain")|(menc_price$brand=="Saint Laurent")|(menc_price$brand=="Alexander McQueen")|(menc_price$brand=="Kiton")|(menc_price$brand=="Visvim")

menc_price2 <- menc_price[menc_price1,]

library(ggplot2)

v10 <- ggplot(data=menc_price2, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,5000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in male clothing")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
v10





----------------#bags------------------



gender_male2<- farfetch1[farfetch1$gender =="Male"&farfetch1$category=="Bags",]

gender_male2

summary(gender_male2)
# Prada        : 307
# Fendi        : 178
# Valentino    : 123
# Saint Laurent: 114
# Burberry     : 110
# Givenchy     : 107

-------------#Top brands have the most male bag products-------------

bag1<- (gender_male2$brand =="Prada")|(gender_male2$brand =="Fendi")|(gender_male2$brand =="Valentino")|(gender_male2$brand =="Saint Laurent")|(gender_male2$brand =="Burberry")|(gender_male2$brand =="Givenchy")

bag2 <- gender_male2[bag1,]

men_bag <- ggplot(data=bag2,aes(x=brand))+ 
  geom_bar(fill="Dark green") +
  xlab("Top brands ")+
  ylab("the number of products") +
  ggtitle("Top brands have the most bag products") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
men_bag


-------------#boxplot graph: Top 10 the most expensive brands in male bags----------

menb_price <- gender_male2[order(gender_male2$price,decreasing = FALSE),]


menb_price1 <- (menb_price$brand=="Prada")|(menb_price$brand=="Thom Browne")|(menb_price$brand=="Bottega Veneta")|(menb_price$brand=="Fendi")|(menb_price$brand=="Brunello Cucinelli")|(menb_price$brand=="Valextra")|(menb_price$brand=="Loewe")|(menb_price$brand=="Tom Ford")|(menb_price$brand=="Alexander McQueen")|(menb_price$brand=="Saint Laurent")

menb_price2 <- menb_price[menb_price1,]


v11 <- ggplot(data=menb_price2, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
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
v11


----------------#shoes------------------

gender_male3<- farfetch1[farfetch1$gender =="Male"&farfetch1$category=="Shoes",]

gender_male3

summary(gender_male3)
#Nike      :  739
#Adidas    :  713
#Tod's     :  413
#Mars√®ll  :  381
#Prada     :  368
#Hogan     :  315

-------------#Top brands have the most male shoes products-------------

shoes1<- (gender_male3$brand =="Nike")|(gender_male3$brand =="Adidas")|(gender_male3$brand =="Tod's")|(gender_male3$brand =="Mars√®ll")|(gender_male3$brand =="Prada")|(gender_male3$brand =="Hogan")

shoes2 <- gender_male3[shoes1,]

men_shoes <- ggplot(data=shoes2,aes(x=brand))+ 
  geom_bar(fill="Dark green")+
  xlab("Top brands ")+
  ylab("the number of products") +
  ggtitle("Top brands have the most shoes products") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
men_shoes



-------------#boxplot graph: Top 10 the most expensive brands in male shoes----------

mens_price <- gender_male3[order(gender_male3$price,decreasing = FALSE),]


mens_price1 <- (mens_price$brand=="Jimmy Choo")|(mens_price$brand=="Swear")|(mens_price$brand=="Yeezy")|(mens_price$brand=="Sebastian Tarek")|(mens_price$brand=="Bontoni")|(mens_price$brand=="Good Art Hlywd")|(mens_price$brand=="Guidi")|(mens_price$brand=="Yohji Yamamoto")|(mens_price$brand=="Isaac Sellam Experience")|(mens_price$brand=="Nike")

mens_price2 <- mens_price[mens_price1,]


v12 <- ggplot(data=mens_price2, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,4000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in male shoes")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
v12
