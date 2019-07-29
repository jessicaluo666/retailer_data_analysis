
---------------------#female----------------------------
gender_female<- farfetch1[farfetch1$gender =="Female",]

gender_female

summary(gender_female)

#Prada           :  3125
#Saint Laurent   :  2612
#Dolce & Gabbana :  2552
#Stella McCartney:  2198
#Fendi           :  2038
#Marni           :  2030

-------#Brands with the most women products-----------

female <- (gender_female$brand =="Fendi")|(gender_female$brand =="Prada")|(gender_female$brand =="Stella McCartney")|(gender_female$brand =="Dolce & Gabbana")|(gender_female$brand =="Saint Laurent")|(gender_female$brand =="Marni")

female2 <- gender_female[female,]

female3 <- ggplot(data=female2,aes(x=brand))+ 
  geom_bar(aes(fill=category)) +
  xlab("Top brands ")+
  ylab("the number of products") +
  ggtitle("Brands with the most women products") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
female3


----------------#clothings------------------

gender_female1<- farfetch1[farfetch1$gender =="Female"&farfetch1$category=="Clothing",]

gender_female1      

summary(gender_female1)
#P.A.R.O.S.H.    :  1575
#MSGM            :  1514 
#Stella McCartney:  1341
#Dsquared2       :  1209
#Dolce & Gabbana :  1130
#Saint Laurent   :  1098

-------------#Top brands have the most female clothing products-------------

clothing3<- (gender_female1$brand =="P.A.R.O.S.H.")|(gender_female1$brand =="MSGM")|(gender_female1$brand =="Stella McCartney")|(gender_female1$brand =="Dolce & Gabbana")|(gender_female1$brand =="Dsquared2")|(gender_female1$brand =="Saint Laurent")

clothing4 <- gender_female1[clothing3,]

women_clothing <- ggplot(data=clothing4,aes(x=brand))+ 
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
women_clothing


-------------#boxplot graph:Top 10 the most expensive brands in female clothings----------


womenc_price <- gender_female1[order(gender_female1$price,decreasing = FALSE),]

womenc_price1 <- (womenc_price$brand=="Cara Mila")|(womenc_price$brand=="Thom Browne")|(womenc_price$brand=="Liska")|(womenc_price$brand=="Marni")|(womenc_price$brand=="Marchesa")|(womenc_price$brand=="Rubin Singer")|(womenc_price$brand=="Giambattista Valli")|(womenc_price$brand=="Zuhair Murad")|(womenc_price$brand=="Philipp Plein")|(womenc_price$brand=="Dolce & Gabbana")

womenc_price2 <- womenc_price[womenc_price1,]

library(ggplot2)

v15 <- ggplot(data=womenc_price2, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,5000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in female clothing")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
v15


----------------------------#bags--------------------------------------



gender_female2<- farfetch1[farfetch1$gender =="Female"&farfetch1$category=="Bags",]

gender_female2

summary(gender_female2)
# Prada               :  862
# Michael Michael Kors:  696
# Fendi               :  665
# Saint Laurent       :  579
# Furla               :  493
# Valentino           :  481

-------------#Top brands have the most female bag products-------------


bag3<- (gender_female2$brand =="Prada")|(gender_female2$brand =="Fendi")|(gender_female2$brand =="Michael Michael Kors")|(gender_female2$brand =="Saint Laurent")|(gender_female2$brand =="Furla")|(gender_female2$brand =="Valentino")

bag4 <- gender_female2[bag3,]

women_bag <- ggplot(data=bag4,aes(x=brand))+ 
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
women_bag

womenb_price <- gender_female2[order(gender_female2$price,decreasing = FALSE),]



-------------#boxplot graph: Top 10 the most expensive brands in female bags----------


womenb_price <- gender_female2[order(gender_female2$price,decreasing = FALSE),]

womenb_priceE <- gender_female2[order(gender_female2$price,decreasing = TRUE),]

womenb_price1 <- (womenb_price$brand=="Stalvey")|(womenb_price$brand=="Fendi")|(womenb_price$brand=="Bottega Veneta")|(womenb_price$brand=="Dolce & Gabbana")|(womenb_price$brand=="Oscar de la Renta")|(womenb_price$brand=="Tyler Ellis")|(womenb_price$brand=="The Row")|(womenb_price$brand=="Philipp Plein")|(womenb_price$brand=="Brunello Cucinelli")|(womenb_price$brand=="Thom Browne")

womenb_price2 <- womenb_price[womenb_price1,]


v13 <- ggplot(data=womenb_price2, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,7500))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in female bag")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))
v13



---------------------------#shoes------------------------------------------


gender_female3<- farfetch1[farfetch1$gender =="Female"&farfetch1$category=="Shoes",]

gender_female3

summary(gender_female3)
#Jimmy Choo      : 1000
#Saint Laurent   :  642
#Giuseppe Zanotti:  583
#Mars√®ll        :  582
#Sergio Rossi    :  576
#Prada           :  537

-------------#Top brands have the most female bag products-------------


shoes3<- (gender_female3$brand =="Jimmy Choo")|(gender_female3$brand =="Saint Laurent")|(gender_female3$brand =="Giuseppe Zanotti")|(gender_female3$brand =="Mars√®ll")|(gender_female3$brand =="Prada")|(gender_female3$brand =="Sergio Rossi")

shoes4 <- gender_female3[shoes3,]

women_shoes <- ggplot(data=shoes4,aes(x=brand))+ 
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
women_shoes



------------#boxplot Top 10 the most expensive brands in female shoes----------


womens_price <- gender_female3[order(gender_female3$price,decreasing = FALSE),]

womens_priceE <- gender_female3[order(gender_female3$price,decreasing = TRUE),]

womens_price1 <- (womens_price$brand=="Philipp Plein")|(womens_price$brand=="Saint Laurent")|(womens_price$brand=="Paul Andrew")|(womens_price$brand=="Toga Pulla")|(womens_price$brand=="Attico")|(womens_price$brand=="Sebastian Tarek")|(womens_price$brand=="Miu Miu")|(womens_price$brand=="Balmain")|(womens_price$brand=="Swear")|(womens_price$brand=="Ermanno Scervino")

womens_price2 <- womens_price[womens_price1,]


v14 <- ggplot(data=womens_price2, aes(x=brand, y=price))+
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
        text = element_text(family = "Comic Sans MS"))}
v14




