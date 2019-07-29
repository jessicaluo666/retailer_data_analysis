



#"Thom Browne","Oscar de la Renta","Fendi","Stalvey","Dolce & Gabbana","Tyler Ellis","The Row","Bottega Veneta","Philipp Plein","Prada","Readymade"
#"Adidas","Kappa","Nike","Coach","No Ka' Oi","Less Bore","Fila","Anya Hindmarch","Soci√©t√© Anonyme"



--------------#Thom Browne's price variance----------------
#Thom Browne
tb3 <- category_bag[category_bag$brand== "Thom Browne",]
tb_price <- tb4$price
tb_variance <- var(tb_price)
tb_variance            #2141052

--------------#Oscar de la Renta's price variance----------------
#Oscar de la Renta
oscar2 <- category_bag[category_bag$brand== "Oscar de la Renta",]
oscar_price <- oscar2$price
oscar_variance <- var(oscar_price)
oscar_variance          #3212108


--------------#Fendi's price variance----------------
#Fendi
fendi <- category_bag[category_bag$brand== "Fendi",]
fendi_price <- fendi1$price
fendi_variance <- var(fendi_price)
fendi_variance          #848985.7

--------------#Stalvey's price variance----------------
#Stalvey
stalvey2 <- category_bag[category_bag$brand== "Stalvey",]
stalvey_price <- stalvey2$price
stalvey_variance <- var(stalvey_price)
stalvey_variance          #112338

--------------#Dolce & Gabbana's price variance----------------
#Dolce & Gabbana
dg <- category_bag[category_bag$brand== "Dolce & Gabbana",]
dg_price <- dg$price
dg_variance <- var(dg_price)
dg_variance          #1111186

--------------#Tyler Ellis's price variance----------------
#Tyler Ellis
te <- category_bag[category_bag$brand== "Tyler Ellis",]
te_price <- te$price
te_variance <- var(te_price)
te_variance          #1160413

--------------#The Row's price variance----------------
#The Row
the_row <- category_bag[category_bag$brand== "The Row",]
therow_price <- the_row$price
therow_variance <- var(therow_price)
therow_variance          #2543024


--------------#Bottega Veneta's price variance----------------
#Bottega Veneta
bv <- category_bag[category_bag$brand== "Bottega Veneta",]
bv_price <- bv$price
bv_variance <- var(bv_price)
bv_variance          #984543.3


--------------#Philipp Plein's price variance----------------
#Philipp Plein
pp3 <- category_bag[category_bag$brand== "Philipp Plein",]
pp_price <- pp3$price
pp_variance <- var(pp_price)
pp_variance          #666458.2


--------------#Prada's price variance----------------
#Prada
prada1 <- category_bag[category_bag$brand== "Prada",]
prada_price <- prada1$price
prada_variance <- var(prada_price)
prada_variance          #837701.7
 

--------------#Readymade's price variance----------------
#Readymade
readymade <- category_bag[category_bag$brand== "Readymade",]
readymade_price <- readymade$price
readymade_variance <- var(readymade_price)
readymade_variance          #2918709



--------------#Price variance of expensive bags among brands-------------------

brand3 <- c("Thom Browne","Oscar de la Renta","Fendi","Stalvey","Dolce & Gabbana","Tyler Ellis","The Row","Bottega Veneta","Philipp Plein","Prada","Readymade")
variance3 <- c(2141052,3212108,848985.7,112338,1111186,1160413,2543024,984543.3,666458.2,837701.7,2918709)



top_bag_variance <- cbind(brand3,variance3)
top_bag_variance
top_bag_variance1 <- data.frame(brand3,variance3)
top_bag_variance1[order(top_bag_variance1$variance3,decreasing = FALSE),]


v10 <- ggplot(data=top_bag_variance1, aes(x=brand3, y=variance3))+
  geom_point(aes(x=brand3, y=variance3), size=3)+
  xlab("Brand")+
  ylab("Variance")+
  ggtitle("Price variance of bag among brands ") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v10




--------------#Price variance of least expensive bags among brands-------------------

#"Adidas","Kappa","Nike","Coach","No Ka' Oi","Less Bore","Fila","Anya Hindmarch","Soci√©t√© Anonyme"


--------------#Adidas's price variance----------------
#Adidas
adidas1 <- category_bag[category_bag$brand== "Adidas",]
adidas_price <- adidas1$price
adidas_variance <- var(adidas_price)
adidas_variance            #3837.297

--------------#Kappa's price variance---------------
#Kappa
kappa <- category_bag[category_bag$brand== "Kappa",]
kappa_price <- kappa$price
kappa_variance <- var(kappa_price)
kappa_variance          #392


--------------#Nike's price variance----------------
#Nike
nike2 <- category_bag[category_bag$brand== "Nike",]
nike_price <- nike2$price
nike_variance <- var(nike_price)
nike_variance          #8

--------------#Coach's price variance----------------
#Coach
coach <- category_bag[category_bag$brand== "Coach",]
coach_price <- coach$price
coach_variance <- var(coach_price)
coach_variance          #40992.44

--------------#No Ka' Oi's price variance----------------
#No Ka' Oi
nko <- category_bag[category_bag$brand== "No Ka' Oi",]
nko_price <- nko$price
nko_variance <- var(nko_price)
nko_variance          #1711.974

--------------#Less Bore's price variance----------------
#Less Bore
lessbore <- category_bag[category_bag$brand== "Less Bore",]
lessbore_price <- lessbore$price
lessbore_variance <- var(lessbore_price)
lessbore_variance          #21336.33

--------------#Fila's price variance----------------
#Fila
fila <- category_bag[category_bag$brand== "Fila",]
fila_price <- fila$price
fila_variance <- var(fila_price)
fila_variance          #1098.619


-------------#Anya Hindmarch's price variance----------------
#Anya Hindmarch
ah1 <- category_bag[category_bag$brand== "Anya Hindmarch",]
ah_price <- ah1$price
ah_variance <- var(ah_price)
ah_variance          #297096.8


--------------#Soci√©t√© Anonyme's price variance----------------
#Soci√©t√© Anonyme
soci <- category_bag[category_bag$brand== "Soci√©t√© Anonyme",]
soci_price <- soci$price
soci_variance <- var(soci_price)
soci_variance          #0


--------------#Price variance of least expensive bags among brands-------------------

brand4 <- c("Adidas","Kappa","Nike","Coach","No Ka' Oi","Less Bore","Fila","Anya Hindmarch","Soci√©t√© Anonyme")
variance4 <- c(3837.297,392,8,40992.44,1711.974,21336.33,1098.619,297096.8,0)


bottom_bag_variance <- cbind(brand4,variance4)
bottom_bag_variance
bottom_bag_variance1 <- data.frame(brand4,variance4)
bottom_bag_variance1[order(bottom_bag_variance1$variance4,decreasing = FALSE),]


v10 <- ggplot(data=bottom_bag_variance1, aes(x=brand4, y=variance4))+
  geom_point(aes(x=brand4, y=variance4), size=3)+
  xlab("Brand")+
  ylab("Variance")+
  ggtitle("Price variance of bag among brands ") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v10


