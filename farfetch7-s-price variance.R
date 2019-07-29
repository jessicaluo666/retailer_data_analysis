


#"Swear","Paul Andrew","Toga Pulla","Bontoni","Sebastian Tarek","Yeezy","Saint Laurent","Philipp Plein","Attico","Jimmy Choo"

#"Birkenstock","Diesel","Pajar","Adidas","Nike","Adidas By Pharrell Williams","Tommy Hilfiger","Fila","The North Face","Versace Jeans"




--------------#Swear's price variance----------------

#Swear
swear <- category_shoes[category_shoes$brand== "Swear",]
swear_price <- swear$price
swear_variance <- var(swear_price)
swear_variance            #441757.5

--------------#Paul Andrew's price variance----------------

#Paul Andrew
paul<- category_shoes[category_shoes$brand== "Paul Andrew",]
paul_price <- paul$price
paul_variance <- var(paul_price)
paul_variance          #201725.5


--------------#Toga Pulla's price variance----------------

#Toga Pulla
toga<- category_shoes[category_shoes$brand== "Toga Pulla",]
toga_price <- toga$price
toga_variance <- var(toga_price)
toga_variance          #2058164


--------------#Bontoni's price variance----------------

#Bontoni
bontoni<- category_shoes[category_shoes$brand== "Bontoni",]
bontoni_price <- bontoni$price
bontoni_variance <- var(bontoni_price)
bontoni_variance          #630087.8


--------------#Sebastian Tarek's price variance----------------

#Sebastian Tarek
sebastian<- category_shoes[category_shoes$brand== "Sebastian Tarek",]
sebastian_price <- sebastian$price
sebastian_variance <- var(sebastian_price)
sebastian_variance          #144525


--------------#Yeezy's price variance----------------

#Yeezy
yeezy<- category_shoes[category_shoes$brand== "Yeezy",]
yeezy_price <- yeezy$price
yeezy_variance <- var(yeezy_price)
yeezy_variance          #204687


--------------#Saint Laurent's price variance----------------

#Saint Laurent
sl<- category_shoes[category_shoes$brand== "Saint Laurent",]
sl_price <- sl$price
sl_variance <- var(sl_price)
sl_variance          #186336.2


--------------#Philipp Plein's price variance----------------

#Philipp Plein
pp4<- category_shoes[category_shoes$brand== "Philipp Plein",]
pp1_price <- pp4$price
pp1_variance <- var(pp1_price)
pp1_variance          #316477


--------------#Attico's price variance----------------
#Attico
attico<- category_shoes[category_shoes$brand== "Attico",]
attico_price <- attico$price
attico_variance <- var(attico_price)
attico_variance          #332520.8


--------------#Jimmy Choo's price variance----------------
#Jimmy Choo
jimmy<- category_shoes[category_shoes$brand== "Jimmy Choo",]
jimmy_price <- jimmy$price
jimmy_variance <- var(jimmy_price)
jimmy_variance          #110843.3


--------------#Price variance of expensive bags among brands-------------------


brand5 <- c("Swear","Paul Andrew","Toga Pulla","Bontoni","Sebastian Tarek","Yeezy","Saint Laurent","Philipp Plein","Attico","Jimmy Choo")
variance5 <- c(441757.5,201725.5,2058164,630087.8,144525,204687,186336.2,316477,332520.8,110843.3)


top_shoes_variance <- cbind(brand5,variance5)
top_shoes_variance
top_shoes_variance1 <- data.frame(brand5,variance5)
top_shoes_variance1[order(top_shoes_variance1$variance5,decreasing = FALSE),]


v12 <- ggplot(data=top_shoes_variance1, aes(x=brand5, y=variance5))+
  geom_point(aes(x=brand5, y=variance5), size=3)+
  xlab("Brand")+
  ylab("Variance")+
  ggtitle("Price variance of shoes among brands ") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v12

-------#Toga Pulla products' in different categories with price----------

toga1 <- farfetch1[farfetch1$brand == "Toga Pulla",]
toga1
toga2  <- ggplot(data=toga1, aes(x=category, y=price))+
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,1500))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Toga Pulla") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        text = element_text(family = "Comic Sans MS"))

toga2







#"Birkenstock","Diesel","Pajar","Adidas","Nike","Adidas By Pharrell Williams","Tommy Hilfiger","Fila","The North Face","Versace Jeans"


------------#Birkenstock's price variance-----------------

#Birkenstock
birkenstock <- category_shoes[category_shoes$brand== "Birkenstock",]
birkenstock_price <- birkenstock$price
birkenstock_variance <- var(birkenstock_price)
birkenstock_variance            #3780.246


------------#Diesel's price variance-----------------

#Diesel
diesel3 <- category_shoes[category_shoes$brand== "Diesel",]
diesel3_price <- diesel3$price
diesel3_variance <- var(diesel3_price)
diesel3_variance            #6258.312

------------#Pajar's price variance-----------------

#Pajar
pajar <- category_shoes[category_shoes$brand== "Pajar",]
pajar_price <- pajar$price
pajar_variance <- var(pajar_price)
pajar_variance            #27956.63

------------#Adidas's price variance-----------------
#Adidas
adidas2 <- category_shoes[category_shoes$brand== "Adidas",]
adidas2_price <- adidas2$price
adidas2_variance <- var(adidas2_price)
adidas2_variance            #20919.96

------------#Nike's price variance-----------------
#Nike
nike3 <- category_shoes[category_shoes$brand== "Nike",]
nike3_price <- nike3$price
nike3_variance <- var(nike3_price)
nike3_variance            #58917.16

--------#Adidas By Pharrell Williams's price variance-----------------
#Adidas By Pharrell Williams
adidaspw <- category_shoes[category_shoes$brand== "Adidas By Pharrell Williams",]
adidaspw_price <- adidaspw$price
adidaspw_variance <- var(adidaspw_price)
adidaspw_variance            #14761.89


------------#Tommy Hilfiger's price variance-----------------
#Tommy Hilfiger
tommy <- category_shoes[category_shoes$brand== "Tommy Hilfiger",]
tommy_price <- tommy$price
tommy_variance <- var(tommy_price)
tommy_variance            #3614.753


------------#Fila's price variance-----------------
#Fila
fila1 <- category_shoes[category_shoes$brand== "Fila",]
fila1_price <- fila1$price
fila1_variance <- var(fila1_price)
fila1_variance            #3138.877

------------#The North Face's price variance-----------------
#The North Face
north <- category_shoes[category_shoes$brand== "The North Face",]
north_price <- north$price
north_variance <- var(north_price)
north_variance           #5618

------------#Versace Jeans's price variance-----------------
#Versace Jeans
verjeans <- category_shoes[category_shoes$brand== "Versace Jeans",]
verjeans_price <- verjeans$price
verjeans_variance <- var(verjeans_price)
verjeans_variance           #4357.204



--------------#Price variance of least expensive shoes among brands-------------------


brand6 <- c("Birkenstock","Diesel","Pajar","Adidas","Nike","Adidas By Pharrell Williams","Tommy Hilfiger","Fila","The North Face","Versace Jeans")
variance6 <- c(3780.246,6258.312,27956.63,20919.96,58917.16,14761.89,3614.753,3138.877,5618,4357.204)


bottom_shoes_variance <- cbind(brand6,variance6)
bottom_shoes_variance
bottom_shoes_variance1 <- data.frame(brand6,variance6)
bottom_shoes_variance1[order(bottom_shoes_variance1$variance6,decreasing = FALSE),]


v13 <- ggplot(data=bottom_shoes_variance1, aes(x=brand6, y=variance6))+
  geom_point(aes(x=brand6, y=variance6), size=3)+
  xlab("Brand")+
  ylab("Variance")+
  ggtitle("Price variance of shoes among brands ") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v13






