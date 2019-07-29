
farfetch1 <- data.frame(brand,price,gender,category)
head(farfetch1)


category_shoes <- farfetch1[farfetch1$category =="Shoes" ,]

max(category_shoes$price) #7065
min(category_shoes$price) #24

expensive_shoes <- category_shoes[category_shoes$price <= 7065 &category_shoes$price >3500,]
expensive_shoes


---------#point graph: the most expensive shoes with its brand----------------------------


v6 <- ggplot(data=expensive_shoes,aes(x=brand, y=price))+
  geom_point(aes(x=brand, y=price),size=3)+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("the most expensive shoes with its brand")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v6
---------#boxplot graph: the most expensive shoes with its brand----------------------------

filt5 <- (category_shoes$brand =="Attico")|(category_shoes$brand =="Bontoni")|(category_shoes$brand =="Jimmy Choo")|(category_shoes$brand =="Paul Andrew")|(category_shoes$brand =="Philipp Plein")|(category_shoes$brand =="Saint Laurent")|(category_shoes$brand =="Sebastian Tarek")|(category_shoes$brand =="Swear")|(category_shoes$brand =="Toga Pulla")|(category_shoes$brand =="Yeezy")
filt5
expensive_shoesbrand <- category_shoes[filt5,]
expensive_shoesbrand 
head(expensive_shoesbrand)

expensive_shoesbrand1 <- ggplot(data=expensive_shoesbrand, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,4000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in shoes")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=10),
        legend.text = element_text(size=15),
        text = element_text(family = "Comic Sans MS"))
expensive_shoesbrand1  


-----------#Sebastian Tarek's products' in different categories with price -------------------------------------------------------------------------
st  <- farfetch1[farfetch1$brand == "Sebastian Tarek",]
st 

st1  <- ggplot(data=st, aes(x= gender , y=price))+
  geom_point(aes(x= gender, y=price), size=5)+
  coord_cartesian(ylim = c(0,6000))+
  xlab("Price")+
  ggtitle("Brand-Sebastian Tarek") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=10),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
st1



-----------#point graph: the least expensive shoes with its brand-----------------------------------------
  
cheap_shoes <- category_shoes[category_shoes$price <= 43 &category_bag$price >=24,]
cheap_shoes

v6 <- ggplot(data=cheap_shoes,aes(x=brand, y=price))+
  geom_point(aes(x=brand, y=price),size=2)+
  coord_cartesian(ylim = c(0,60))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("the least expensive 10 shoes with its brand") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=10),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
v6

