


category_clothing <- farfetch1[farfetch1$category =="Clothing",]

-------------#Philipp Plein products' in different  categories with price------------------------------------------

pp1 <- farfetch1[farfetch1$brand == "Philipp Plein",]
pp1

graph.function(data=pp1) 

pp2 <- graph.function(data=pp1) +
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,3500))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Philipp Plein")
pp2




-------------#Liska products' in different categories with price------------------------------------------

liska <- farfetch1[farfetch1$brand == "Liska",]
liska

graph.function(data=liska) 

liska1 <- graph.function(data=liska) +
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,10000))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Liska")

liska1

-------------#Thom Browne products' in different categories with price---------------------------

tb <- farfetch1[farfetch1$brand == "Thom Browne",]
tb

graph.function(data=tb) 

tb1 <- graph.function(data=tb) +
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,5000))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Thom Browne")

tb1


-------# marchesa products' in different categories with price--------------
  
marchesa<- farfetch1[farfetch1$brand == "Marchesa",]
marchesa

marchesa1  <- 
  graph.function(data=marchesa) +
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,10000))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Marchesa")

marchesa1

----------------#Cara Mila products' in different categories with price----------------------------
  
caramila<- farfetch1[farfetch1$brand == "Cara Mila",]
caramila

caramila1  <- 
  graph.function(data=caramila) +
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,10000))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Cara Mila")

caramila1


-------------#boxplot graph: Top 10 the most expensive brands in clothing with price--------------------------------


filt2 <- (category_clothing$brand =="Giambattista Valli")|(category_clothing$brand =="Rubin Singer")|(category_clothing$brand =="Philipp Plein")|(category_clothing$brand =="Liska")|(category_clothing$brand =="Thom Browne")|(category_clothing$brand =="Marchesa")|(category_clothing$brand =="Zuhair Murad")|(category_clothing$brand =="Marni")|(category_clothing$brand =="Cara Mila")|(category_clothing$brand =="Dolce & Gabbana")
filt2
expensive_clothingbrand <- category_clothing[filt2,]
expensive_clothingbrand  

expensive_clothingbrand1 <- ggplot(data=expensive_clothingbrand, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,15000))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the most expensive brands in clothing")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
expensive_clothingbrand1
  
------------#Diesel products' in different  categories with price------------------------------  

diesel <- farfetch1[farfetch1$brand == "Diesel",]
diesel
diesel1  <- ggplot(data=diesel, aes(x=category, y=price))+
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,500))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Diesel") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))

diesel1


------------#Nike products' in different categories with price--------------------------
  
nike <- farfetch1[farfetch1$brand == "Nike",]
nike
nike1 <- ggplot(data=nike, aes(x=category, y=price))+
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,400))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-NIKE") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
nike1
-------------# Fiorucci products' in different  categories with price----------------------------------------------------

Fiorucci <- farfetch1[farfetch1$brand == "Fiorucci",]
Fiorucci
Fiorucci1<- ggplot(data=Fiorucci, aes(x=category, y=price))+
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,400))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Fiorucci") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
Fiorucci1

--------------#Reebok products' in different  categories with price---------------------------------------------------
  
Reebok <- farfetch1[farfetch1$brand == "Reebok",]
Reebok
Reebok1<- ggplot(data=Reebok, aes(x=category, y=price))+
  geom_boxplot(aes(fill=category), outlier.color = NA)+
  coord_cartesian(ylim = c(0,400))+
  xlab("Category")+
  ylab("Price")+
  ggtitle("Brand-Reebok") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 40),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
Reebok1

-------------#boxplot graph: Top 10 the least expensive brands in clothing with price------------


filt3 <- (category_clothing$brand =="Diesel")|(category_clothing$brand =="Adidas")|(category_clothing$brand =="Necessary Anywhere")|(category_clothing$brand =="Nike")|(category_clothing$brand =="Fef√®")|(category_clothing$brand =="Fiorucci")|(category_clothing$brand =="RBN X Bjorn Borg")|(category_clothing$brand ==" Howlin'")|(category_clothing$brand =="Reebok")|(category_clothing$brand =="Adidas By Stella Mccartney")|(category_clothing$brand =="Calvin Klein Underwear")
filt3
cheap_clothingbrand <- category_clothing[filt3,]
cheap_clothingbrand 

cheap_clothingbrand1 <- ggplot(data=cheap_clothingbrand, aes(x=brand, y=price))+
  geom_boxplot(aes(fill=brand), outlier.color = NA)+
  coord_cartesian(ylim = c(0,500))+
  xlab("Brand")+
  ylab("Price")+
  ggtitle("Top 10 the least expensive brands in clothing")+
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        text = element_text(family = "Comic Sans MS"))
cheap_clothingbrand1


#Giambattista Valli" "Rubin Singer""Philipp Plein""Liska""Thom Browne""Marchesa""Zuhair Murad""Marni""Cara Mila""Dolce & Gabbana"
--------#Dolce & Gabbana's price variance---------------------------------
  #Dolce & Gabbana
dg <- category_clothing[category_clothing$brand== "Dolce & Gabbana",]
dg
dg_price <- dg$price
dg_price
dg_variance <- var(dg_price)
dg_variance

--------#Cara Mila's price variance--------------------------------------
#Cara Mila
cm <- category_clothing[category_clothing$brand== "Cara Mila",]
cm
cm_price <- cm$price
cm_price
cm_variance <- var(cm_price)
cm_variance

---------#Marni's price variance-----------------------------------
#Marni
marni2 <- category_clothing[category_clothing$brand== "Marni",]
marni2
marni_price <- marni2$price
marni_price
marni_variance <- var(marni_price)
marni_variance

-------- #Zuhair Murad's price variance------------------------------------
  #Zuhair Murad
zm2<- category_clothing[category_clothing$brand== "Zuhair Murad",]
zm2
zm_price <- zm2$price
zm_price
zm_variance <- var(zm_price)
zm_variance

---------#Marchesa 's price variance------------------------------------
  #Marchesa
marchesa2<- category_clothing[category_clothing$brand== "Marchesa",]
marchesa2
marchesa_price <- marchesa2$price
marchesa_price
marchesa_variance <- var(marchesa_price)
marchesa_variance‘


---------- #Thom Browne's price variance----------------------------
  #Thom Browne
tb2<- category_clothing[category_clothing$brand== "Thom Browne",]
tb2
tb_price <- tb2$price
tb_price
tb_variance <- var(tb_price)
tb_variance

-------------#liska's price variance----------------------------
  #Liska
liska2<- category_clothing[category_clothing$brand== "Liska",]
liska2
liska_price <- liska2$price
liska_price
liska_variance <- var(liska_price)
liska_variance

-------------#Philipp Plein's price variance---------------------------
  #Philipp Plein
pp <-  category_clothing[category_clothing$brand== "Philipp Plein", ]
pp
pp_price <- pp$price
pp_price
pp_variance <- var(pp_price)
pp_variance

------------#Rubin Singer's price variance----------------------------
  #Rubin Singer
rs<-  category_clothing[category_clothing$brand== "Rubin Singer", ]
rs
rs_price <- rs$price
rs_price
rs_variance <- var(rs_price)
rs_variance

--------#Giambattista Valli's price variance----------------------------------
#Giambattista Valli
gv<-  category_clothing[category_clothing$brand== "Giambattista Valli", ]
gv
gv_price <- gv$price
gv_price
gv_variance <- var(gv_price)
gv_variance

-----------#point graph: Price variance of expensive clothing among brands-------------------
  
brand1 <- c("Giambattista Valli","Philipp Plein","Rubin Singer","Liska","Thom Browne","Marchesa","Zuhair Murad","Marni","Cara Mila","Dolce & Gabbana")
variance1 <- c(3933159,1245936,16966258,13856504,5079588,31499878,21021221,2077533,70444132,1586201)


top_clothing_variance <- cbind(brand1,variance1)
top_clothing_variance
top_clothing_variance1 <- data.frame(brand1,variance1)
top_clothing_variance1[order(top_clothing_variance1$variance1,decreasing = FALSE),]

 
v7 <- ggplot(data=top_clothing_variance1, aes(x=brand1, y=variance1))+
  geom_point(aes(x=brand1, y=variance1), size=3)+
  xlab("Brand")+
  ylab("Variance")+
  ggtitle("Price variance of clothing among brands ") +
  theme(axis.title.x = element_text(color="Black", size=30),
        axis.title.y = element_text(color="Black", size=30),
        axis.text.x = element_text(color="BLack", size=10),
        axis.text.y = element_text(color="Black", size=15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        text = element_text(family = "Comic Sans MS"))
  v7

  ------------#point graph:Price variance of least expensive clothing among brands-------------------

  
#"Diesel" "Adidas" "Necessary Anywhere" "Nike" "Fef√®" "Fiorucci" "RBN X Bjorn Borg" " Howlin'" "Reebok" "Adidas By Stella Mccartney" "Calvin Klein Underwear"
  
  
-----------------------#Diesel's price variance---------------------
    #Diesel
diesel2  <- category_clothing[category_clothing$brand== "Diesel", ]
diesel2
diesel_price <- diesel2$price
diesel_price
diesel_variance <- var(diesel_price)
diesel_variance  #24155.3

-----------------------#Adidas's price variance---------------------
#Adidas
adidas  <- category_clothing[category_clothing$brand== "Adidas", ]
adidas
adidas_price <- adidas$price
adidas_price
adidas_variance <- var(adidas_price)
adidas_variance  #7702.901

-------------------#Necessary Anywhere's price variance---------------------
#Necessary Anywhere
na1 <- category_clothing[category_clothing$brand== "Necessary Anywhere", ]
na1
na_price <- na1$price
na_price
na_variance <- var(na_price)
na_variance  #1.327635
  
--------------------#Nike's price variance---------------------
#Nike
nike1 <- category_clothing[category_clothing$brand== "Nike", ]
nike1
nike_price <- nike1$price
nike_price
nike_variance <- var(nike_price)
nike_variance  #15320.52

------------------#Fef√®'s price variance---------------------
#Fef√®
Fef <- category_clothing[category_clothing$brand== "Fef√®", ]
Fef
Fef_price <- Fef$price
Fef_price
Fef_variance <- var(Fef_price)
Fef_variance  #2462.601

--------------------#Fiorucci's price variance---------------------
#Fiorucci
Fiorucci <- category_clothing[category_clothing$brand== "Fiorucci", ]
Fiorucci
Fiorucci_price <- Fiorucci$price
Fiorucci_price
Fiorucci_variance <- var(Fiorucci_price)
Fiorucci_variance  #6528.977

--------------------#RBN X Bjorn Borg's price variance---------------------
#RBN X Bjorn Borg
rbb2 <- category_clothing[category_clothing$brand== "RBN X Bjorn Borg", ]
rbb2
rbb_price <- rbb2$price
rbb_price
rbb_variance <- var(rbb_price)
rbb_variance  #3745.059


-------------------#Howlin's price variance---------------------
#Howlin
Howlin <- category_clothing[category_clothing$brand== "Howlin'", ]
Howlin
Howlin_price <- Howlin$price
Howlin_price
Howlin_variance <- var(Howlin_price)
Howlin_variance  #6608.924


------------------#Reebok's price variance---------------------
#Reebok
Reebok2 <- category_clothing[category_clothing$brand== "Reebok", ]
Reebok2
Reebok_price <- Reebok2$price
Reebok_price
Reebok_variance <- var(Reebok_price)
Reebok_variance  #813.8884


-------------#Adidas By Stella Mccartney's price variance---------------------
#Adidas By Stella Mccartney
abm <- category_clothing[category_clothing$brand== "Adidas By Stella Mccartney", ]
abm
abm_price <- abm$price
abm_price
abm_variance <- var(abm_price)
abm_variance  #7652.146



---------------#Calvin Klein Underwear's price variance---------------------
#Calvin Klein Underwear
cku<- category_clothing[category_clothing$brand== "Calvin Klein Underwear", ]
cku
cku_price <- cku$price
cku_price
cku_variance <- var(cku_price)
cku_variance  #166.2095


-----------#point graph: Price variance of least expensive clothing among brands-------------------

  
  brand2 <- c("Necessary Anywherei","Diesel","Fiorucci","Adidas","Howlin'","Nike","Calvin Klein Underwear","RBN X Bjorn Borg ","Reebok","Adidas By Stella Mccartney","Fef√®")
  variance2 <- c(1.327635,24155.3,6528.977,7702.901,6608.924,15320.52,166.2095,3745.059,813.8884,7652.146,2462.601)
  
  bottom_clothing_variance <- cbind(brand2,variance2)
  bottom_clothing_variance
  bottom_clothing_variance1 <- data.frame(brand2,variance2)
  bottom_clothing_variance1[order(bottom_clothing_variance1$variance2,decreasing = FALSE),]
  
  v9 <- ggplot(data=bottom_clothing_variance1, aes(x=brand2, y=variance2))+
    geom_point(aes(x=brand2, y=variance2), size=3)+
    xlab("Brand")+
    ylab("Variance")+
    ggtitle("Price variance of clothing among brands ") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 20),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  v9 
  
  
  
  
  
  
  
  
  
