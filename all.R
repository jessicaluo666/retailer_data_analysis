# retailer_data_analysis
retailer data 

--------------1_Clothing_category-----------------------
  #price
  price <- farfetch$price_without_currency_symbol
  
  max(category_clothing$price)  ------------#63823
    min(category_clothing$price)  ------------#12
    
    #brand
    brand <- farfetch$brand
  
  #gender
  gender <- farfetch$gender
  
  #category
  category <- farfetch$category
  
  
  farfetch1 <- data.frame(brand,price,gender,category)
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
  
  ---------------#point graph :The least expensive clothings with its brand--------
  
  cheap_clothing <- category_clothing[category_clothing$price >= 12&category_clothing$price < 21,]
  cheap_clothing
  
  v8 <- ggplot(data=cheap_clothing,x=brand)+
    geom_point(aes(x=brand, y=price), size=3)+
    xlab("Brand")+
    ylab("Price")+
    coord_cartesian(ylim = c(0,30))+
    ggtitle("The least expensive clothings with its brand") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=12),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  v8
  
  ---------------#bar graph: The least expensive clothings with its brand--------
  
  v3 <- ggplot(data=cheap_clothing,x=brand)+
    geom_bar(aes(x=brand), fill="Dark green")+
    xlab("Brand")+
    coord_cartesian(ylim = c(0,30))+
    ggtitle("The least expensive clothings with its brand") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=12),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  v3
  
  -------------#Philipp Plein products' in different categories with price------------------------------------------
  
  pp1 <- farfetch1[farfetch1$brand == "Philipp Plein",]
  pp1
  
  
  pp2 <- ggplot(data=pp1, aes(x=category, y=price))+
    geom_boxplot(aes(fill=category), outlier.color = NA)+
    coord_cartesian(ylim = c(0,3500))+
    xlab("Category")+
    ylab("Price")+
    ggtitle("Brand-Philipp Plein") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          text = element_text(family = "Comic Sans MS"))
  pp2
  
  
  
  -------------#Liska products' in different categories with price------------------------------------------
  
  liska <- farfetch1[farfetch1$brand == "Liska",]
  liska
  
  
  liska1 <- ggplot(data=liska, aes(x=category, y=price))+
    geom_boxplot(aes(fill=category), outlier.color = NA)+
    coord_cartesian(ylim = c(0,10000))+
    xlab("Category")+
    ylab("Price")+
    ggtitle("Brand-Liska") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          text = element_text(family = "Comic Sans MS"))
  liska1
  
  -------------#Thom Browne products' in different categories with price---------------------------
  
  tb <- farfetch1[farfetch1$brand == "Thom Browne",]
  tb
  tb1  <- ggplot(data=tb, aes(x=category, y=price))+
    geom_boxplot(aes(fill=category), outlier.color = NA)+
    coord_cartesian(ylim = c(0,5000))+
    xlab("Category")+
    ylab("Price")+
    ggtitle("Brand-Thom Browne") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  
  tb1
  
  
  -------# marchesa products' in different  categories with price--------------
  
  marchesa<- farfetch1[farfetch1$brand == "Marchesa",]
  marchesa
  marchesa1  <- ggplot(data=marchesa, aes(x=category, y=price))+
    geom_boxplot(aes(fill=category), outlier.color = NA)+
    coord_cartesian(ylim = c(0,10000))+
    xlab("Category")+
    ylab("Price")+
    ggtitle("Brand-Marchesa") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  
  marchesa1
  
  ----------------#Cara Mila products' in different  categories with price----------------------------
  
  caramila<- farfetch1[farfetch1$brand == "Cara Mila",]
  caramila
  caramila1  <- ggplot(data=marchesa, aes(x=category, y=price))+
    geom_boxplot(aes(fill=category), outlier.color = NA)+
    coord_cartesian(ylim = c(0,10000))+
    xlab("Category")+
    ylab("Price")+
    ggtitle("Brand-Cara Mila") +
    theme(axis.title.x = element_text(color="Black", size=30),
          axis.title.y = element_text(color="Black", size=30),
          axis.text.x = element_text(color="BLack", size=10),
          axis.text.y = element_text(color="Black", size=15),
          plot.title = element_text(size = 40),
          legend.title = element_text(size=20),
          legend.text = element_text(size=20),
          text = element_text(family = "Comic Sans MS"))
  
  caramila1
  
  
  -------------#boxplot graph: Top 10 the most expensive brands in clothing with price--------------------------------
  
  
  filt2 <- (category_clothing$brand =="Giambattista Valli")|(category_clothing$brand =="Rubin Singer")|(category_clothing$brand =="Philipp Plein")|(category_clothing$brand =="Liska")|(category_clothing$brand =="Thom Browne")|(category_clothing$brand =="Marchesa")|(category_clothing$brand =="Zuhair Murad")|(category_clothing$brand =="Marni")|(category_clothing$brand =="Cara Mila")|(category_clothing$brand =="Dolce & Gabbana")
  
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
  
  -------------# Fiorucci products' in different categories with price----------------------------------------------------
  
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
  
  --------------#Reebok products' in different categories with price---------------------------------------------------
  
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
  
  
  -------------------------2_bag_category----------------------
    
    
    max(category_bag$price)  ------------#15900
    min(category_bag$price)  ------------#18
    
    expensive_bag <- category_bag[category_bag$price > 5900 & category_bag$price <= 15900 ,]
  expensive_bag 
  nrow(expensive_bag)
  
  library(ggplot2)
  
  -----------------#boxplot graph: the most expensive bags with its brand----------------------------
  
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
  
  
  ------------------3_shoes_category------------------
    
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
  
  
  -----------#Sebastian Tarek's products' in different categories with price -----------------
  
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
  
  ------------------#4_male_gender---------------------------
  
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
  
  
  
  ----------------#4.1_male_clothings------------------
  
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
  
  
  
  
  
  ----------------#4.2_male_bags------------------
  
  
  
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
  
  
  ----------------#4.3_male_shoes------------------
  
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
  
  
  
  
  ------------------#5_female_gender---------------------------
  
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
  
  
  ----------------#5.1_female_clothings------------------
  
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
  
  
  ---------------------#5.2_female_bags--------------------------------------
  
  
  
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
  
  
  
  ------------------#5.3_female_shoes------------------------------------------
  
  
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
          text = element_text(family = "Comic Sans MS"))
  v14
  
  
  
  
  