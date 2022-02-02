library(tidyverse) 
library(modelr)    
library(broom)     
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(corrplot)
library(matlib)
library(ResourceSelection)
library(DescTools)
library(cvms)
library(ggplot2)
library(MASS)
library(broom)
library(tibble)
library(mlogit)
library("DescTools")
library(nnet)
library(dendextend) 
library(factoextra)
###Uygulama-1: Binary Logistic Regression-Skulls Data

pizza_marka <- read_excel("Pizza_data.xlsx")
pizza_marka<-as.data.frame(pizza_marka)
summary(pizza_marka)
pizza_marka$fat_cat[pizza_marka$fat < 15.65] <- "DUSUK"
pizza_marka$fat_cat[pizza_marka$fat >= 15.65 & pizza_marka$fat < 20.3]<- "ORTA"
pizza_marka$fat_cat[pizza_marka$fat >= 20.3]<- "YUKSEK"
pizza_marka$brand<-factor(pizza_marka$brand)
pizza_marka$fat_cat<-factor(pizza_marka$fat_cat)
summary(pizza_marka)


#mydata <- scale(nutrient[,-1]) # gerektiginde degiskenleri standardize etmek için

# Korelasyon matrisinin incelenmesi
#library("Hmisc")
#rcorr(as.matrix(nutrient[,-1]),type="pearson") 

# Hiyerarsik Kümeleme
d <- dist(pizza_marka[,c(-1,-9)], method = "euclidean") # uzaklik matrisi
fit <- hclust(d, method="complete") # method= "single", "complete", "average", "ward.D", "centroid"
dend<-as.dendrogram(fit) # Dendogram çizimi
plot(dend)
plot(color_branches(dend, k=3))


#Canned sardines aykiri deger, cikarilirsa:
pizza2=pizza_marka[-25,]
d2 <- dist(pizza2, method = "euclidean")
fit2 <- hclust(d2, method="complete") 
dend2<-as.dendrogram(fit2)
labels(dend2) <- paste(as.character(pizza_marka$brand)[order.dendrogram(dend2)],"(",labels(dend2),")", sep = "") #Etiketleri eklemek için
plot(color_branches(dend2, k=3))
par(mar = c(10,2,1,1)) # etiketler sýðmasý için
plot(color_branches(dend2, k=3)) # tekrardan çizdirdiðimizde


## Düzenlemeler için (https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html)
geneldend<-dend2 %>%
  set("branches_lwd", 2) %>%
  set("branches_k_color", k = 3)%>%
  set("labels_cex", 1.2)%>%
  set("labels_colors",k=3)
plot(geneldend,font.axis = 2) #type="triangle"


###Dendogram farkli bir paket uzerinden de cizilebilir

res.hc <- hclust(d2,  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco") 


## ggplot2 ile dendogram
ggd1 <- as.ggdend(geneldend)
library(ggplot2)
ggplot(ggd1, horiz = F) 


### polar eksende dendrogram cizmek icin 
library(circlize)
geneldend%>%
  set("labels_colors",k=3) %>% 
  circlize_dendrogram(dend_track_height=0.7)



############     K-MEANS    #############
#Küme sayisinin Belirlenmesi için 
fviz_nbclust(pizza2[,c(-1,-9)], kmeans, method = "wss")


##initial <- read_sav("initial.sav")
set.seed(95739487) 
km.res <- kmeans(pizza2[,c(-1,-9)],3, iter.max=10, algorithm="Lloyd")### initial[,-1] baþlangýç kümelerini seçmek isterseniz
t(km.res$centers) #Cluster means = SPSS'teki Final Cluster Centers


#Ilk iki faktörün cizdirilmesi
library(cluster)
clusplot(pizza2[,c(-1,-9)], km.res$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


#ya da bu fonksiyonla Temel Bileþenlere Göre
fviz_cluster(km.res, data = pizza2[,c(-1,-9)], palette = "jco",
             ggtheme = theme_minimal())

pizza2$cluster<-km.res$cluster # Veriye gözlemin atandigi kumenin eklenmesi
pizza2$cluster<-as.factor(pizza2$cluster)


library(rstatix)
#Anova tablosu
cal_aov <- aov(cal ~ cluster, data = pizza2)
summary(cal_aov)
sodium_aov <- aov(sodium ~ cluster, data = pizza2)
summary(sodium_aov)
fat_aov <- aov(fat ~ cluster, data = pizza2)
summary(fat_aov)
carb_aov <- aov(carb ~ cluster, data = pizza2)
summary(carb_aov)
