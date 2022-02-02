#SPSS'ten veri almak
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(corrplot)
library(funModeling)
library(rstatix)
library(biotools)
library(MASS)
library(klaR)
library(ggplot2)

#Linear Discriminant Analysis - Multi-class
#SPSS'ten veri almak
pizza_marka <- read_excel("Pizza_data.xlsx")
pizza_marka<-as.data.frame(pizza_marka)
summary(pizza_marka)
pizza_marka$brand<-factor(pizza_marka$brand)
pizza_marka$fat_cat[pizza_marka$fat < 15.65] <- "DUSUK"
pizza_marka$fat_cat[pizza_marka$fat >= 15.65 & pizza_marka$fat < 20.3]<- "ORTA"
pizza_marka$fat_cat[pizza_marka$fat >= 20.3]<- "YUKSEK"
pizza_marka$fat_cat<-factor(pizza_marka$fat_cat)
summary(pizza_marka)

#Homogeneity of Covariances

boxM(pizza_marka[,2:5], pizza_marka$fat_cat)

#LDA
pizza_lda<-lda(fat_cat~., data=pizza_marka) 
pizza_lda
#Y1=-0.05mpg +0.02engine -0.02horse +0.0002weight
#Y2=-0.13mpg -0.01engine -0.01horse +0.002weight

#Grup tahmini yapilmasi
pizzapred<-predict(pizza_lda)
#carpred$x #diskriminant skorlarý
#carpred$class #Sinif Tahminleri 

#plots
ldahist(pizzapred$x[,1], g = pizza_marka$fat_cat) #1.disc. fonsiyonu icin
ldahist(pizzapred$x[,2], g = pizza_marka$fat_cat) #2.disc.fonksiyonu icin

#Çapraz Tablonun olusturulmasi
tablo_car<-table(pizza_marka$fat_cat,pizzapred$class)
tablo_car

#Dogru siniflanma orani
classrate_pizza<-sum(diag(tablo_car))/sum(tablo_car)
classrate_pizza

#Nispi sans kriteri p1^2+p^2
as.vector(pizza_lda$prior[1]^2 + pizza_lda$prior[2]^2 + pizza_lda$prior[3]^2)

#Orjinal gruplar ile Tahmin edilen gruplarýn karþýlaþtýrýlmasý
comp_pizza<-cbind(pizza_marka$fat_cat,pizzapred$class)

#3lü çizim
pizza_marka %>%
  mutate(LD1 = predict(pizza_lda)$x[, 1],
         LD2 = predict(pizza_lda)$x[, 2]) %>%
  ggplot(aes(LD1, LD2, col = fat_cat)) +
  geom_point()



