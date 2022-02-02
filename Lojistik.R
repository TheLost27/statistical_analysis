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

#korelasyon

korelasyon<-corrplot(cor(pizza_marka[,c(-1,-9)]), method="number") #type="upper" eklenebilir
invkor<-inv(korelasyon$corr)# korelasyon matrisinin tersi (VIF)
invkor
pizza_n<-pizza_marka[,-5]#faceheight çýkarýldý

view(pizza_n)
#Logistic Regresyon 
model_pizza <- glm(fat_cat ~mois+prot+ash+cal, family = "binomial", data = pizza_n)
summary(model_pizza)

### SPSS OBNÝMUS TESTS OF MODEL COEFFÝCÝENTS TABLOSU  ###

#Ki-kare istatistginin hesabi
model_pizza$deviance
model_pizza$null.deviance
kikare<- model_pizza$null.deviance-model_pizza$deviance
kikare

#serbestlik derecesi hesabi
model_pizza$df.null
model_pizza$df.residual
df<-model_pizza$df.null-model_pizza$df.residual
df

#Ki kare istatistigine ait p degerinin hesabi (p<0.05 ise eklenen degiskenlerin modele katkisi anlamlidir.)
kikare.p<- 1 - pchisq(kikare,df)
kikare.p

###Hoshmer Lemeshov hesabi (p>0.05 ise model anlamlýdýr. yani model veriye uyumludur.)


hoslem.test(model_pizza$y,fitted(model_pizza))

#Modelin R^2 degerlerinin hesabi 

PseudoR2(model_pizza, which = c("CoxSnell","Nagelkerke"))

#formülasyonla hesaplanmasý (ödevde çalýþtýrmanýa gerek yok)
N<-nrow(pizza_n)

#Cox and Snell R^2
R.cs <- 1 - exp ((model_pizza$deviance - model_pizza$null.deviance) /N)
R.cs

#Nagelkerke R^2
R.n<-R.cs /(1-(exp(-(model_pizza$null.deviance/N))))
R.n

#Model katsayilarinin exponential alinmis hali ve güven araliklari
exp(coef(model_pizza))
exp(confint.default(model_pizza,level = 0.95)) 

#Atama Tablosu
type_pred<-fitted(model_pizza)
typefac<- ifelse(type_pred>0.5,"B","A")
t_tab <- table(pizza_marka$fat_cat, typefac)
t_tab

#Toplam Dogru Atanma Yüzdesi
sum(diag(t_tab)) / sum(t_tab)

#Atama Tablosu Görselleþtirme

datatib <- tibble("target" = pizza_marka$fat_cat,"prediction" = typefac)
datatib
basic_table <- table(datatib)
basic_table
cfm <- tidy(basic_table)
cfm

plot_confusion_matrix(cfm, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_normalized = FALSE,
                      add_row_percentages = FALSE,
                      font_col_percentages= font(size = 6),
                      font_counts = font(size = 6),
                      tile_border_color = "black")+
  xlab("Gerçek")+
  ylab("Tahmin")+
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))


#Stepwise Yöntemler ile Lojistik Regresyon


step_model<-step(model_pizza,direction="backward")
step_model
summary(step_model)
exp(step_model$coefficients)
exp(confint.default(step_model,level = 0.95)) 


#Atama Tablosu
type_pred<-fitted(step_model)
typefac<- ifelse(type_pred>0.5,"B","A")
t_tab <- table(pizza_marka$fat_cat, typefac)
t_tab

#Toplam Dogru Atanma Yüzdesi
sum(diag(t_tab)) / sum(t_tab)

#Atama Tablosu Görselleþtirme

datatib <- tibble("target" = pizza_marka$fat_cat,"prediction" = typefac)
datatib
basic_table <- table(datatib)
basic_table
cfm <- tidy(basic_table)
cfm

plot_confusion_matrix(cfm, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_normalized = FALSE,
                      add_row_percentages = FALSE,
                      font_col_percentages= font(size = 1),
                      font_counts = font(size = 1),
                      tile_border_color = "black")+
  xlab("Gerçek")+
  ylab("Tahmin")+
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))


###Uygulama-2: Multinomial Logistic Regression - Cars data uygulamasi
pizza_marka <- read_excel("Pizza_data.xlsx")
pizza_marka <- as.data.frame(pizza_marka)
summary(pizza_marka)
pizza_marka$fat_cat[pizza_marka$fat < 15.65] <- "DUSUK"
pizza_marka$fat_cat[pizza_marka$fat >= 15.65 & pizza_marka$fat < 20.3]<- "ORTA"
pizza_marka$fat_cat[pizza_marka$fat >= 20.3]<- "YUKSEK"
pizza_marka$brand<-factor(pizza_marka$brand)
pizza_marka$fat_cat<-factor(pizza_marka$fat_cat)
summary(pizza_marka)
# ya da bu sekilde referans kategori tanimlanabilir
#cars$origin=as.factor(cars$origin)
#cars$origin<- relevel(cars$origin, ref =3) #karakter olarak tanýmli ise "Japanese" yazilabilirdi

###  multinominal Lojistik regresyon

ml.dat <- mlogit.data(pizza_marka, choice="fat_cat", shape="wide")
ml <- mlogit(fat_cat ~  0|mois+prot+ash+cal , data=ml.dat, reflevel = 'DUSUK')
summary(ml)

exp(ml$coefficients) #Model katsayilarinin exponential alinmis hali 

##R^2 Deðerleri

multi_mo <- multinom(fat_cat ~ mois+prot+ash+cal , data = pizza_marka)
PseudoR2(multi_mo, which = c("CoxSnell","Nagelkerke","McFadden"))

#Sýnýflandýrma Tablosu

tahminkat<-colnames(ml$probabilities)[max.col(ml$probabilities)]
tahminkat<-as.factor(tahminkat)

pizza_tab<- table(pizza_marka$fat_cat, tahminkat)
pizza_tab

#Toplam doðru sýnýflama oraný
sum(diag(pizza_tab)) / sum(pizza_tab)


