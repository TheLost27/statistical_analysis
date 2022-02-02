###MANOVA 1. Uygulama - brand fat_cat Veri Kümesi
install.packages(c("tidyverse","ggpubr","rstatix","car","broom","dplyr","tidyr","haven"))
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)


pizza_marka <- read_excel("Pizza_data.xlsx")
pizza_marka<-as.data.frame(pizza_marka)
summary(pizza_marka)
pizza_marka$fat_cat[pizza_marka$fat < 15.65] <- "DUSUK"
pizza_marka$fat_cat[pizza_marka$fat >= 15.65 & pizza_marka$fat < 20.3]<- "ORTA"
pizza_marka$fat_cat[pizza_marka$fat >= 20.3]<- "YUKSEK"
pizza_marka$brand<-factor(pizza_marka$brand)
pizza_marka$fat_cat<-factor(pizza_marka$fat_cat)
summary(pizza_marka)

#Grup gozlem dagilimi
pizza_marka %>%
  group_by(brand) %>%
  dplyr::summarise(N = n())

#Degiskenlere Göre Ortalamalar ve Ortalama Çizimleri
#brand duzeyleri bazýnda degisken ortalamalarý ve sapmalar
pizza_marka %>% 
  group_by(brand) %>%
  summarise(across(-fat_cat, list(mean=mean,sd=sd)))

#fat_cat duzeyleri bazýnda degisken ortalamalarý
pizza_marka %>% 
  group_by(fat_cat) %>%
  summarise(across(-brand, list(mean=mean,sd=sd)))

library(gplots)
#brand icin :
plotmeans(sodium~brand,xlab="brand",ylab="tatmin", main="Mean Plot\nwith 95% CI",data=pizza_marka)
plotmeans(carb~brand, xlab="brand",ylab="sahiplenme", main="Mean Plot\nwith 95% CI",data=pizza_marka)
plotmeans(cal~brand, xlab="brand",ylab="kalicilik", main="Mean Plot\nwith 95% CI",data=pizza_marka)

#fat_cat icin :
plotmeans(sodium~fat_cat,xlab="fat_cat",ylab="tatmin", main="Mean Plot\nwith 95% CI",data=pizza_marka)
plotmeans(carb~fat_cat, xlab="fat_cat",ylab="sahiplenme", main="Mean Plot\nwith 95% CI",data=pizza_marka)
plotmeans(cal~fat_cat, xlab="fat_cat",ylab="kalicilik", main="Mean Plot\nwith 95% CI",data=pizza_marka)

#Multivariate Normality
library(dplyr)
pizza_marka %>%
  dplyr::select(sodium,carb,cal) %>%
  mshapiro_test()

#Homogeneity of Covariances
#install.packages("biotools")
library(biotools)
box_m(pizza_marka[, c("sodium","carb","cal")], pizza_marka$brand)

#MANOVA  
brand_dep_manova <- manova(cbind(sodium,carb,cal) ~ brand,data=pizza_marka)
summary(brand_dep_manova, test = "Hotelling-Lawley")
summary(brand_dep_manova, test = "Wilks")
summary(brand_dep_manova, test = "Pillai")
summary(brand_dep_manova, test = "Roy")

#Tek tek bu þekilde levene testleri elde edilebilir
#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
leveneTest(sodium ~ brand, data=pizza_marka,center=mean)
leveneTest(carb ~ brand, data=pizza_marka,center=mean)
leveneTest(cal ~ brand, data=pizza_marka,center=mean)

#ya da bu sekilde toplulastirilmis bicimde daha pratik olarak elde edilebilir:
#Homogeneity of variance- Levene's Test
pizza_marka %>% 
  pivot_longer( c(sodium,carb,cal),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ brand,center=mean)

### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(brand_dep_manova)

#Levene sonuclarina gore: Kalýcýlýk icin Tukey, Tatmin ve Sahiplenme icin Games Howell’a bakýlmalýdýr

# Çoklu Karsilastirmalar (Multiple Comparisons)
#Levene- Equal variances -Tukey
#kalicilik için (tek degisken oldugu icin bu sekilde bakildi)
cal_aov <- aov(cal ~ brand, data = pizza_marka)
TukeyHSD(cal_aov, "brand")

##örnek göstermek icin eklendi-birden cok degisken icin bakilacaginde bu sekilde toplulastirilabilir:
# Çoklu Karsilastirmalar (Multiple Comparisons)
#Levene- Equal variances -Tukey
m_tukey2 <- pizza_marka %>%
  pivot_longer( c(cal),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ brand)
m_tukey2<-m_tukey2[,c(1,2,4,3,5:9)]
m_tukey2

#Levene- Not equal variances  - Games Howell
#Tatmin ve sahiplenme icin:
m_gh_3 <- pizza_marka %>%
  pivot_longer( c(sodium,carb),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  games_howell_test(value ~ brand,) 
m_gh_3<-m_gh_3[,c(1,2,4,3,5:9)]
m_gh_3


##örnek göstermek icin eklendi-Tek degisken icin bakýlmak istenirse bu sekilde yazilabilir
#Levene- Not equal variances  - Games Howell
gh_tek<-games_howell_test(pizza_marka,sodium ~ brand)
gh_tek<-gh_tek[,c(1,3,2,4:8)]
gh_tek


###Çift Yönlü Manova

library(heplots)
pizza_marka$fat_cat_num[pizza_marka$fat_cat == "DUSUK"]<- 0
pizza_marka$fat_cat_num[pizza_marka$fat_cat == "ORTA"]<- 1
pizza_marka$fat_cat_num[pizza_marka$fat_cat == "YUKSEK"]<- 2
boxM( cbind(sodium,carb,cal) ~ brand*fat_cat_num, data=pizza_marka)

#MANOVA  
brand_cift <- manova(cbind(sodium,carb,cal) ~ brand*fat_cat,data=pizza_marka)
#Sadece wilks alindi ornek icin, uygun olan secilebilir
summary(brand_cift, test = "Wilks")

#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
pizza_marka %>% 
  pivot_longer( c(sodium,carb,cal),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ brand*fat_cat,center=mean)

### Test of Between Subjects####Farklilik yaratan degisken hangisi ?
summary.aov(brand_cift)

# Çoklu Karsilastirmalar (Multiple Comparisons)
#fat_cat icin
m_tukey22 <- pizza_marka %>%
  pivot_longer( c(sodium,carb,cal),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ fat_cat*brand)
m_tukey22<-m_tukey22[,c(1,2,4,3,5:9)]
fat_cat_etk<-filter(m_tukey22, term=="fat_cat")
fat_cat_etk

#brand ve fat_cat için Etkilesim Grafikleri (Interaction Plots) 
attach(pizza_marka)
interaction.plot(brand,fat_cat,sodium, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(brand,fat_cat,carb, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
interaction.plot(brand,fat_cat,cal, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
detach(pizza_marka)


###MANOVA 2. Uygulama - Manova_Toprak Veri Kümesi 
library(haven)
Manova_toprak <- read_sav("Manova_toprak.sav")
Manova_toprak<- as.data.frame(Manova_toprak)
Manova_toprak$ilce<-factor(Manova_toprak$ilce,levels = c(1,2,3,4),labels=c("Akyazý","Kaynarca","Hendek","Karasu"))
Manova_toprak$Bitkituru<-factor(Manova_toprak$Bitkituru,levels = c(1,2,3),labels=c("Fýndýk","Mýsýr","Buðday"))
summary(Manova_toprak)

#Grup gozlem dagilimi
Manova_toprak %>%
  group_by(ilce) %>%
  summarise(N = n())

#Multivariate Normality
library(dplyr)
pizza_marka %>%
  dplyr::select(ash,sodium,carb,cal) %>%
  mshapiro_test()

#Normallik
library(funModeling)
plot_num(pizza_marka)

#Univariate Normality
#Her bir degisken için
shapiro.test(Manova_toprak$Potasyum)
shapiro.test(Manova_toprak$Fosfor)
shapiro.test(Manova_toprak$Kalsiyum)
shapiro.test(Manova_toprak$Magnezyum)

#donusum yapilirsa:
Manova_toprak$lnpotasyum<-log(Manova_toprak$Potasyum)
Manova_toprak$lnfosfor<-log(Manova_toprak$Fosfor)
Manova_toprak$lnkalsiyum<-log(Manova_toprak$Kalsiyum)
Manova_toprak$lnmagnezyum<-log(Manova_toprak$Magnezyum)

#Normallik
library(funModeling)
plot_num(Manova_toprak[,7:10])
#Univariate Normality
shapiro.test(Manova_toprak$lnpotasyum)
shapiro.test(Manova_toprak$lnfosfor)
shapiro.test(Manova_toprak$lnkalsiyum)
shapiro.test(Manova_toprak$lnmagnezyum)

#Lnkalsiyum ve lnmagnezyum hariç, normallik saðlanmaktadýr.

#Multivariate Normality
Manova_toprak %>%
  dplyr::select(lnpotasyum,lnfosfor,lnkalsiyum,lnmagnezyum) %>%
  mshapiro_test()

#cýkarilirsa saglaniyor- lnmagnezyum cikarilmisti:
Manova_toprak %>%
  dplyr::select(lnpotasyum,lnfosfor,lnkalsiyum) %>%
  mshapiro_test()

#Degiskenlere Göre Ortalamalar ve Ortalama Çizimleri
#brand duzeyleri bazýnda degisken ortalamalarý ve sapmalar
Manova_toprak %>% 
  group_by(ilce) %>%
  summarise(across(c(lnpotasyum,lnfosfor,lnkalsiyum), list(mean=mean,sd=sd)))

#fat_cat duzeyleri bazýnda degisken ortalamalarý
Manova_toprak %>% 
  group_by(Bitkituru) %>%
  summarise(across(c(lnpotasyum,lnfosfor,lnkalsiyum), list(mean=mean,sd=sd)))

library(gplots)
#ilce icin:
plotmeans(lnpotasyum~ilce,xlab="ilce",ylab="lnpotasyum", main="Mean Plot\nwith 95% CI",data=Manova_toprak)
plotmeans(lnfosfor~ilce, xlab="ilce",ylab="lnfosfor", main="Mean Plot\nwith 95% CI",data=Manova_toprak)
plotmeans(lnkalsiyum~ilce, xlab="ilce",ylab="lnkalsiyum", main="Mean Plot\nwith 95% CI",data=Manova_toprak)

#bitki turu icin:
plotmeans(lnpotasyum~Bitkituru,xlab="bitkituru",ylab="lnpotasyum", main="Mean Plot\nwith 95% CI",data=Manova_toprak)
plotmeans(lnfosfor~Bitkituru, xlab="bitkituru",ylab="lnfosfor", main="Mean Plot\nwith 95% CI",data=Manova_toprak)
plotmeans(lnkalsiyum~Bitkituru, xlab="bitkituru",ylab="lnkalsiyum", main="Mean Plot\nwith 95% CI",data=Manova_toprak)

#sacýlým matrisi
lnli<-Manova_toprak[,c(7:9)]
library(PerformanceAnalytics)
chart.Correlation(Manova_toprak[,7:9], histogram=TRUE, pch=19)#method="kendall"

#Outliers-Univariate 
#Örneðin lnpotasyum deðiþkeni için ilçelere göre inceleme yapýlýrsa:
library(dplyr)
out<- Manova_toprak %>% tibble::rownames_to_column(var="outlier") %>% group_by(ilce) %>% mutate(is_outlier=ifelse(is_outlier(lnpotasyum), lnpotasyum, as.numeric(NA)))
out$outlier[which(is.na(out$is_outlier))] <- as.numeric(NA)

ggplot(out, aes(y=lnpotasyum, x=ilce,fill=ilce))+
  geom_boxplot() + 
  geom_text(aes(label=outlier),na.rm=TRUE,nudge_x=0.15,size=3.5)+
  labs(x="Ýlçeler", y = "lnpotasyum")+
  scale_fill_discrete(name = "Ýlçeler")

#Outliers-Multivariate
#Manova_toprak$id=1:nrow(Manova_toprak)
Manova_toprak[,c(5,7:9)] %>%
  group_by(ilce) %>%
  mahalanobis_distance() %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

#Homogeneity of Covariances
#install.packages("biotools")
library("biotools")
box_m(Manova_toprak[, c("lnpotasyum","lnfosfor","lnkalsiyum")], Manova_toprak$ilce)

#MANOVA  
lntoprak_man <- manova(cbind(lnpotasyum,lnfosfor,lnkalsiyum) ~ ilce,data=Manova_toprak)
summary(lntoprak_man, test = "Hotelling-Lawley")
summary(lntoprak_man, test = "Wilks")
summary(lntoprak_man, test = "Pillai")
summary(lntoprak_man, test = "Roy")

#Tek tek bu þekilde levene testleri elde edilebilir:
#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
leveneTest(lnpotasyum ~ ilce, data=Manova_toprak,center=mean)
leveneTest(lnfosfor ~ ilce, data=Manova_toprak,center=mean)
leveneTest(lnkalsiyum ~ ilce, data=Manova_toprak,center=mean)

#ya da bu sekilde toplulastýrýlmýs bicimde daha pratik olarak elde edilebilir:
#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
library(tidyverse)
Manova_toprak %>% 
  pivot_longer( c(lnpotasyum,lnfosfor,lnkalsiyum),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ ilce,center=mean)

### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(lntoprak_man)

# Çoklu Karsilastirmalar (Multiple Comparisons)
#bagimli degiskenlerin levene sonucuna gore hepsi icin Tukey bakilacak:
#Levene- Equal variances -Tukey
m<-Manova_toprak[,c(5,7:9)]
pc <- m %>%
  pivot_longer( c(lnpotasyum,lnfosfor,lnkalsiyum),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ ilce)
pc<-pc[,c(1,2,4,3,5:9)]
pc

#ya da bu sekilde tek tek bakýlabilir
#lnpotasyum için ornegin:
lnpot_aov <- aov(lnpotasyum ~ ilce, data = Manova_toprak)
TukeyHSD(lnpot_aov, "ilce")


###Çift Yönlü Manova
library(heplots)
boxM( cbind(lnpotasyum,lnfosfor,lnkalsiyum) ~ ilce*Bitkituru, data=Manova_toprak)

#MANOVA  
lntoprak_cift <- manova(cbind(lnpotasyum,lnfosfor,lnkalsiyum) ~ Bitkituru*ilce,data=Manova_toprak)
summary(lntoprak_cift, test = "Pillai") #uygun olanlardan biri secilebilir

#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
Manova_toprak %>% 
  pivot_longer( c(lnpotasyum,lnfosfor,lnkalsiyum),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ ilce*Bitkituru,center=mean)


### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(lntoprak_cift)

# Çoklu Karsilastirmalar (Multiple Comparisons)
#Bitkituru icin- Levene- Equal variances -Tukey
m2<-pizza_marka[,c(5:9)]
pc_2 <- pizza_marka %>%
  pivot_longer( c(sodium,carb,cal),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ brand*brand)
pc_2<-pc_2[,c(1,2,4,3,5:9)]
pizza_etk<-filter(pc_2, term=="pizza_marka")
pizza_etk


#Bitki türü  ve ilce için Etkilesim Grafikleri (Interaction Plots) 
attach(Manova_toprak)
interaction.plot(ilce,Bitkituru,lnpotasyum, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(ilce,Bitkituru,lnkalsiyum, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
detach(Manova_toprak)


