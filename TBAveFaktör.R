### Temel Bilesenler Analizi (Principal Components Analysis) ###

#Veri seti Food-Price
#SPSS import:
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

#Excel import:
#library(readxl)
#food <- read_excel("food.xls")

#KORELASYON MATRÝSÝNÝN ÝNCELENMESÝ

library(GGally)

pizza_data<-pizza_marka[,2:6]   ## ya da food[,-1] de diyebilirdik.
ggpairs(pizza_data)#sacilim matrisi

library("Hmisc") #anlamlýlýk deðerleriyle birlikte görmek istersek (2-tailed sonuçlar)
rcorr(as.matrix(pizza_data),type="pearson") # Veri matris formatýnda olmalý

library(corrplot)
corrplot(cor(pizza_data)) #Bir baþka grafiksel gösterim

#PCA UYGULANABÝLÝRLÝGÝ (KMO - ANTÝ-IMAGE - BARTLETT TEST)
library(psych)
KMO(pizza_data) # KMO ve MSA Anti-image matris kosegenleri

#Bartlett Küresellik Testi(Bartlett's Test of Spherecity) 
cortest.bartlett(cor(pizza_data),nrow(pizza_data)) #Bartlett test 

#TEMEL BÝLESENLER ANALÝZÝ- princomp fonksiyonu
fit.pca <- prcomp(pizza_data, scale=TRUE) # korelasyon matrisi icin scale=TRUE yaz 
fit.pca$rotation ## yükler
fit.pca$x   #scores

#Bilesen sayisina karar vermek
summary(fit.pca) # varyans açýklama oranlarý 
(fit.pca$sdev)^2 #ozdegerler 1 den büyük olanlarý seç

#Scree plot
plot(fit.pca)
plot(fit.pca,type="line")
#ya da 
library(factoextra)
scree <- fviz_eig(fit.pca)
scree

#ilk iki bilesene karar verildi:
fit.pca$rotation[,1:2] #loadings

#Y1=0.49Bread +0.57Burger +0.33Milk +0.22Oranges +0.50Tomatoes
#Y2=-0.309Bread -0.04Burger -0.43Milk +0.79Oranges +0.28Tomatoes


faktor_yukleri<-t(fit.pca$rotation)*fit.pca$sdev # koklambda ile carpýlmýs hali bu da bizi faktore goturuyor
faktor_yukleri #asal bileþenler

row.names(fit.pca$x)<-pizza_marka$fat_cat #skorlarý isimlendirme 

#skorlarý veriye kaydetme
pizza_marka$comp1 =fit.pca$x[,1] 
pizza_marka$comp2 =fit.pca$x[,2] 

#indeks olusturma ### 
pizza_marka$index=pizza_marka$comp1+pizza_marka$comp2
indeks<-sort(pizza_marka$index, decreasing = T)
head(indeks)# Gözlem sayýsý çok olduðunda kullanýlablir.


library(factoextra)
fviz_pca_var(fit.pca,col.var="steelblue",
             repel = TRUE # Avoid text overlapping
)



### Faktor Analizi ### - Domes-factor veriseti

pizza_marka <- read_excel("Pizza_data.xlsx")
pizza_marka<-as.data.frame(pizza_marka)
summary(pizza_marka)
pizza_marka$fat_cat[pizza_marka$fat < 15.65] <- "DUSUK"
pizza_marka$fat_cat[pizza_marka$fat >= 15.65 & pizza_marka$fat < 20.3]<- "ORTA"
pizza_marka$fat_cat[pizza_marka$fat >= 20.3]<- "YUKSEK"
pizza_marka$brand<-factor(pizza_marka$brand)
pizza_marka$fat_cat<-factor(pizza_marka$fat_cat)
summary(pizza_marka)

pizza_factor=pizza_marka[,2:6]
pizza_factor=na.omit(pizza_factor) # kayýp gözlemler çýkarýldý

#KORELASYON MATRÝSÝNÝN ÝNCELENMESÝ
library(corrplot)
library(matlib)
corrplot(cor(pizza_factor))
korelasyon<-cor(pizza_factor)
korelasyon

library(Hmisc) #anlamlýlýk deðerleriyle birlikte görmek istersek (2-tailed sonuçlar)
rcorr(as.matrix(pizza_factor),type="pearson") # Veri matris formatýnda olmalý

invkor<- inv(korelasyon)# korelasyon matrisinin tersi (VIF)
colnames(invkor)<-rownames(invkor)<-colnames(korelasyon) # deðiþken isimleri yazmasý için
invkor

#Faktor Analizi Uygulanabilirligi (KMO - Anti-Image - Bartlett Test)
library(psych)
KMO(pizza_factor) # KMO ve MSA Anti-image matris kosegenleri

#Bartlett Küresellik Testi(Bartlett's Test of Spherecity) 
#install.packages("psych")
cortest.bartlett(cor(pizza_factor),nrow(pizza_factor)) #Bartlett test 


#Temel Bilesenler Analizi Yöntemi(Kok lambda yaklasimi) secilerek faktor analizi
fa_kokl<-principal(pizza_factor, nfactors =3, rotate = "none")
###fit.pca2 <- prcomp( ~., domes_factor, scale=TRUE) # koymasak olur 125.satýr scree plot için gerekli sadece
print(fa_kokl$loadings, digits=3, cutoff=.0, sort=TRUE) ## sýralý hale getirmek için

fa_kokl$communality  #tum degiskenlerin communality(h^2) hesaplamasý

fa_kokl$loadings[ , ]^2# aj^2 'ler tüm faktörler için (communality herbir deðiþken için yüklerin kareler toplamýdýr)
rowSums(fa_kokl$loadings[ , ]^2) #communality hesaplanýþý görülmesi için

### ncol(domes_factor) Deðiþken sayýsýný direk yazmak yerine kullanýlabilir 

var_oran<-colSums(fa_kokl$loadings[ , ]^2)/9 #varyans aciklama oranlari (deðiþken sayýsýna dikkat et!!!)
var_oran
sum(var_oran) # 3 Faktörün toplam varyans açýklama oraný

#scree
plot(fa_kokl$values, type="b", main="ScreePlot", xlab="Number of Factors", ylab="Eigenvalues")
#ya da 
library(factoextra) #109. satýra baðlý bir fonksiyon
scree <- fviz_eig(fit.pca2)
scree

artik_kor<-fa_kokl$residual ## Artýk korelasyon matrisi
artik_kor

#n_col_artik<-ncol(artik_kor)
#n_artik<-length(artik_kor)
#n_2artik<-n_artik-n_col_artik
length(artik_kor[abs(artik_kor)<0.05])/72 ## 0,05'den küçük çýkanlarýn oraný (72 sizin verinizde deðiþkenlik gösterecek !!!!! ipucu yukarýda)


#Varimax döndürme ile
fa_kokl<-principal(pizza_factor, nfactors =3, rotate = "varimax")
print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE)

#caused cýkarýlýyor
fa_kokl<-principal(pizza_factor[,-4], nfactors =3, rotate = "varimax")
print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE)

#F1=0.866protect +0.882save +0.829defend 
#F2=0.831mental +0.745insane +0.776stable
#F3=0.740provo +0.836passion

fa.diagram(fa_kokl)

?principal

#Faktor yuku grafigi
fyuk=fa_kokl$loadings[,1:3]#degiskenlerin faktor yukleri
library(scatterplot3d)
s3d=scatterplot3d(as.data.frame(unclass(fyuk)), main="3D factor loadings", color=1:ncol(pizza_factor[,-4]), pch=20,lwd=5, cex.axis =1,cex.lab = 1.5, font.lab = 2)
text(s3d$xyz.convert(fyuk), labels = rownames(fyuk),
     cex= 1, col = 1:ncol(domes_factor[,-4]))
#ya da 
factor.plot(fa_kokl, labels=rownames(fa_kokl$loadings))

#Eger 2 faktor secilseydi plot:
fyuk2=fa_kokl$loadings[,1:2]
factor.plot(fyuk2, cut=0.5, labels=rownames(fa_kokl$loadings),font=2, cex=0.8)


?factor.plot

#Principal Axis Yontemiyle Faktor Analizi( MLE, Minimum Residual vs de secilebilir )
library(psych)
?fa
fsolution <- fa(pizza_factor, nfactors = 2, rotate = "none", fm = "pa") 
print(fsolution$loadings, digits=2, cutoff=.2, sort=TRUE)

#passion cikartildi
frotate1 <- fa(pizza_factor[,-7], nfactors = 2, rotate = "none", fm = "pa",scores=TRUE) 
print(frotate1$loadings, digits=2, cutoff=.2, sort=TRUE)

frotate <- fa(pizza_factor[,-7], nfactors = 2, rotate = "varimax", fm = "pa",scores=TRUE) #oblimin ya da promax
print(frotate$loadings, digits=2, cutoff=.2, sort=TRUE)


fa.diagram(frotate)

factor.plot(frotate, labels=rownames(frotate$loadings))


