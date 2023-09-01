#Exercice 13

poids <- read.table("C:/Users/ProDesK/Desktop/Poids_naissance.csv", header = TRUE)
View(poids)

poids <- transform(poids,LWT=LWT*0.4535923)
#Exercice 14

Mort.à = c(93, 53, 72, 68, 68, 53)
Années.de.carrière = c(66, 25, 48, 37, 31, 32)
Nombre.de.films = c(211, 58, 98, 140, 74, 81)
Prénom = c("Michel", "André", "Jean", "Louis", "Lino", "Jacques")
Nom = c("Galabru", "Raimbourg", "Gabin", "de Funès", "Ventura", "Villeret")
Date.du.décès = c("04-01-2016", "23-09-1970", "15-10-1976", "27-01-1983",
                  "22-10-1987", "28-01-2005")
acteurs = data.frame(Mort.à, Années.de.carrière, Nombre.de.films, Prénom,
                     Nom, Date.du.décès)

colnames(acteurs)[1] = "Age.du.décès"

acteurs$Prénom

acteurs[order(acteurs$Age.du.décès), ]

#Exercice 15
w <- read.table("C:/Users/ProDesK/Desktop/fromage.txt", header = TRUE)
attach(w)
X1
str(w)
summary(w) 
pairs(w)
ww = w[(X1 > 5.1) & (X3 < 1.77), ] 
str(ww) 
summary(ww) 
#Exercice 16
data(airquality)
?airquality
names(airquality) 
dim(airquality) 
summary(airquality)

boxplot(airquality$Ozone~airquality$Month) 

install.packages("dplyr")
library(dplyr)

saison <-case_when (
  airquality$Month==5 ~ "printemps",
  airquality$Month==6 |airquality$Month==7|airquality$Month==8 ~ "été",
  airquality$Month==9 ~ "automne",
  TRUE ~ "Autre"
  
)
install.packages("ggplot2")
library(ggplot2)

qplot(Temp, Ozone, data = airquality, colour = Month)


#Exercice 17
#simulation
X=rnorm(100,0,5)
head(X)

#graphique
i=c(1:100)
Y=1.7+2.1*i+X
plot(i,Y)
abline(a=1.7, b=2.1, col="red")


#Exercice 18

brun =c(68,15,5,20)
chatin=c(119,54,29,84)
roux=c(26,14,14,17)
blond=c(7,10,16,94)
couleur=data.frame(brun,chatin,roux,blond)
View(couleur)

freq <- couleur/sum(couleur)
round(freq*100,digit=2)

r <- apply(freq,1,sum)
round(r,digit=2)
c <-  apply(freq ,2,sum)
round(c,digit=2)

L <- sweep(freq,1,STAT=r,FUN="/")
round(L,digits=2)

C <- sweep(freq,1,STAT=c,FUN="/")
round(L,digits=2)
sum((L[1,]-L[2,])^2/c)
sum((L[1,]-L[4,])^2/c)
sum((L[1,]-c)^2/c)

T <- (freq-r%*%t(c))/(r%*%t(c))
round(T,digit=2)
#test de khi-deux
chisq.test(couleur)$statistic
