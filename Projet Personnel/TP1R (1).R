
#EXO19

data <- data.frame(BEPC = c(15,10,15,40), BAC = c(12,18,5,35), Licence
                   = c(3,4,8,15), Total = c(30,32,28,90))
rownames(data) <- c("Plus de 50 ans", "Entre 30 et 50 ans", "Moins de 30
ans", "Total")

data

freq <- data/sum(data)
round(freq*100,digit=2)
r <- apply(freq,1,sum)
round(r,digit=2)

c <-  apply(freq ,2,sum)
round(c,digit=2)


L <- sweep(freq,1,STAT=r,FUN="/")
round(L,digits=2)


C <- sweep(freq,1,STAT=c,FUN="/")
round(C,digits=2)


chisq.test(data)



#exo 20

tableau <- matrix(c(290,410,110,190), ncol=2, byrow=TRUE)
colnames(tableau) <- c("Bleu","Brun")
rownames(tableau) <- c("Celib","Marie")
tableau
tableau <- as.table(tableau)

barplot(tableau)
n <- margin.table(tableau) 
m1 <- margin.table(tableau,1)
m2 <- margin.table(tableau,2)
prop.table(tableau) 
tab0 <- as.array(m1) %*% t(as.array(m2))/n
tab0 <- as.table(tab0)
summary(tableau)
summary(tab0)


HairEyeColor
HairEyeNew<- margin.table(HairEyeColor, margin = c(1,2))
chisq.test(HairEyeNew)

HairEyeNew/sum(HairEyeNew)


#Exo21

data()
data(cars)
cars  
names(cars)
dim(cars) 
plot(cars) 
?lm
reg <- lm(dist ~ speed, data = cars) 
attributes(reg)
summary(reg)
anova(reg)
names(reg)
plot(reg)
plot(cars,pch=20,col='blue')
abline(reg=reg,col="red")
abline(reg$coeff,col="yellow")
hat<-predict(reg)
hat
predict(reg , data.frame(speed = 20) , interval = "prediction")

