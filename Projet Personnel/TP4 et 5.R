#TP4
#Exercice 27 

library("FactoMineR")
library("factoextra")

gsvd <- function(Z,r,c) {
  
  N=diag(r)
  
  M=diag(c)
  
  k <- qr(Z)$rank
  colnames<-colnames(Z)
  rownames<-rownames(Z)
  Z <- as.matrix(Z)
  Ztilde <- diag(sqrt(r)) %*% Z %*% diag(sqrt(c))
  e <- svd(Ztilde)
  U <-diag(1/sqrt(r))%*%e$u[,1 :k]
  V <-diag(1/sqrt(c))%*%e$v[,1 :k]
  d <- e$d[1 :k]
  rownames(U) <- rownames
  rownames(V) <- colnames 
  if (length(d)>1)
    colnames(U) <- colnames (V) <- paste("dim", 1 :k, sep = "")
  return(list(U=U,V=V,d=d))
}

#1)
load("post-198636-chiens.rda")
head(chiens)
class(chiens)

#2)
H <- subset(chiens,select=-fonction)
H

#3)
#a)

K <- tab.disjonctif(H)
K


Freq <- K/sum(K) 


r<-apply(Freq,1,sum) 
c<-apply(Freq,2,sum) 

Z<-(Freq-r%*%t(c))/r%*%t(c)

U<-gsvd(Z,r,c)$U
V<-gsvd(Z,r,c)$V
d<-gsvd(Z,r,c)$d

X <- sweep(U,2,STAT=d,FUN="*") 
Y <- sweep(V,2,STAT=d,FUN="*") 

#b)

#inertie totale
p <- ncol(H) 
m <- ncol(K) 
m/p-1

sum(d^2)

#c)
#nb dim
length(round(d^2,digit=3))
n<-27
min(n-1,m-p)

#d)
barplot(d^2/sum(d^2)*100,
        names.arg=1:length(d),
        xlab="dim",
        ylab="pourcentage d'inertie expliquée")

#e)

X <- data.frame(U[,1:3]%*%diag(d[1:3])) 
rownames(X) <- rownames(H)
colnames(X) <- paste("dim", 1:3, sep = "") 
round(X,digit=2)

Y <- data.frame(V[,1:3]%*%diag(d[1:3])) 
rownames(Y) <- colnames(K)
colnames(Y) <- paste("dim", 1:3, sep = "") 
round(Y,digit=2)

#f)


plot(X[,c(1,2)],pch=20) 
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
text(X[,c(1,2)],labels=rownames(X),pos=3)

#Plan factoriel des modalités
plot(Y[,c(1,2)],pch=17) 
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
text(Y[,c(1,2)],labels=rownames(Y),pos=3)

#g)
which(K[,1]==1) 
moy <- apply(X[which(K[,1]==1),],2,mean) 
moy*(1/d[1:3]) 
Y[1,] 

#h)
eta2 <- function(x, gpe) {
  moyennes <- tapply(x, gpe, mean)
  effectifs <- tapply(x, gpe, length)
  varinter <- (sum(effectifs * (moyennes - mean(x)) ^ 2)) 
  vartot <- (var(x) * (length(x) - 1))
  res <- varinter / vartot
  return(res)
}

eta2(X$dim1,chiens$taille)
eta2(X$dim2,chiens$taille)

#4)
#a)

res <- MCA(chiens, quali.sup = 7)
res

#b)
head(X)

head(res$ind$coord)[,1:3]
head(res$var$coord)[,1:3]
plot(res,choix="ind",invisible=c("var","quali.sup"))
plot(res,choix="ind",invisible="ind")

#c)
res$var$eta2[,1:2]
plot(res,choix="var",invisible=c("ind"))

#d)
chiensNA <- H
chiensNA[1,1] <-NA
chiensNA[2,2] <-NA

#e)
res2 <- MCA(chiensNA,graph=FALSE) 
plot(res2,choix="ind",invisible="ind")

#5)a)
N <- table(H[,1:2]) 
resca<-CA(N,ncp=2,graph=FALSE) 

#b)
resmca<-MCA(H[,1:2],graph=FALSE)

#c)
resca$eig
resmca$eig

#relation entre les valeurs propres des deux analyses
mu <- resca$eig[,1]
(1+sqrt(mu))/2
(1-sqrt(mu))/2


#TP5
#Exercice 29

fromage2 <- read.delim("~/Users/ProDesK/Desktop/post-198637-fromage2.txt", row.names=1)

fromage.cr <- scale(fromage2,center=T,scale=T)
d.fromage <- dist(fromage.cr)

#CAH
cah.ward <- hclust(d.fromage,method="ward.D2")
plot(cah.ward)
rect.hclust(cah.ward,k=4)
cahF <- cutree(cah.ward,k=4)
sort(groupes.cah)

#k-means
kmeansF <- kmeans(fromage.cr,centers=4,nstart=5)
kmeansF$cluster

#correspondance
table(cahF,kmeansF$cluster)

