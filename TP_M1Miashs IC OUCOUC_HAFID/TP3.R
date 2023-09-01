#Exercice 31
#1)

data("USArrests")
head(USArrests)

class(USArrests)

#2)

head(princomp(USArrests,cor=TRUE)$scores)
head(prcomp(USArrests,scale=TRUE)$x)

#3)
#a)
Z <- scale(USArrests)

#b)

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
r <- rep(1/nrow(Z),nrow(Z)) 
c <- rep(1,ncol(Z)) 
U <- gsvd(Z,r,c)$U
d <- gsvd(Z,r,c)$d
Psi <- U %*% diag(d)
head(Psi)

#d)

install.packages(c("FactoMineR"))
library("FactoMineR")
head(PCA(USArrests,graph=FALSE)$ind$coord)


#Exercice 32

data <- data.frame(CAMP = c(239,1003,682,2594), HOTEL = c(155,1556,1944,1124),
                   LOCA = c(129,1821,967,2176), RESI = c(0,1521,1333,1038))
rownames(data) <- c("AGRI","CADR","INAC","OUVR")

#1)

chisq <- chisq.test (data)
chisq

#2) 
sansT=data

cont=prop.table(sansT)
round(cont,digit=2)

r1 <- apply(cont,1,sum)
round(r1, digit=2)

c1 <- apply(cont,2,sum)
round(c1, digit=2)

cont2<-cbind(cont,r1)
cont2<-rbind(cont2,c1)
cont2[5,5]=sum(c1)
colnames(cont2) <- c("CAMP","HOTEL","LOCA","RESI","TOTAL")
rownames(cont2) <- c("AGRI","CADR","INAC","OUVR","TOTAL")
round(cont2,digit=2)

L1 <- sweep(cont2[-5,],1,STAT=r1,FUN="/")
round(L1,digits=2)

C1 <- sweep(cont2[,-5],2,STAT=c1,FUN="/")
round(C1,digits=2)

#3) AFC

res.ca <- CA (data, graph = FALSE)
print(res.ca)

#Exercice 33

#1)
install.packages(c("ca"))

library(ca)
data(smoke)
smoke

#2)a)
F <- smoke/sum(smoke)
r<-apply(F,1,sum)
r

c<-apply(F,2,sum)
c

Z <- (F-r%*%t(c))/r%*%t(c)
Z

#b)
U<-gsvd(Z,r,c)$U
V<-gsvd(Z,r,c)$V
d<-gsvd(Z,r,c)$d

X <- sweep(U,2,STAT=d,FUN="*") 
X

Y <- sweep(V,2,STAT=d,FUN="*") 
Y

#c)

plot(X[,1:2],xlab="dim 1",ylab="dim 2",xlim=c(-0.4,0.4),ylim=c(-0.4,0.4),main="Premier plan factoriel")
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
text(X[,1:2],rownames(smoke),pos=4)
points(Y[,1:2],pch=2,col=2)
text(Y[,1:2],colnames(smoke),pos=4,col=2)

#d)

T <- sum(d^2) 
d[1:2]^2/T*100

sum(d[1:2]^2/T)*100

#3)

?CA
res <- CA(smoke,graph=FALSE)
res$eig

head(res$row$coord)
head(res$col$coord)

?plot.CA
plot(res)

#Exercice 34

#1)
library(ca)
data <- read.csv(file="writers.csv", header = TRUE,row.names = 1)
data

#2)
K <- data[1:15,]
chisq.test(K)

#3)
res <- CA(K)

res$eig[1:8,]

plot(res,axes=c(3,4))

#4)
res <- CA(data,row.sup=c(16,17),graph=FALSE)
plot(res,col.row.sup=3)

#5)

X <- rbind(res$row$coord[,1:4],res$row.sup$coord[,1:4])
D <- dist(X)
#CAH
tree <- hclust(D,method="ward.D2")

plot(tree,hang=-1)


cutree(tree,k=4)
