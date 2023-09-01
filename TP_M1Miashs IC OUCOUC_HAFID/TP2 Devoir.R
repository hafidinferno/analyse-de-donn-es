#TP2 

#Exercice 23 

#Question 1 :

library("FactoMineR")
library("factoextra")

dataC<-data.frame(
  Indidvidus = c("Z1","Z2"),
  n1 = c(1.00 ,5.00),
  n2 = c(2.00 ,10.00), 
  n3 = c(3.00 ,8.00), 
  n4 = c(4.00 ,8.00), 
  n5 = c(9.00 ,12.00) 
)
dim(dataC)
#construire la matrice de correlation 
pairs(dataC[,2:6])
Matricecorl<-cor(dataC[,2:6])

#observons si c'est une matrice identitaire 

det(Matricecorl)

#faire l'APC

res.pca<-PCA(Matricecorl,scale.unit = TRUE,graph=TRUE)

#Interpretation : On voit que les individus n1 et n5 sont les individus qui contribuent le plus à droite de l'axe.

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#Question 2

#en utilisant primcomp et prcomp

X<-princomp(Matricecorl,scale=FALSE)

Y<-prcomp(Matricecorl,scale=FALSE)


#Exercice 24 station du ski 

stations <- read.csv("C:/Users/ProDesK/Downloads/post-197353-stations.txt", sep="")

res.pca1 <- PCA(stations,quali.sup =1, graph = FALSE)

#valeurs propres 

valp<-- get_eigenvalue(res.pca1)


#l'interprétation. 

fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50))

#interpréter les 2 axes.
var <- get_pca_var(res.pca1)
var

# Coordonnées
head(var$coord)

# Cos2 : qualité de répresentation
head(var$cos2)

# Contributions aux composantes principales
head(var$contrib)

fviz_pca_var(res.pca1, col.var = "blue")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
fviz_contrib(res.pca1, choice = "ind", axes = 1 :2)

ind <- get_pca_ind(res.pca1)
ind

# Coordonnées des individus
head(ind$coord)

# Qualité des individus
head(ind$cos2)

# Contributions des individus
head(ind$contrib)

fviz_pca_ind (res.pca1)
fviz_pca_ind (res.pca1, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE )








