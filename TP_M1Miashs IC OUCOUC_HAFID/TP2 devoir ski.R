#Devoir TP2 

#Exercice 24 station du ski 

stations <- read.csv("C:/Users/ProDesK/Downloads/post-197353-stations.txt", sep="")

res.pca1 <- PCA(stations,quali.sup =1, graph = FALSE)

#valeurs propres 

valp<-- get_eigenvalue(res.pca1)

# La règle de Kaiser. Elle consiste à retenir les axes pour lesquels les valeurs propres sontsupérieures à 1 . Il est à noter qu'on peut aussi avoir des résultats d'ACP dont la somme des valeurs propres
#n'est pas égale à p . Dans ce cas, il faut adapter cette règle deKaiser et retenir les valeurs propres
#supérieures à la moyenne des valeurs propres, et non plus à 1.

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


