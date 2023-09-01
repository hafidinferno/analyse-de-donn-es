#Travail personnel couleur des cheveux et yeux :
library(factoextra)
library(FactoMineR)

dataTable<-read.csv("C:/Users/ProDesK/Desktop/yeux-cheveux-sexes data.csv")

str(dataTable)
res.famd <- FAMD(dataTable, graph = FALSE)
get_famd_var(res.famd)
fviz_screeplot(res.famd)
#Test d'independance entre les deux caracteres .
test<-chisq.test(dataTable$cheveux,dataTable$yeux)

#Analyse factorielle

res.mca <- MCA (dataTable, graph = FALSE)

print(res.mca)

fviz_mca_biplot(res.mca) 

#valeurs propres 
eig.val <- get_eigenvalue(res.mca)
#visualisation de chaque pourcentage de variance 
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))
#visualisation du biplot des individus et des variables
fviz_mca_biplot (res.mca, repel = TRUE, ggtheme = theme_minimal())


