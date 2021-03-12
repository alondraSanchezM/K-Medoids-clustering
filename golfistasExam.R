#-------------------------------------------Alondra Sánchez Molina 

#Ejercicio 2
library(cluster)
library(ggplot2)
library(factoextra)
library(clustertend)
library(dendextend)
library(corrplot)
library(NbClust)


dt_lgpa = read.csv("RFiles/lgpa.csv",header=TRUE,sep=",")
View(dt_lgpa)

#Preprocesamiento de datos
sum(is.na(dt_lgpa)) 

rownames(dt_lgpa) <- dt_lgpa$�..Name                             
dt_lgpa <- dt_lgpa[, -c(colnames(dt_lgpa) %in% ("�..Name"))]
dt_lgpa <- dt_lgpa[, -c(10)]  
View(dt_lgpa)

#Se escalan los datos
data <- scale(dt_lgpa)

#Calculo de K mas optimo
fviz_nbclust(data, clara, method = "silhouette")+ theme_classic()

# Uso de CLARA
clara.res <- clara(data, 2, samples = 50, pamLike = TRUE)
print(clara.res)


fviz_cluster(clara.res,
             palette = c("#D81159", "#FC4E07"), # color palette
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

aggregate(dt_lgpa, by=list(cluster=clara.res$cluster), mean)

new <- cbind(dt_lgpa, cluster = clara.res$cluster)
new <- as.data.frame(new) 
new[new$cluster==1,]
new[new$cluster==2,]