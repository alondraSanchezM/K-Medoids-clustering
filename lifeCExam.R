#-------------------------------------------Alondra Sánchez Molina

#Ejercicio 1

library(cluster)
library(ggplot2)
library(factoextra)

dt_lifeCycleS<-read.csv("LifeCycleSavings.csv",header = TRUE, sep = ",")
View(dt_lifeCycleS)

#Preprocesamiento de datos 
sum(is.na(dt_lifeCycleS))  

rownames(dt_lifeCycleS) <- dt_lifeCycleS$Contry                             
dt_lifeCycleS <- dt_lifeCycleS[, -c(colnames(dt_lifeCycleS) %in% ("Contry"))]
View(dt_lifeCycleS)

#Se escalan los datos
data <- scale(dt_lifeCycleS)

#Calculo de K mas optimo
fviz_nbclust(data, pam, method = "silhouette")+ theme_classic()

pam_resul <- pam(data, 2)
print(pam_resul)

#Ploteado de clusters
fviz_cluster(pam_resul,
             palette = c("#D81159", "#8F2D56"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)

aggregate(dt_lifeCycleS, by=list(cluster=pam_resul$cluster), mean)

new_lifeCycleS <- cbind(dt_lifeCycleS, cluster = pam_resul$cluster)
new_lifeCycleS <- as.data.frame(new_lifeCycleS) 
new_lifeCycleS[new_lifeCycleS$cluster==1,]
new_lifeCycleS[new_lifeCycleS$cluster==2,]