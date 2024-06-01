require("data.table")
require("yaml")
#install.packages("randomForest")
#install.packages("proxy")  # Para calcular las distancias a partir de las proximidades
library(randomForest)
library(proxy)

dataset <- fread("~/buckets/b1/datasets/dataset_pequeno.csv")


campos_buenos <- setdiff(
  colnames(dataset),
  c( "clase_ternaria")
)

col=colnames(dataset)
col=col[1:10]
dataset_rf=dataset[, ..col]
dataset_rf=dataset_rf[foto_mes=="202107"]
dataset_rf=dataset_rf[1:10000]

rf_model <- randomForest(x = dataset_rf, y = NULL, proximity = TRUE) 

proximity_matrix=rf_model$proximity
head(proximity_matrix)

prox=data.table(proximity_matrix)

# Convertir la matriz de proximidades en una matriz de distancias
distance_matrix <- 1 - proximity_matrix



hclust_model <- hclust(as.dist(distance_matrix))

rf.cluster = cutree(hclust_model, k=3)