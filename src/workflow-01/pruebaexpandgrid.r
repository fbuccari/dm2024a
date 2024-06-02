require("data.table")
require("yaml")
#install.packages("randomForest")
#install.packages("proxy")  # Para calcular las distancias a partir de las proximidades
library(randomForest)
library(proxy)

dataset <- fread("~/buckets/b1/datasets/dataset_pequeno.csv")

