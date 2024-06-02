#install.packages("httr")
#install.packages("jsonlite")

library(httr)
library(jsonlite)
# Cargar las librerías necesarias
library(httr)
library(jsonlite)

# Configura tu token de API de Kaggle
#kaggle_api_token <- fromJSON("./buckets/b1/kaggle.json")

# Información necesaria
competition <- "itba-data-mining-2024-a" # Reemplaza con el nombre de la competencia

message <- "Mi primer submit desde R"  # Mensaje del submit

# URL para hacer el submit
url <- sprintf("https://www.kaggle.com/api/v1/competitions/submissions/submit/file/%s", competition)

# Realizar el submit

cortes <- seq(from=9000L,to=13000L, by=500L)



# Verificar el estado de la respuesta
# if (response$status_code == 200) {
#   cat("Submit realizado con éxito!\n")
# } else {
#   cat("Error en el submit. Código de estado:", response$status_code, "\n")
#   print(content(response, "text"))
# }

seeds=c(349177,406513,411007,524669, 546523,570959,731761,804913,894869,927973)

for (corte in cortes)
{
  for (seed in seeds)
  {
  #file_path <- "ruta/al/archivo.csv"  # Reemplaza con la ruta a tu archivo

  nombre_raiz <- paste0(
    "ZZ-0002_01_059",
    "_s",
    seed,"_", corte
  )
  
  
    
  nom_submit <- paste0("expw_ZZ-0002_",
    nombre_raiz, ".csv"
  )
    
    l1 <- "#!/bin/bash \n"
    l2 <- "source ~/.venv/bin/activate  \n"
    l3 <- paste0( "kaggle competitions submit -c ", competition)
    l3 <- paste0( l3, " -f ", nom_submit )
    l3 <- paste0( l3,  " -m ",  "\"", carpeta_actual(),  " , ",  nom_submit , "\"",  "\n")
    l4 <- "deactivate \n"
    
    cat( paste0( l1, l2, l3, l4 ) , file = "subir.sh" )
    Sys.chmod( "subir.sh", mode = "744", use_umask = TRUE)
    
    system( "./subir.sh" )
  }
}


