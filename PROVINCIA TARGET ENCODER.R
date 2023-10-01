
# library("readxl")
# datos <- read_excel("d:/kat/TRATA_limpia.xlsx")
setwd("C:/Users/Xuele/Documents/MACHINE LEARNING/TEMA 6")
load("TRATACodificada.rda")

TRATAPROV<-TRATA
listconti<-c("dias_transcurridos")							
listclass<-c("situacion", "es_anonima", "provincia", "via_ingreso", "derivacion_institucion", 							
             "derivacion_judicializa", "denunciante_tipo", "denunciante_edad_aparente", 							
             "MESES", "weekday_ingreso", "localidad", "denunciante_genero1", "hora_categ","nro_registro_interno")							
vardep<-c("subtema1")							


dput(names(TRATAPROV))


c("nro_registro_interno", "situacion", "es_anonima", "provincia", 
  "via_ingreso", "derivacion_institucion", "derivacion_judicializa", 
  "denunciante_tipo", "denunciante_edad_aparente", "subtema1", 
  "MESES", "weekday_ingreso", "localidad", "denunciante_genero1", 
  "dias_transcurridos", "hora_categ")

data22<-TRATAPROV[,c(listconti,listclass,vardep)]

data22

# PROBAMOS CON PROVINCIA, LAS DEMÁS SE HACEN IGUAL

# PEGO DESDE LA LINEA 167 DE ENCODERS...BINARIA

# *********************************************************************************
# CODIFICACIONES VARIABLES CATEGORICAS PARA VARIABLE DEPENDIENTE BINARIA
# *********************************************************************************

# Para hacerlo más serio en un entorno de validación:

# https://towardsdatascience.com/benchmarking-categorical-encoders-9c322bd77ee8

# *********************************************************************************
# ENCODING ORDINAL (aunque en este caso ya son numéricas)
# *********************************************************************************

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

data22[["provincia.ord"]]<- encode_ordinal(data22[["provincia"]])


# save(data2,file="data2.Rda")

dput(names(data22))

# *********************************************************************************
# HASHING
# *********************************************************************************
library(FeatureHashing)

data33<-data22
data33 <- hashed.model.matrix(c("provincia"), data33, hash.size = 12,
                             create.mapping = TRUE)
# Extract the mapping
mapping <- hash.mapping(data33)
# Check collision rate
mean(duplicated(mapping))

data33<-as.data.frame(as.matrix(data33))

colnames(data33) <- paste("Hash", colnames(data33), sep = ".")

data33<-cbind(data22,data33)

dput(names(data33))


# *********************************************************************************
# TARGET ENCODING PURO CON PROP. BASE
# *********************************************************************************
source("funcion target_encod_binaria.R")

data55 <- mediaporbin(data33, "provincia", "subtema1","propkPROV")
dput(names(data55))


# *********************************************************************************
# TARGET ENCODING PURO CON CON PARÁMETRO m de CONTROL, m mas alto, menos sobreajuste
# *********************************************************************************

data66<-target_encod_bin(data55,"provincia","subtema1",20,1,"targetbin1")


# *********************************************************************************
# RANK TARGET ENCODING 
# *********************************************************************************

# Mejor para controlar sobreajuste, usa la salida del target encoder puro.

data66$Rank1<-rank(data66$propkPROV)


# save(data4,file="data6.Rda")

# RANK COUNT ENCODING

data66$Rankc1<-rank(data66$i.count)


# IMPORTANTE: verificamos que es un data frame y no hay missings

data66<-as.data.frame(data66)


library(naniar)
gg_miss_var(data66)

dput(names(data66))


c("dias_transcurridos", "situacion", "es_anonima", "provincia", 
  "via_ingreso", "derivacion_institucion", "derivacion_judicializa", 
  "denunciante_tipo", "denunciante_edad_aparente", "MESES", "weekday_ingreso", 
  "localidad", "denunciante_genero1", "hora_categ", "nro_registro_interno", 
  "subtema1", "provincia.ord", "Hash.1", "Hash.2", "Hash.3", "Hash.4", 
  "Hash.5", "Hash.6", "Hash.7", "Hash.8", "Hash.9", "Hash.10", 
  "Hash.11", "Hash.12", "propkPROV", "iden", "i.count", "targetbin1", 
  "Rank1", "Rankc1")

# Para el ejemplo solo dejo las variables construidas a partir de provincia . Quito iden y i.count.

listcontii<-  c("provincia.ord", "Hash.1", "Hash.2", "Hash.3", "Hash.4", "Hash.5", 
    "Hash.6", "Hash.7", "Hash.8", "Hash.9", "Hash.10", "Hash.11", 
    "Hash.12", "propkPROV",  "targetbin1", "Rank1", 
    "Rankc1")

table(data66$subtema1)

source ("funcion steprepetido binaria.R")
tablaa<-steprepetidobinaria(data=data66,vardep=c("subtema1"),
                           listconti=listcontii,sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")

tablaa[[1]]



# modelo Freq contador
# 1   propkPROV   28        1
# 29 targetbin1   13        1




PROV <- subset(data66, select = c(nro_registro_interno, targetbin1, provincia))






save(PROV, file = "TRATA_PROV_encoder.RDA")


library(readxl)
library(openxlsx)

write.xlsx(PROV, file = "PROV-RELACION.xlsx", rowNames = FALSE) 