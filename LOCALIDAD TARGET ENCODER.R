
# library("readxl")
# datos <- read_excel("d:/kat/TRATA_limpia.xlsx")
setwd("C:/Users/Xuele/Documents/MACHINE LEARNING/TEMA 6")
load("TRATA_localidadTOTAL.rda")


TRATA_LOC<-TRATA
listconti<-c("dias_transcurridos")							
listclass<-c("situacion", "es_anonima", "provincia", "via_ingreso", "derivacion_institucion", 							
             "derivacion_judicializa", "denunciante_tipo", "denunciante_edad_aparente", 							
             "MESES", "weekday_ingreso", "localidad1", "denunciante_genero1", "hora_categ","nro_registro_interno")							
vardep<-c("subtema1")							


dput(names(TRATA_LOC))


c("situacion", "es_anonima", "provincia", "via_ingreso", "derivacion_institucion", 
  "derivacion_judicializa", "denunciante_tipo", "denunciante_edad_aparente", 
  "subtema1", "MESES", "weekday_ingreso", "localidad1", "denunciante_genero1", 
  "dias_transcurridos", "hora_categ","nro_registro_interno")

data2<-TRATA_LOC[,c(listconti,listclass,vardep)]

data2

# PROBAMOS CON localidad11, LAS DEMÁS SE HACEN IGUAL

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

data2[["localidad1.ord"]]<- encode_ordinal(data2[["localidad1"]])


# save(data2,file="data2.Rda")

dput(names(data2))

# *********************************************************************************
# HASHING
# *********************************************************************************
library(FeatureHashing)

data3<-data2
data3 <- hashed.model.matrix(c("localidad1"), data3, hash.size = 24,
                             create.mapping = TRUE)
# Extract the mapping
mapping <- hash.mapping(data3)
# Check collision rate
mean(duplicated(mapping))

data3<-as.data.frame(as.matrix(data3))

colnames(data3) <- paste("Hash", colnames(data3), sep = ".")

data3<-cbind(data2,data3)

dput(names(data3))


# *********************************************************************************
# TARGET ENCODING PURO CON PROP. BASE
# *********************************************************************************
source("funcion target_encod_binaria.R")

data5 <- mediaporbin(data3, "localidad1", "subtema1","propkLOC")
dput(names(data5))


# *********************************************************************************
# TARGET ENCODING PURO CON CON PARÁMETRO m de CONTROL, m mas alto, menos sobreajuste
# *********************************************************************************

data6<-target_encod_bin(data5,"localidad1","subtema1",40,1,"targetbinloc1")


# *********************************************************************************
# RANK TARGET ENCODING 
# *********************************************************************************

# Mejor para controlar sobreajuste, usa la salida del target encoder puro.

data6$Rank1<-rank(data6$propkLOC)


# save(data4,file="data6.Rda")

# RANK COUNT ENCODING

data6$Rankc1<-rank(data6$i.count)


# IMPORTANTE: verificamos que es un data frame y no hay missings

data6<-as.data.frame(data6)


library(naniar)
gg_miss_var(data6)

dput(names(data6))


c("dias_transcurridos", "situacion", "es_anonima", "provincia", 
  "via_ingreso", "derivacion_institucion", "derivacion_judicializa", 
  "denunciante_tipo", "denunciante_edad_aparente", "MESES", "weekday_ingreso", 
  "localidad1", "denunciante_genero1", "hora_categ", "subtema1", 
  "localidad1.ord", "Hash.1", "Hash.2", "Hash.3", "Hash.4", "Hash.5", 
  "Hash.6", "Hash.7", "Hash.8", "Hash.9", "Hash.10", "Hash.11", 
  "Hash.12", "Hash.13", "Hash.14", "Hash.15", "Hash.16", "Hash.17", 
  "Hash.18", "Hash.19", "Hash.20", "Hash.21", "Hash.22", "Hash.23", 
  "Hash.24", "propkLOC", "iden", "i.count", "targetbinloc1", "Rank1", 
  "Rankc1","nro_registro_interno")


# Para el ejemplo solo dejo las variables construidas a partir de provincia . Quito iden y i.count.

listconti<-  c("localidad1.ord", "Hash.1", "Hash.2", "Hash.3", "Hash.4", "Hash.5", 
               "Hash.6", "Hash.7", "Hash.8", "Hash.9", "Hash.10", "Hash.11", 
               "Hash.12", "Hash.13", "Hash.14", "Hash.15", "Hash.16", "Hash.17", 
               "Hash.18", "Hash.19", "Hash.20", "Hash.21", "Hash.22", "Hash.23", 
               "Hash.24", "propkLOC",  "targetbinloc1", "Rank1", 
               "Rankc1")

table(data6$subtema1)

source ("funcion steprepetido binaria.R")
tabla<-steprepetidobinaria(data=data6,vardep=c("subtema1"),
                           listconti=listconti,sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")

tabla[[1]]



# modelo Freq contador
# 1          propkLOC+Rank1   35        2
# 36 propkLOC+targetbinloc1    6        2




LOC <- subset(data6, select = c(nro_registro_interno, subtema1, targetbinloc1,localidad1))

save(LOC, file = "TRATA_LOC_encoder.RDA")

library(readxl)
library(openxlsx)

write.xlsx(LOC, file = "LOC-RELACION.xlsx", rowNames = FALSE) 