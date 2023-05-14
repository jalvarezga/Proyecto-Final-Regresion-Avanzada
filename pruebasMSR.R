library(dplyr)
source(paste0(getwd(), "/paqueteTecnico/claseNietoBUGS.R"))
coordenadas <- read.csv("csvFolder/coordenadasCentrales.csv", header = TRUE, sep = ",")


# Modificación de Y_i 
poblacionNeigh <- c(19261, 17783, 9648, 55297, 19890, 126909, 18306, 47263, 33489, 38924, 40867, 5351, 26659, 17386, 8749, 30021, 54161, 36772, 4403, 32571, 6619, 33526)
neighNamesVector <- c("Allston", "Back Bay", "Beacon Hill", "Brighton", "Charlestown", "Dorchester", "Downtown", "East Boston", "Fenway", "Hyde Park", "Jamaica Plain", "Longwood", "Mattapan", "Mission Hill", "North End", "Roslindale", "Roxbury", "South Boston", "South Boston Waterfront", "South End", "West End", "West Roxbury")

mVector <- data.frame(cbind(neighNamesVector, poblacionNeigh))

mVector$mVectorInicial <- as.numeric(mVector$poblacionNeigh)

write.csv(mVector[,c(1,3)], "csvFolder/vector-mi.csv", row.names = FALSE)

datosFiltradosNeigh <- datosFiltrados %>% 
  group_by(distrito) %>% 
  summarise(count = n())

write.csv(datosFiltradosNeigh, "csvFolder/vector-xi.csv", row.names = FALSE)

# vector respuesta----------------------------------------------
vector_yi <- data.frame(prop = log(datosFiltradosNeigh$count/mVector$mVectorInicial), neigh = datosFiltradosNeigh$distrito)


write.csv(vector_yi, "csvFolder/vector-yi.csv", row.names = FALSE)

# creación de regresores-----------------------------------------

data_edades <- read.csv("csvFolder/base_edades.csv", header = TRUE, sep = ",")
data_edades[,-1]<- apply(data_edades[,-1], 2, quitarPorcentaje)

data_educacion <- read.csv("csvFolder/base_educacion.csv", header = TRUE, sep = ",")
data_educacion[,-1]<- apply(data_educacion[,-1], 2, quitarPorcentaje)

data_householdtype <- read.csv("csvFolder/base_householdtype.csv", header = TRUE, sep = ",")
data_householdtype[,-1]<- apply(data_householdtype[,-1], 2, quitarPorcentaje)

data_movilidad <- read.csv("csvFolder/base_movilidad.csv", header = TRUE, sep = ",")
data_movilidad[,-1]<- apply(data_movilidad[,-1], 2, quitarPorcentaje)

data_percapita <- read.csv("csvFolder/base_percapita.csv", header = TRUE, sep = ",")

data_pobreza <- read.csv("csvFolder/base_pobreza.csv", header = TRUE, sep = ",")
data_pobreza$Poverty.rate<- quitarPorcentaje(data_pobreza$Poverty.rate)

data_razas <- read.csv("csvFolder/base_razas.csv", header = TRUE, sep = ",")
data_razas[,-1] <- apply(data_razas[,-1], 2, quitarPorcentaje)

data_indices <- read.csv("csvFolder/indices.csv", header = TRUE, sep = ",")









