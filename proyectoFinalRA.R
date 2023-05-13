###
### 
datosCrudos=read.csv("csvFolder/crimenes2022.csv",header=TRUE)
#Filtramos datos
datosCrudos$OFFENSE_DESCRIPTION=as.factor(datosCrudos$OFFENSE_DESCRIPTION)
summary(datosCrudos$OFFENSE_DESCRIPTION)
datosFiltrados=datosCrudos[(datosCrudos$OFFENSE_DESCRIPTION=='LARCENY ALL OTHERS')|
              (datosCrudos$OFFENSE_DESCRIPTION=='ASSAULT - SIMPLE')|
              (datosCrudos$OFFENSE_DESCRIPTION=='VANDALISM')|
              (datosCrudos$OFFENSE_DESCRIPTION=='LARCENY SHOPLIFTING')|
              (datosCrudos$OFFENSE_DESCRIPTION=='LARCENY THEFT FROM MV - NON-ACCESSORY')|
              (datosCrudos$OFFENSE_DESCRIPTION=='ASSAULT - AGGRAVATED')|
              (datosCrudos$OFFENSE_DESCRIPTION=='AUTO THEFT - MOTORCYCLE / SCOOTER')|
              (datosCrudos$OFFENSE_DESCRIPTION=='FIREARM/WEAPON - FOUND OR CONFISCATED')|
              (datosCrudos$OFFENSE_DESCRIPTION=='BURGLARY - RESIDENTIAL')|
              (datosCrudos$OFFENSE_DESCRIPTION=='ROBBERY')|
              (datosCrudos$OFFENSE_DESCRIPTION=='LARCENY THEFT OF BICYCLE')|
              (datosCrudos$OFFENSE_DESCRIPTION=='LARCENY THEFT OF MV PARTS & ACCESSORIES')
            ,]
head(datosFiltrados)
tail(datosFiltrados)                
datosFiltrados[datosFiltrados$OFFENSE_DESCRIPTION=='ROBBERY',]
summary(datosFiltrados$Lat) #hay algunos datos sin latitud
summary(datosFiltrados$Long)#hay algunos datos sin longitud
is.na(datosFiltrados$Lat)
is.na(datosFiltrados$Long)
dim(datosFiltrados)
is.na(datosFiltrados$Long)==is.na(datosFiltrados$Lat)
sum(is.na(datosFiltrados$Long)==is.na(datosFiltrados$Lat))
dim(datosFiltrados)
#los datos sin coordenada de latitud o longitud son los mismos
datosFiltrados=datosFiltrados[is.na(datosFiltrados$Long)==FALSE,]
summary(datosFiltrados$Long) #ya no hay datos faltantes

# Hacemos código de voronoi
Distritos=read.csv('csvFolder/coordenadasCentrales.csv')
Distritos
Distritos$Neighborhood
Distritos$Latitude
distrito=c()
for(i in 1:(dim(datosFiltrados)[1])){
  distancias=rep(0,22)
  for(k in 1:22){
    distancias[k]=(Distritos$Latitude[k]-datosFiltrados$Lat[i])^2+(Distritos$Longitude[k]-datosFiltrados$Long[i])^2
  }
  indice=which.min(distancias)
  distrito[i]=Distritos$Neighborhood[indice]
}
distrito
datosFiltrados=cbind(datosFiltrados, distrito)
datosFiltrados
write.csv(datosFiltrados, "csvFolder/datosFiltrados.csv", row.names = FALSE)
head(datosFiltrados)
library(sf)
#install.packages('mapview')
library(mapview)
datosFiltrados$Lat
mapview(datosFiltrados, xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
        layer.name = c("Crimenes"), col.regions='blue')
#Vista general de los crimenes sin estratificar


#Ahora los estratificamos por distrito
unique(datosFiltrados$distrito)

length(unique(datosFiltrados$distrito)) #tenemos los 22 distritos

colors=c('forestgreen', 'wheat4','darkorchid1', 'gold', 'navy','lightgreen','orange', 'slateblue',
                      'magenta', 'red', 'brown', 'tan', 'olivedrab', 'dodgerblue','darksalmon' ,
                      'navy', 'yellow', 'slateblue','seagreen', 'orangered', 'orchid', 'mediumvioletred')
length(colors)
p=mapview(datosFiltrados[datosFiltrados$distrito==unique(datosFiltrados$distrito)[1],], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
                                layer.name = c(unique(datosFiltrados$distrito)[1]), col.regions=colors[1])
p
#y ahora agregamos  los distritos estratificando
for(i in 2:length(unique(datosFiltrados$distrito))){
  print(i)
p=p+mapview(datosFiltrados[datosFiltrados$distrito==unique(datosFiltrados$distrito)[i],], xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE,
layer.name = c(unique(datosFiltrados$distrito)[i]), col.regions=colors[i])
}
p



