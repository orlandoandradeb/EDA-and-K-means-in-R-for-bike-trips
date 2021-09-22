#############################################  Anaálisis datos usuarios #################################################

library(sf)
library(sp)   ###  usarmos la funcion  over para asignaar una coordenada a un poligno espesificop de la ciudad. 
library(dplyr)
library(rgeos)    # para sacar los centroides de los polignos. 
library(spData)
library(stplanr)      # geographic transport data package
library(tmap)   
library(maptools)
library(mapdeck)
library(raster)    ### para la lectura de shapefile 
library(spatialEco)
library(plyr) # contiene la funcion count para realizar la conrtabilizacion de los viajes por zonas 
library(tidyverse)
library(ggplot2)
library(ggthemes)  # mas temas para ggplot 2
library(dygraphs)   # mapa de tiempo interacrivo 
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)   # convierte a fomarto POSIXct    dmy_hm()  etc depende el orden 
library(data.table)  # extraccion de hora (tiempo) o fecha (date) de una columa datetime con IDate(x)  y  as.ITime(x) 
library(colorspace)   ## lo requiere ggthemes
library(plotrix)    
library(cluster)
library(factoextra) 
library(jsonlite)


setwd("C:/Users/orlan/Documents/MOT/Modulos de análisis/Analisis R/Analisis OD datos usuarios")
rm(list = ls(all=TRUE))
getwd() 


### para el ejercicio juntamos los data frame por columas similares (estacion)
cordenadas_estaaciones <-  read.csv("MI_BICI.csv")

origen_mibici <-  read.csv("OrigenMibici.csv")
origen_mibici<- merge( cordenadas_estaaciones , origen_mibici , by="estacion")

#### Patro de generacion de viajes en el tiempo y por edad y genero 
#Histograma edades de usuarios  
ggplot(origen_mibici, aes(x= Año_de_nacimiento))  +  labs( title = "Histograma año de nacimiento usuarios",  y= "Usuarios", x= "Año de nacimiento") + 
  geom_histogram(binwidth = 1, aes(fill = ..count..) ) +    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style ="darkunica" ) + theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6"))


#sexo 
ggplot(origen_mibici, aes( x=Genero , group= Genero, color=Genero, fill=Genero )) + geom_bar()+    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style =   "darkunica" )  +  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) + 
  labs( title = "Histograma distribución de sexo", y ="Usuarios" )
# vease: https://www.r-graph-gallery.com/         https://www.data-to-viz.com/ 



##############                      Generacion de viajes a traves del tiempo               #############
# con la funcion as.data. frame  reestructuramos la columa de data and time para poder manopularla con mayore facilidad ( si lo tranforma a formato DATE)

origen_mibici$fecha <- as.Date(origen_mibici$Inicio_del_viaje, format='%d/%m/%Y')  # el formato por default da un orden de :  "2001-01-15"   año, mes , dia , hay que modigicarlo para que nos de el orden deseado

origen_mibici$dia=weekdays(origen_mibici$fecha)   ## Obtencion de los dias 

### histograma de viajes por dia  de la semana 
ggplot(origen_mibici, aes( x=dia , group= dia, color=dia, fill=dia )) + geom_bar()+    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style = "darkunica" )  + theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6"))+
  labs( title = "Distribución de viajes por días ", y ="Viajes" , x= "Dia")


###### VIAJES  atraves del tiempo  (dia concreto vs mes  en este caso) 
#IMPORTANTE: para represetnar tiempo (fechas concretas o horas) es mejor utilizar la libreria: dygraphs , la columa debe de estar en un formato de variable de tiempo NO en FACTOR
# en este caso no queremos los tiempos ( es decir la hora, solo la variacion de viajes por dias)

# contabilizacion de viajes por dia 
dias_viajes <- count(origen_mibici$fecha)

ggplot(dias_viajes, aes(x = x, y = freq )) +
  geom_line(color="#5499c7") + geom_point(size = 1.1, color="white") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")+  
  theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6")) +
  labs( title = "Variación de la demanda en el mes", y ="Viajes" , x= "Dia")  +
  ylim(0, 8000)


##### interacrivo 
don=xts(x = dias_viajes$freq, order.by = dias_viajes$x)

dygraph(don , main ="Distribución de viajes"  ) %>%
  dyOptions(stackedGraph = TRUE, fillGraph=TRUE, drawPoints = TRUE, pointSize = 2, fillAlpha=0.1, drawGrid = TRUE, colors=" #5499c7 ") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)  
# Vease: https://rstudio.github.io/dygraphs/    https://www.r-graph-gallery.com/317-time-series-with-the-dygraphs-library/ 
# vease: https://www.r-graph-gallery.com/         https://www.data-to-viz.com/ 
# vease: https://www.stat.berkeley.edu/~s133/dates.html




##### Variabilidad de los viajes a lo largo del dia (en esta etapa si se usara hms) TOMAR COMO EJEMPLO UN DIA ENTRE SEMANA Y FIN DE SMENA  

#### selecion de filas que hacen match con el valor de una columa 
lunes= subset(origen_mibici, dia=="lunes")

##Es necesario extrar SOLO EL TIEMPO (la hora) para poder hacer el plot de la variacion de la demanda a lo largo del  dia  (recuerdomoes questamos toamndo muchos lunes pero queremos ver el comportminao general  como de un solo dia por eso aislamos el tiempo)
## cambiamos al formato POSIXct , convertir a formato POSIXct la columa de fecha y hora 
lunes$Inicio_del_viaje <-  dmy_hm(lunes$Inicio_del_viaje)
str(lunes)   # ahora es formato POSIXct

## Extracion solo del tiempo (hora)
lunes$hora = strftime(lunes$Inicio_del_viaje, "%H")    # otra opcion:    lunes$hora <- as.ITime(lunes$Inicio_del_viaje)
### la operacion de arriba solo extrae el hora, se puede extrar mas %H:%m:%s   pero es  mejor asi para delimitar las lecturas  

### IMPORTANTE:  el formato POSIXct estae asociado a una ubicacion concretea, cuando extraes solo la hora de una columa la hora la traslada a dicha ubicacion, en este caso hay una diferencia de 5 horas. hay que ajustar eso 
lunes$hora <- as.numeric(lunes$hora)
lunes$hora <- lunes$hora + 5    # ajustado la diferencia por el formato POSIXct 
hits_hourL = count(lunes$hora)
hits_hourL$dia <- "Lunes"

### repetimos el proceso pero para el sabado para comoarar el comporatmeinto entre semana y fines de semana 
sabado= subset(origen_mibici, dia=="sábado")
sabado$Inicio_del_viaje <-  dmy_hm(sabado$Inicio_del_viaje)
sabado$hora = strftime(sabado$Inicio_del_viaje, "%H")    # otra opcion:    lunes$hora <- as.ITime(lunes$Inicio_del_viaje)
sabado$hora <- as.numeric(sabado$hora)
sabado$hora <- sabado$hora + 5    # ajustado la diferencia por el formato POSIXct 
hits_hourS = count(sabado$hora)
hits_hourS$dia <- "sábado"

jueves<- subset(origen_mibici, dia=="jueves")
jueves$Inicio_del_viaje <-  dmy_hm(jueves$Inicio_del_viaje)
jueves$hora = strftime(jueves$Inicio_del_viaje, "%H")    # otra opcion:    lunes$hora <- as.ITime(lunes$Inicio_del_viaje)
jueves$hora <- as.numeric(jueves$hora)
jueves$hora <- jueves$hora + 5    # ajustado la diferencia por el formato POSIXct 
hits_hourJ = count(jueves$hora)
hits_hourJ$dia <- "jueves"

#cambio de nombre de las columas  
names(hits_hourL)[names(hits_hourL) == "x"] <- "hora" # el de la izquierda es el nombre original y el de la derecha el nombre deseado 
names(hits_hourS)[names(hits_hourS) == "x"] <- "hora"  
names(hits_hourJ)[names(hits_hourJ) == "x"] <- "hora"  

hits_hour <- rbind(hits_hourL, hits_hourS, hits_hourJ)   # une con el mismo orden de columas (añade filas) ver la funcion cbind  esa agrega columas nuevas. 

ggplot(hits_hour, aes(x = hora, y = freq, group = dia, color = dia)) +
  geom_line() +
  geom_point(size = 1.1) +
  theme_hc(style =   "darkunica") +
  scale_fill_hc("darkunica") +
  labs( title = "Variación de la demanda en el día", y ="Viajes" , x= "Hora")  +
  ylim(0, 3500) + scale_x_continuous(breaks =  c(5,8,10,12,14,16,18,20,22,24)) + 
theme(legend.position = "right", axis.text.x=element_text(colour = "#a8a4a6"),  axis.text.y=element_text(size=9, colour = "#a8a4a6"))



#####         demostracion  de la variacion de viajes en el tiempo (tomando un dia como ejemplo)  KEPLER  ###################################
kepler <- subset( origen_mibici, origen_mibici$fecha == "2018-07-02" ) 

# pasarlo a formato tiempo 
kepler$Inicio_del_viaje <- dmy_hm(kepler$Inicio_del_viaje)

# adjudicion a una hora concreta de las mediciones 
kepler$hora = strftime(kepler$Inicio_del_viaje, "%H")    # otra opcion:    lunes$hora <- as.ITime(lunes$Inicio_del_viaje)

# ajuste de tiempo 
kepler$horaA <- as.numeric(kepler$hora)
kepler$horaA <- kepler$horaA + 5 

## realizamos la contabilizacion de viajes generados por estacion: 
viajes_estacion <- count( kepler, c('estacion','horaA'))

### unir data frames con columas  comunes 
viajes_estacion<- merge(viajes_estacion, cordenadas_estaaciones , by="estacion")

# convertirlo a formato tiempo: 
viajes_estacion$HORA <-  as.ITime(viajes_estacion$hora) 

# una vez ajustado se vuelve a convertir en formato tiempo 
write.csv(kepler, "kepler.csv" )



#########           SEGUNDA ETAPA DE ANALISIS       ########


## cambiar el nomnbre de una columna 
colnames(origen_mibici)

names( origen_mibici)[names(origen_mibici) == "lon"] <- "lon_o"
names(origen_mibici)[names(origen_mibici) == "lat"] <- "lat_o" # el de la izquierda es el nombre original y el de la derecha el nombre deseado 


destinos_mibici<-  read.csv("DestinoMibici.csv")
destinos_mibici<- merge( cordenadas_estaaciones , destinos_mibici , by="estacion")

colnames(destinos_mibici)

names( destinos_mibici )[names( destinos_mibici) == "lon"] <- "lon_d"
names( destinos_mibici)[names(destinos_mibici) == "lat"] <- "lat_d" # el de la izquierda es el nombre original y el de la derecha el nombre deseado 

#### En este punto ya contamos con las coordenadas de origen y las coordenadad de destino hay que ligarlas a la 
#zonoficacion (colonia) que coorespoonden cada una y despues unirlo en un dataframe con el Viaje_ID como comun 


###### Zonoficación de la zona de estudio    
GDL <-  shapefile ("Guadalajara.shp")
head(GDL@data)   # ver  la tabla de atributos del shape (solo los primeros 5 filas)
##la columa "nombre" es la que contiene el nombre de las zonas, tambien podemos usar la columa gid
plot(GDL)

#pasamos el dataframe de origen a un objeto espacial con las columas de las coordenadas:
coordinates(origen_mibici) = ~lon_o +   lat_o
proj4string(origen_mibici) <- CRS("+init=EPSG:4326")


points(origen_mibici, pch=20)
origen_mibici <- point.in.poly(origen_mibici, GDL) # aqui uno los distritos o zonas con las coordenadas de origen
head(origen_mibici@data)


#generar un dataframe con el ID de las zonas de destino, posteriomente  los uniremos con la columna viaje_id
coordinates(destinos_mibici) = ~lon_d +   lat_d
proj4string(destinos_mibici) <- CRS("+init=EPSG:4326")
plot(GDL)
points(destinos_mibici, pch=20 )
destinos_mibici <- point.in.poly(destinos_mibici, GDL)# aqui uno los distritos o zonas con las coordenadas de origen
head(destinos_mibici@data)

###Hacemos un merge y creamos la matriz  OD de los dataframe de origen y destino tomand el viaje_ID como columa comun
## los pasaremos a dataframe de nuevo para hacerle el merge y borrar despues columnas repetidas 
destinos_mibici <- as.data.frame(destinos_mibici)
origen_mibici <- as.data.frame(origen_mibici)
matrizOD <- merge(origen_mibici , destinos_mibici , by="Viaje_Id")
matrizOD <- matrizOD[ ,-c ( 3,4,5,6,7,9,10,15,21,22)] 
### Reordanamiento y rename de las columnas  
#reordenamiento 
matrizOD2 <- matrizOD[ ,c (1,8,9,10,11,2,3,4,5,6 ,7,12,13,14,15 )] 


#renames la matriz OD
colnames(matrizOD2)

names(matrizOD2 )[names(matrizOD2) == "Año_de_nacimiento.y"] <- "Año_de_nacimiento"
names(matrizOD2)[names(matrizOD2) == "Genero.y"] <- "Genero" # el de la izquierda es el nombre original y el de la derecha el nombre deseado 
names(matrizOD2 )[names(matrizOD2) == "inicio_del_viaje.y"] <- "Inicio_de_viaje"
names(matrizOD2 )[names(matrizOD2) == "Fin_del_viaje.y"] <- "Fin_del_viaje"
names(matrizOD2 )[names(matrizOD2) == "estacion.x"] <- "estacion_origen"
names(matrizOD2 )[names(matrizOD2) == "gid.x"] <- "gid_origen"
names(matrizOD2 )[names(matrizOD2) == "nombre.x"] <- "nombre_origen"
names(matrizOD2 )[names(matrizOD2) == "coords.x1.x"] <- "lon_o"
names(matrizOD2 )[names(matrizOD2) == "coords.x2.x"] <- "lat_o"
names(matrizOD2 )[names(matrizOD2) == "estacion.y"] <- "estacion_destino"
names(matrizOD2 )[names(matrizOD2) == "gid.y"] <- "gid_destino"
names(matrizOD2 )[names(matrizOD2) == "nombre.y"] <- "nombre_destino"
names(matrizOD2 )[names(matrizOD2) == "coords.x1.y"] <- "lon_d"
names(matrizOD2 )[names(matrizOD2) == "coords.x2.y"] <- " lat_d"
names(matrizOD2) [names(matrizOD2) == "Usuario_Id.y"] <- "Usuario_ID"

colnames(matrizOD2)


### contabilizar los viajes  generados y recibidios  en cada zona 
total_trips <- count(matrizOD2, c('nombre_origen','nombre_destino'))
# esta funcion (count) contabilizara cuantas veces existe una relacion entre la columa (nombre_origen) y (nombre_destino)
### cunado tenga mas modalidades se realizara una contabilidad similar y se mezclaran posteriorement en un 
#nuevo dataframe con el total de trips de cada modalidad.

# tenemos que adjudicar la cantida de  viajes generados y la cantidad de viajes recibidios a cada zona para plotearlo en un mapa estatico de colors de origen y destiono por zonas 
total_trips_generados_zona= aggregate(total_trips$freq, by=list(nombre=total_trips$nombre_origen), FUN=sum)
total_trips_recibidos_zona= aggregate(total_trips$freq, by=list(nombre=total_trips$nombre_destino), FUN=sum)

#### cambiamos la columa x por un nombre "trips_generados " "trips_atraidos"
names(total_trips_generados_zona)[names(total_trips_generados_zona) == "x"] <- "trips_generados"
names(total_trips_recibidos_zona)[names( total_trips_recibidos_zona) == "x"] <- "trips_atraidos"

##### Vamos a unir la cantidad de trips generados  por zona y los trips atraidos por zona a el shape #######

GDL@data = data.frame(GDL@data,total_trips_generados_zona[match(GDL@data [, "nombre"], total_trips_generados_zona[,"nombre"]),] )
GDL@data = data.frame(GDL@data,total_trips_recibidos_zona[match(GDL@data [, "nombre"], total_trips_recibidos_zona[,"nombre"]),] )


### eliminamos las columas repetifas de la tabla de atributos de un OBJETO GEOESPACIAL poligono (principalmente los nombres repetidos)

head(GDL@data,7)## el head al agregarle la coma y un numero permite selecionar cuantas filas se desplegaran en la consola 
GDL <- GDL[,-(5)] #remove column 5
GDL <- GDL [,-(6)] 

head(GDL@data,7)## el head al agregarle la coma y un numero permite selecionar cuantas filas se desplegaran en la consola 



#####         Ploteo de generacion y atraccion de viajes  por distrito      ########
tmap_mode("view") # , tmap_mode(" plot")   es para verlos estaticos 
tmaptools::palette_explorer()   # para ver la peleta de colores 
# providers de leaflet : https://leaflet-extras.github.io/leaflet-providers/preview/    Wimimedia (es una base como google maps)

tm_shape(GDL) + tm_fill(col = "trips_generados", palette = "plasma", n= 6) + tm_basemap(leaflet::providers$CartoDB.DarkMatter) +  # OpenStreetMap.DE
tm_scale_bar(breaks = c(0, 100, 200), size = 1)  + tm_layout(title = "Generación de viajes")  + tm_borders(col = "#797776", lwd = 1)


tm_shape(GDL) + tm_fill(col = "trips_atraidos", palette = "plasma", n= 6) + tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)  + tm_layout(title = "Atraccion de viajes")  + tm_borders(col = "#797776", lwd = 1)

#### ploteo de densidad de atraccion de viajes 3D mapdeck

MAPBOX= 'pk.eyJ1Ijoib3JsYW5kb2FuZHJhZGViIiwiYSI6ImNqdHduOTlwMzBwamc0NHBnMDJ6bm5pN24ifQ.fny0kehwXxMQLb8pBkpxPA'
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX
#### Visualizacion en red 
mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = destinos_mibici
    , lat = "coords.x2"
    , lon = "coords.x1"
    , cell_size = 450      # 450*450 metros es el tamaño de las celdas 
    , elevation_scale = 5
    , colour_range = viridisLite::plasma(6)
    , layer_id = "grid_layer"
  )

##### Visualizacion en hexagono 
mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45) %>%
  add_hexagon(
    data = destinos_mibici
    , lat = "coords.x2"
    , lon = "coords.x1"
    , radius = 300
    , layer_id = "hex_layer"
    , elevation_scale = 8

  )



########                             CREACION DE LINEAS DE DESEO                ############

head(total_trips)   ## para el uso del comando od2lines es necesario que el archivo donde contiene la cantida de viajes totales tenga en la primerea columa el poligono de origen y en la segunda columa el poligono de destino (ID)
str(total_trips)
head(GDL@data) ## hay que cambiar el orden de las columas de GDL, que el primera columa sea "nombre" es decir el ID del poligono usado en el dataframe
GDL <- GDL [,-(1:3)] 
head(GDL@data)
str(GDL@data)


newflowlines <- od2line(flow = total_trips, zones = GDL)
plot(newflowlines)
head(newflowlines@data)
#### IMPORTANTE ######
### Flow=  es un dataframe que contiene los ID de los poligonos y los viajes realizados entre poligonos, la primera columa es origen, la segunda columa es destino 
## zones= objetco geo espacial el cual contiene los poligonos de OD, la primerae columa contiene el ID de los poligons utulizados en el data frame OD 


###       PLOTEO DE LINEAS DE DESEO 
tmaptools::palette_explorer()   # para ver la peleta de colores 
tmap_mode("view") 
tm_shape(newflowlines) +
  tm_lines(col="freq", lwd="freq", scale=11 ,legend.lwd.show = TRUE, palette = "Reds") + tm_layout(title = " Lineas de deseo") +tm_basemap(leaflet::providers$CartoDB.DarkMatter)+
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)   #scale sera el tamaño final de la linea 







####################     Visualizacion de la matriz OD  2D          ####################
library(reshape2)
library(igraph) # para convertilo en una matriz
### convertimos en una matriz
total_trips<-graph.data.frame(total_trips,directed=FALSE)
total_trips<-get.adjacency(total_trips,attr='freq',spars=FALSE)
total_trips<-as.data.frame(total_trips)



# el siguiente ploteo no requiere que se  pase a  un formato de matriz

### ploteo de matriz relacional    IMPORTANTE:  se lee de eje y a eje x, es decir (cuantos viajes desde el eje Y al eje X se hicieron )
ggplot(total_trips, aes(x = nombre_destino, y = nombre_origen)) + 
  geom_raster(aes(fill=freq)) + 
  scale_fill_gradient(low="grey", high="#8c1251") +
  labs(x="Destino", y="Origen", title="Matrix") +
  theme_hc(style ="darkunica") + theme(axis.text.x=element_text(size=8, angle=90, vjust=0.3, colour = "#a8a4a6"),
                     axis.text.y=element_text(size=9, colour = "#a8a4a6"),
                     plot.title=element_text(size=14), legend.position = "right")

### Deteccion de clusters en la matriz  vease:  https://rpubs.com/lgadar/matrix-visualizations




### En que zonas crecen mas los viajes?  (tasa de creicmiento o disminucion de viajes por zonas)

#### Nodes  ¿¿? 


### Route Network 


##### Priorización de  infraestructura 


#### Viajes futuros / celular automata 

###### Routes 






#############################              Cluster k means datos sociodemograficos de GDL              ###################

################################              CREACION DE CLUSTERS DEMOGRAFICOS          ################################## 
# 1) Data selection 
# 2) Preparation 
# 3) Culsterion 
# 4) Interpetration 

####################      1)    DATA SELECTION 
## Lectura de shapfile 
ZMG= shapefile("D:/IMEPLAN/Shapes/Colonias/Colonias AMG9 Mas actual/AMG9.shp")

tmap_mode("view")  

tm_shape(ZMG) + tm_fill(col = "MUN", palette = "Spectral", n= 6) + tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)  + tm_layout(title = "AMG")  + tm_borders(col = "black", lwd = 1)

DF= ZMG@data 


#### Eleccion de las columas a elegit 
DF <- subset(DF, select = c(251,5,192,9,60,112,118,124,130,136,137,138,139,144,145,151,154,184,188, 189, 190,191 ))

# Eliminacion de filas que contagan valores 0 en poblacion total y vivienda total dentro de la tabla de atributos del spatial object 
nrow(ZMG@data)
ZMG@data <- subset(ZMG@data, POBTOT > 0 & VIVTOT > 0 )
nrow(ZMG@data)

## Eliminacioan de filas repetidas dentro del spatial dataframe 
ZMG@data <- ZMG@data  %>% distinct(ID)   # disitinct PERTENECE AL PAQUETE :  dplyr  , con ese paquete se pueden modificar tablas de atributos 
nrow(ZMG@data)
# Remove duplicated rows en ela trabla de atributos sin columa espesifcia : dejar en blanco el interior del parenteses de distinct
# Vease: https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/


# eliminacion de  filas repetidas del data frame  (colonias con el mismo ID )
DF <- DF [!duplicated(DF$ID), ]   ####  AQUI ELIMINAMOS LAS FILAS REPETIDAS usandao la columa ID como indicador 

## Eliminacion de datos con valor cero de poblacion y viendandas totales 
DF <- subset(DF, POBTOT > 0 & VIVTOT > 0)




###############  2)    DATA PREPARATION, Data stadarisation (dejar todo en termeinos porcentuales)  


# % Poblacion Economicamente Activa PEA
DF$PEA <- (DF$PEA/DF$POBTOT)*100
names(DF)[names(DF) == "PEA"] <- "%_PEA"  ## el de la izquierda es el nombre original. 

## % PRES2005	Población de 5 años y más residente en la entidad en junio de 2005 (o que es una colonia nueva y se acaban de mudar)
DF$PRES2005 <- (DF$PRES2005/DF$POBTOT)*100
names(DF)[names(DF) == "PRES2005"] <- "%_LOCALES"  ## el de la izquierda es el nombre original. 

# P15PRI_IN  %  de personas con primaria incompleta (mayores de 15 años)
DF$P15PRI_IN <- (DF$P15PRI_IN /DF$POBTOT)*100
names(DF)[names(DF) == "P15PRI_IN"] <- "%_15Primaria_INC"  ## el de la izquierda es el nombre original. 

# P15SEC_IN % de personas con secundaria incomppleta (mayores de 15 años)
DF$P15SEC_IN <- (DF$P15SEC_IN /DF$POBTOT)*100
names(DF)[names(DF) == "P15SEC_IN"] <- "%_15Secundaria_INC"  ## el de la izquierda es el nombre original. 

# % POCUPADA Poblacion ocupada 
DF$POCUPADA <- (DF$POCUPADA /DF$POBTOT)*100
names(DF)[names(DF) == "POCUPADA"] <- "%_POCUPADA"  ## el de la izquierda es el nombre original. 

# % POCUPADA_M  Poblacion Masculina ocupada 
DF$POCUPADA_M <- (DF$POCUPADA_M /DF$POBTOT)*100
names(DF)[names(DF) == "POCUPADA_M"] <- "%_POCUPADA_M"  ## el de la izquierda es el nombre original. 

# % POCUPADA_MF Poblacion Femanina  ocupada 
DF$POCUPADA_F <- (DF$POCUPADA_F /DF$POBTOT)*100
names(DF)[names(DF) == "POCUPADA_F"] <- "%_POCUPADA_F" 

# % Poblacion desocupada 
DF$PDESOCUP <- (DF$PDESOCUP /DF$POBTOT)*100
names(DF)[names(DF) == "PDESOCUP"] <- "%_PDESOCUP" 

# % Poblacion inscrita al IMSS 
DF$PDER_IMSS <- (DF$PDER_IMSS /DF$POBTOT)*100
names(DF)[names(DF) == "PDER_IMSS"] <- "%_PDER_IMSS" 

# % poblacion isncrita al ISSTE 
DF$PDER_ISTE <- (DF$PDER_ISTE /DF$POBTOT)*100
names(DF)[names(DF) == "PDER_ISTE"] <- "%_PDER_ISTE" 

# % de poblacion catolica 
DF$PCATOLICA <- (DF$PCATOLICA /DF$POBTOT)*100
names(DF)[names(DF) == "PCATOLICA"] <- "%_PCATOLICA" 

# % de poblacion sin religion 
DF$PSIN_RELG <- (DF$PSIN_RELG /DF$POBTOT)*100
names(DF)[names(DF) == "PSIN_RELG"] <- "%_PSIN_RELG" 

#  % de viviendas con  vehiculos 
DF$VPH_AUTOM <- (DF$VPH_AUTOM /DF$VIVTOT)*100
names(DF)[names(DF) == "VPH_AUTOM"] <- "%_VPH_AUTOM" 

#  % de viviendas con  internet 
DF$VPH_INTER <- (DF$VPH_INTER /DF$VIVTOT)*100
names(DF)[names(DF) == "VPH_INTER"] <- "%_VPH_INTER" 

# % de  poblacion de 0 a 14 años 
DF$POB0_14 <- (DF$POB0_14 /DF$POBTOT)*100
names(DF)[names(DF) == "POB0_14"] <- "%_POB0_14" 

# % de poblacion de 15 a 64 años 
DF$POB15_64 <- (DF$POB15_64 /DF$POBTOT)*100
names(DF)[names(DF) == "POB15_64"] <- "%_POB15_64" 

# % de poblacion de 65 y mas 
DF$POB65_MAS <- (DF$POB65_MAS /DF$POBTOT)*100
names(DF)[names(DF) == "POB65_MAS"] <- "%_POB65_MAS" 

# % poblacion mayor de 18 años con educacion mas de prepra 
DF$P18YM_PB <- (DF$P18YM_PB /DF$POBTOT)*100
names(DF)[names(DF) == "P18YM_PB"] <- "%_P18YM_PB" 



#Ahora que tenemos los  % de las distintas variables, es momento de agregarle una nueva estandarizacion a los datos para que 
# los valores erraticos de una variables (o la naturaleza de magnitudes de cada variables) no afecten el proceso de clustering
# para esto  se realizará una  estandarizacion de los valores para que todos queden en funcion a la desviacion estandar de la media de dicha columa 

####        DESVIACION ESTANDAR A LA MEDIA de cada varible (estandarizacion  de las variables porcentuales)   ################

value <- colnames(DF)   # guarda en un vector el nommbre de las columas de mi dataframe 

# creates a new data frame
stand_data <- DF

# loops columns from position 1 : the last column
for(i in 5:ncol (DF)){       # para que haga el proceso automatico a todas las clumas es  "i in 1:ncol"
  stand_data[, value[i]] <- scale(as.numeric(DF[, value[i]]))}   ### Un proceso de normalizacion podria ser otra opcion

### IMPORTANTE: En este punto todas mis variables han sido estandarizadas, colocamos 5 en el FOR porque desde esa columa quiero que inice la estandarizacion de las 
# desviaciones estandar, porque las columas previas no tienen caso, son nombres o identificadores. la funcion SCALE will calculate the mean and standard deviation of the entire vector
# Vamos a crear un nuevo dataframe con todas las variables estandarizaadas con este método 




###########      MEASURE THE VARIABLES FOR ASSOCIATION        ######## 
# El siguiente proceso solo puede contener variables numericas, eliminaremos las primeras columas que contienen otra clase de datos no numericos o que no interean para el analisis 

stand_data<- stand_data[,-(1:4)]# remover columas con letras y las que no nos interesan 

###  IMPORTANTE: NI EL COEFRICIENTE PEARSON NI EL SIGUIENTE PASO QUE ES EL K-MEAS CLUSTERING FUNCIONAN  CON MISSING VALUES, por lo que removeremos las filas que contegan algun missing  value


# coeficiente pearson. 
coeficiente_pearson <- cor(stand_data, method = "pearson")   
coeficiente_pearson <- as.data.frame(coeficiente_pearson)

# ESta matriz relacional entre  pares  devariables es para medir la relacion de una con. El coeficiente  Pearson va de -1 a 1  # Entre más grande se el valor mayor sera la correlacion entre dichas variables (es por magnitud no por signo)  a mayor valor positivo 
# mayor la relacion  directamente porporcional, a myor negativo mayor la relacion INVERSAMENTE proporcional. As a rule of thumb, two variables with coefficients greater than ±0.8 can be considered to be highly correlated





#######################################  3) Clustering              K-means clustering                        ###########################


########   Eleccion del numero optimo de cluster #### 

# function to compute total within-cluster sum of square
# creates an empty data object first
wss <- NULL
# finding the tot.withinss for 15 kmeans models
for (i in 1:15) wss[i] <- kmeans(stand_data,centers = i,iter.max = 1000)$tot.withinss

plot(1:15, wss, type = "b", pch = 19, xlab = "Number of Clusters",
     ylab = "Total within-cluster sum of squares")
#### IMPORTANTE: LA GRAFICA anterior muestra la suma de los cuadrados de las desviaciones estandar de cada lecturda a los centrodis, mayores suman mayores montos de cuadrados de las desviaciones al centroides de los clusters
# por lo que significa menos homogenieidan de los cluster,  esta grafica es improtante  para la eleccion de la cantidad de clusters  optimos que muestran una homogenieidad en las lecturas pero que al mismo tiempo no sean tantos para que permita una diferenciacion clara entre clusters. 


# eleccion OPTIMA DE CANTIDAD DE CLUSTERS para este caso de cantidad de clusters : 

km <-  kmeans(stand_data, 5,nstart =25, iter.max= 1200) ## Entender bien el metodo de kmenas (creacion de cluster o coordenadas iniciales de cada row debido a sus valores en las variables, el proceso se iterra y va agrupando al conjunto de lecturas que tengan un mayor precido  a ese set de "coordendas" o  posicion de varibales)
#6 clusters
# the cluster membership for each case
KmClusters <- as.matrix(km$cluster)
KmClusters <- as.data.frame(KmClusters)

# the cluster centres
KmCenters <- as.matrix(km$centers)
KmCenters <- as.data.frame(KmCenters)
# the frequency of cases in each cluster
hist(KmClusters$V1)


############ distincion entre clusters 
# we can also look at the betweenss values to observe how distinctive the clusters are
bss <- NULL
for (i in 1:15) bss[i] <- kmeans(stand_data,centers = i,iter.max = 1000)$betweenss
plot(1:15, bss, type = "b", pch = 19, xlab = "Number of Clusters", ylab = "The between-cluster sum of squares")



#########  4) Interpratecion de cluster 

# Clasificacion:una vez selecionado el # de clusters es momento de tener una visualizacion e interpetacion de los cluster, para esto se haran 2 visulizaciones: 

#Visualizacion 1 
clusplot(stand_data,km$cluster, color = TRUE, shade = FALSE,
         labels = 4, lines = 0, plotchar = FALSE)

#Visualizacion 2 
fviz_cluster(km, data = stand_data, geom = "point", ellipse = F, pointsize = 0.7, ggtheme = theme_classic())

# Interpretacion de los cluster:  el dataframe de Kmcenters contiene las coordendas de los centroides de cada cluster 
#Visualizacion de KMCENTERS los cuales son las coordendas de los centroides de cada grupo (cluster): 

radial.plot(KmCenters[1,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = FALSE,
            line.col = "blue", radlab = TRUE )
help("radial.plot")
### IMPORTANTE: para visualizar las coordendas o los valores que componen el centroide de cada grupo podemos optmar por una grafica radial
# que muestre lo valores de cada grupo , la grafica anterio muestra los valores de  cada variable para el grupo 1 



#mismo concepto pero con  areas en el plot 
radial.plot(KmCenters[1,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE,
            rp.type = "p", main =  "Cluster 1")

radial.plot(KmCenters[2,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE,
            rp.type = "p", main = "Cluster 2")

radial.plot(KmCenters[3,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE,
            rp.type = "p", main = "Cluster 3")

radial.plot(KmCenters[4,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE,
            rp.type = "p", main = "Cluster 4")

radial.plot(KmCenters[5,], labels = colnames(KmCenters),
            boxed.radial = FALSE, show.radial.grid = TRUE,
            line.col = "blue", radlab = TRUE,
            rp.type = "p", main = "Cluster 5")


#Extract join the cluster labels to the first column of the pop_data file (OA codes)
Classification <- as.data.frame(cbind(as.character(DF[,1]), KmClusters[,1]))
#we need to rename the column headers
names(Classification) <- c("oacode", "Classification")




######                               Ploteo de Klusters             #########

# Union de dataframe con clasificacioan con la tabla de atributos  con el ID de los poligonos.
DF <- cbind(DF, Classification)

#Lectura del  objeto geo espacial 
ZMG2 <- shapefile("D:/IMEPLAN/Shapes/Colonias/Colonias AMG9 Mas actual/AMG9.shp")
ZMG2<- ZMG2[,-(1:250)] 

# Union del dataframe que contiene la clasificacion y el ID con  el objeto espacial 
ZMG2@data = data.frame(ZMG2@data,DF[match(ZMG2@data [, "ID"], DF[,"ID"]),] )

tmap_mode("view")  
tm_shape(ZMG2) + tm_fill(col = "Classification", palette = "Spectral", n= 6) + tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)  + tm_layout(title = "AMG")  + tm_borders(col = "black", lwd = 1)








##########################################             DATOS GENERADOS POR  USUARIOS     MOT       #################################
#lectura de datos desde el servidor 
Data_Users  <- fromJSON("http://18.224.39.132:7070/api/fetchUserData")

#los datos estan de distance en metros y duration en segundos 

Data_Users$distance <- (Data_Users$distance/1000)   # Distnacia en kilometros (originalmente esta en metros)
Data_Users$duration <- (Data_Users$duration/60)   # (originalmente esta en segundos, lo pasamos a minutos 


#Distribucion de longitudes de viajes 
ggplot(Data_Users, aes( x=Data_Users$distance )) + geom_histogram(binwidth = 1, aes(fill = ..count..) ) +    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style =   "darkunica" )  + theme(legend.position = "none") + labs( title = "Distribución de distancia de viajes", y ="Viajes" , x="Disntance en kilometros")


#Distribucion de  duracion de viajes en minutos 
ggplot(Data_Users, aes( x=Data_Users$duration )) + geom_histogram(binwidth = 5, aes(fill = ..count..) ) +    #bindwidth indicara el salto de paso entre los grupos de medicion de la  frecuencia 
  theme_hc(style =   "darkunica" )  + theme(legend.position = "none") + labs( title = "Distribución de duración de viajes", y ="Viajes" , x="minutos")


#### Visualizacion de  viajes 
MAPBOX= 'pk.eyJ1Ijoib3JsYW5kb2FuZHJhZGViIiwiYSI6ImNqdHduOTlwMzBwamc0NHBnMDJ6bm5pN24ifQ.fny0kehwXxMQLb8pBkpxPA'
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_line(
    data = Data_Users
    , layer_id = "line_layer"
    , origin = c("origin_lng", "origin_lat")
    , destination = c("destination_lng", "destination_lat")
    , stroke_colour = "#ce0c0c"
    , stroke_width = "freq"
  )





##########################################                FIN                      ##################################





    
#################################                 PRUEBAS / EJERCICIOS             #################################
#### ploteo de estaciones (puntos con mapdeck)
MAPBOX= 'pk.eyJ1Ijoib3JsYW5kb2FuZHJhZGViIiwiYSI6ImNqdHduOTlwMzBwamc0NHBnMDJ6bm5pN24ifQ.fny0kehwXxMQLb8pBkpxPA'
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_scatterplot(
    data = destinos_mibici
    , lat = "lat_d"
    , lon = "lon_d"
    , radius = 70
    , fill_colour = "#aa2c2c"     ## funciona con hex color es el codigo del color 
    , layer_id = "scatter_layer"
  )


#### Generacion de centroides , intento de generar lineas de deseo por mi parte si nla funcion od2line: 

## las lineas de deseo contectan a los centroides de los poligonos de los viajes 
# generacion de centroides: 
Centroides = gCentroid(GDL,byid=TRUE)
plot(GDL)
points(Centroides,pch=20)
Centroides <- point.in.poly(Centroides, GDL) # me une el dataframe del poligono con las coordenadas del centroide 

#### lo pasamos a dataframe 
Centroides=as.data.frame(Centroides)
#corroboramos la adiccion de las coordenadas de los centroides con el dataframe de los poligonos correspondientes
head(Centroides,10)
Centroides <- Centroides [,-c (1)] 
names(Centroides)[names(Centroides) == "coords.x1"] <- "lon"
names(Centroides)[names(Centroides) == "coords.x2"] <- "lat"
head(Centroides,10)
### Ahora que tengo las coordenadas de los centroides y tengo el nombre o ID del poligno que corresponden vamos a agregarlos 
# al  objeto donde contego los trips realizadeos 
### NOTA:   retomaremos el dataframe total_trips ya que en este contiene la cantidad total de viajes realizados de poligono a poligono 
head(total_trips)

Centroides_origen <-  Centroides [,-c (1,2,3, 5,6)] 
names(Centroides_origen)[names(Centroides_origen) == "nombre"] <- "nombre_origen"
total_trips<- merge(total_trips, Centroides_origen, by="nombre_origen")
names(total_trips)[names(total_trips) == "lon"] <- "lon_o"
names(total_trips)[names(total_trips) == "lat"] <- "lat_o"

Centroides_destino <-  Centroides [,-c (1,2,3, 5,6)] 
names(Centroides_destino)[names(Centroides_destino) == "nombre"] <- "nombre_destino"   # le cambiamos el nombre para poder unirlo con el data de total_trips que contiene la columa: nombre_destino 
total_trips<- merge(total_trips, Centroides_destino, by="nombre_destino")
names(total_trips)[names(total_trips) == "lon"] <- "lon_d"
names(total_trips)[names(total_trips) == "lat"] <- "lat_d"


####### Ploteo de las lineas de deseo  3D con Mapdeck 
MAPBOX= 'pk.eyJ1Ijoib3JsYW5kb2FuZHJhZGViIiwiYSI6ImNqdHduOTlwMzBwamc0NHBnMDJ6bm5pN24ifQ.fny0kehwXxMQLb8pBkpxPA'
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_line(
    data = total_trips
    , layer_id = "line_layer"
    , origin = c("lon_o", "lat_o")
    , destination = c("lon_d", "lat_d")
    , stroke_colour = "ID"
    , stroke_width = "freq"
  )


### Ejemplo de ploteo con distintos  themas en ggplot2 (LINEAS)

dtemp <- data.frame(months = factor(rep(substr(month.name, 1, 3), 4),
                                    levels = substr(month.name, 1, 3)),
            city = rep(c("Tokyo", "New York", "Berlin", "London"),
                               each = 12),
            temp = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6, -0.2, 0.8, 5.7, 11.3, 17.0, 22.0, 24.8, 24.1, 20.1, 14.1, 8.6, 2.5,
               -0.9, 0.6, 3.5, 8.4, 13.5, 17.0, 18.6, 17.9, 14.3, 9.0, 3.9, 1.0, 3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8))

ggplot(dtemp, aes(x = months, y = temp, group = city, color = city)) +
  geom_line() +
  geom_point(size = 1.1) +
  ggtitle("Monthly Average Temperature") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

#  Fuente:   https://jrnold.github.io/ggthemes/reference/theme_hc.html

### NOTA:
# onvertir un dataframe  que contenga puntos en un objeto geoespacial: 
data(meuse)     #meuse es el dataframe que contine columa con el nombre x & y  como coordendas 
coordinates(meuse) = ~x+y

# eliminicacion o slececion de  columnas
OD <- OD [ ,-c (2, 3)]   ## esto BORRARA las columas selecionadas altenativamente puedes ELEGIR cuales se quedan con:
## test3 <- test[,c(1,3)]     es decir solo quitando el simbolo - 





#####  Generacion de una grafica de corelaccion de matriz  2D
A <- matrix(c(2,5,2,1,0,0,0,0,1,0,0,0,0,1,3,5,6,0,0,1,0,0,0,2,0,0,1,2,7,2,4,6,2,5,1,0,0,1,0,0,0,1,0,0,3,5,4,0,0,1,0,0,1,0,0,2,0,3,5,7,3,1,4,0,1,0,0,0,0,2,0,0,0,1,3,4,6,0,0,1), byrow=T, nrow=8, ncol=10)
colnames(A) <- letters[1:10]
rownames(A) <- LETTERS[1:8]
print(A)


longData<-melt(A)    ###  esta  funcion (MELT) es muy importante, toma una matriz y la convierte en una data frame de 3 columas relacionando las variables y valores entre  filas(variables)
longData<-longData[longData$value!=0,]

ggplot(total_trips, aes(x = nombre_origen, y = nombre_destino)) + 
  geom_raster(aes(fill=freq)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))







