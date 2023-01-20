#Clear environment
rm(list=ls())

library(dplyr)
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(ggplot2)
library(rgdal)
library(tmaptools)
library(viridis)
library(data.table)
data.table::update.dev.pkg()

###set working directory
setwd("C:/Users/user/Desktop/Pruebas")

#Read data
base <- read.csv("C:/Users/user/Desktop/Pruebas/Nacidos_2019.csv")
base <- as.data.frame(base)

#Shape mapa Coombia por municipios Marco Geoestadístico Nacional
col <- read_sf("C:/Users/user/Desktop/Pruebas/MGN_MPIO_POLITICO.shp")  %>% rename(COD_MUNIC = MPIO_CCDGO)


#leer datos como data.table
DT = as.data.table(base)
View(DT)
#Punto 3a: Número de nacimientos por municipio
nacmun <- DT[, .(.N), by = .(COD_MUNIC)]
nacmun
View(nacmun)

#Agrupar datos por municipio
datamun <- base %>% group_by(COD_MUNIC) %>% 
  summarise(promtal=mean(TALLA_NAC), prompes=mean(PESO_NAC))


#Punto 3b: prevalencia de bajo peso al nacer
bajopeso <- DT[, .N, by = .(COD_MUNIC, PESO_NAC<5)]
View(bajopeso)

prevdata = left_join(bajopeso,nacmun, by = "COD_MUNIC")
View(prevdata)

prevdata = as.data.frame(prevdata)
prevdata <- prevdata  %>% group_by(COD_MUNIC) %>% 
  mutate(prev=N.x/N.y)

prevdata <- prevdata[prevdata$PESO_NAC ==FALSE, ]

col = col %>% 
  mutate(COD_MUNIC = as.numeric(COD_MUNIC))

#Unir datos municipios con shape.
dtf = left_join(col,prevdata, by = "COD_MUNIC")

#Gráfico
maptal <- tm_shape(dtf)+tm_polygons(col = "promtal", style = "quantile")
mappes <- tm_shape(dtf)+tm_polygons(col = "prompes", style = "quantile")

legend_title = expression("")
mapprev = tm_shape(dtf) +tm_polygons(col = "prev", title=legend_title, style = "quantile", palette="RdBu" ) + tm_borders() +tm_compass(type = "8star", position = c("right", "top")) + tm_layout(title = "Colombia") + tm_style("classic")
print(mapprev)

#Punto 3C:
madprim <- DT[, .N, by = .(COD_MUNIC, NIV_EDUM<3)]
View(madprim)

edumdata = left_join(madprim,nacmun, by = "COD_MUNIC")
View(edumdata)

edumdata = as.data.frame(edumdata)
edumdata <- edumdata  %>% group_by(COD_MUNIC) %>% 
  mutate(propedum=N.x/N.y)

edumdata <- edumdata[edumdata$NIV_EDUM ==FALSE, ]

col = col %>% 
  mutate(COD_MUNIC = as.numeric(COD_MUNIC))

#Unir datos municipios con shape.
dtf1 = left_join(col,edumdata, by = "COD_MUNIC")

legend_title = expression("")
mapedum = tm_shape(dtf1) +tm_polygons(col = "propedum", title=legend_title, style = "quantile", palette="RdBu" ) + tm_borders() +tm_compass(type = "8star", position = c("right", "top")) + tm_layout(title = "Colombia") + tm_style("classic")
print(mapedum)

#Punto3d

edadmad <- DT[, .N, by = .(COD_MUNIC, EDAD_MADRE<3)]


edaddata = left_join(edadmad,nacmun, by = "COD_MUNIC")
View(edaddata)

edaddata = as.data.frame(edaddata)
edaddata <- edaddata  %>% group_by(COD_MUNIC) %>% 
  mutate(edmad=N.x/N.y)

edaddata <- edaddata[edaddata$EDAD_MADRE ==FALSE, ]

col = col %>% 
  mutate(COD_MUNIC = as.numeric(COD_MUNIC))

#Unir datos municipios con shape.
dtf2 = left_join(col,edaddata, by = "COD_MUNIC")

legend_title = expression("")
mapedad = tm_shape(dtf2) +tm_polygons(col = "edmad", title=legend_title, style = "quantile", palette="RdBu" ) + tm_borders() +tm_compass(type = "8star", position = c("right", "top")) + tm_layout(title = "Colombia") + tm_style("classic")
print(mapedad)



basefinal <- left_join(datamun,prevdata, by = "COD_MUNIC")

basefinal1 <-left_join(basefinal,edumdata, by = "COD_MUNIC")

basefinal2 <-left_join(basefinal1,edaddata, by = "COD_MUNIC")

#base final
basefinal3 <-left_join(basefinal2,nacmun, by = "COD_MUNIC")
View(basefinal3)

BASE <- write.table(basefinal3, file = "BASE.RData")

