#Elaborado por: Monica Palomino 200611835, Paula Urbina 201633091, Nicolás Danies 201822710
#Colaborador: Todos los miembros!
#Fecha de elaboración:09 de junio del 2021
#Fecha última modificación: 10 de junio del 2021

#Se realizó el Taller B

#initial configuration
rm(list=ls())
pacman::p_load(here,tidyverse,viridis,gapminder,sf,skimr)


#Punto 1.1 Datos espaciales

#1.1 Importar datos espaciales

#1.1.1
via = st_read("data/input/VIAS.shp")
puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

#1.1.2


#1.1.3
c_poblado=readRDS("data/input/c poblado (2017).rds") %>% subset(cod_dane >= 54001 & cod_dane<55000)
depto=readRDS("data/input/dp deptos (2017).rds") %>% subset(cod_dpto==54)
mapmuse=readRDS("data/input/victimas_map-muse.rds") %>% subset(cod_mpio>=54001 & cod_mpio<55000)

#1.2 atributos


#1.3 Geometrias del Objeto
#Punto 1.3.1
st_crs(c_poblado)
st_bbox(c_poblado)

st_crs(depto)
st_bbox(depto)

st_crs(mapmuse)
st_bbox(mapmuse)

#Punto 1.3.2

c_poblado=st_transform(c_poblado,"+proj=utm +zone=19
+datum=WGS84 +units=m +no_defs" )

depto=st_transform(depto,"+proj=utm +zone=19
+datum=WGS84 +units=m +no_defs" )

mapmuse=st_transform(mapmuse,"+proj=utm +zone=19
+datum=WGS84 +units=m +no_defs" )

#1.4 Operaciones Geometricas

#1.4.1
mapmuse = mapmuse[depto,]
mapmuse=st_intersection(mapmuse,depto)

#1.4.2
clip_vias = st_intersection(via,c_poblado %>% subset(cod_dane==54001)) %>% st_length()

#1.5 Pintarmapas

#1.5.1
leaflet() %>% addTiles() %>% addCircleMarkers(data=puntos %>% st_transform(.,4326))

#1.5.2