#Elaborado por: Monica Palomino 200611835, Paula Urbina 201633091, Nicolás Danies 201822710
#Colaborador: Todos los miembros!
#Fecha de elaboración:09 de junio del 2021
#Fecha última modificación: 10 de junio del 2021

#Se realizó el Taller B

#initial configuration
rm(list=ls())
pacman::p_load(here,tidyverse,viridis,gapminder,sf,skimr,raster,lwgeom,jtools,ggstance,broom.mixed, outreg,ggplot2,VGAM,rvest)


#Punto 1.1 Datos espaciales

#1.1 Importar datos espaciales

#1.1.1
via = st_read("data/input/VIAS.shp")
puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

#1.1.2
infr=st_read("data/input/MGN_URB_TOPONIMIA.shp")
hospitales= subset(infr,CSIMBOL %in% c("021001","021002","021003"))

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


#2.0

#Cargar datos

mapmuse=readRDS("data/input/victimas_map-muse.rds")
depto=readRDS("data/input/dp deptos (2017).rds")%>% subset(name_dpto=="NORTE DE SANTANDER")
mapmuse=mapmuse[depto,]
vias=st_read("data/input/VIAS.shp")
infr=st_read("data/input/MGN_URB_TOPONIMIA.shp")
c_poblado=readRDS("data/input/c poblado (2017).rds") %>% subset(codmpio>54001 & codmpio<55000)
hospitales= subset(infr,CSIMBOL %in% c("021001","021002","021003"))



#DISTANCIAS MINIMAS VIAS

#encontramos nuestras distancias
dist_vias=st_distance(mapmuse,vias)
dist_vias=data.frame(dist_vias)

#convertimos los valores en numeros
dist_vias=lapply(dist_vias,function(x) as.numeric(x))
dist_vias=data.frame(dist_vias)

#Obtenemos los menores valores de las tablas
dist_vias=apply(dist_vias,MARGIN = 1,min)
dist_vias=data.frame(dist_vias)
#View(dist_vias)

#se repite para los otros dos.

#distancias minimas hospitales
dist_cmedico=st_distance(mapmuse,hospitales)
dist_cmedico=data.frame(dist_cmedico)
dist_cmedico=lapply(dist_cmedico,function(x) as.numeric(x))
dist_cmedico=data.frame(dist_cmedico)
dist_cmedico=apply(dist_cmedico,MARGIN = 1,min)
dist_cmedico=data.frame(dist_cmedico)
#View(dist_cmedico)

#distancias minimas poblado
dist_cpoblado=st_distance(mapmuse,c_poblado)
dist_cpoblado=data.frame(dist_cpoblado)
dist_cpoblado=lapply(dist_cpoblado,function(x) as.numeric(x))
dist_cpoblado=data.frame(dist_cpoblado)
dist_cpoblado=apply(dist_cpoblado,MARGIN = 1,min)
dist_cpoblado=data.frame(dist_cpoblado)
#View(dist_cpoblado)


#Version en una linea, pero pierde legibilidad:
'''
dist_cpoblado=st_distance(mapmuse,c_poblado)%>%data.frame() %>% 
  lapply(., function(x) as.numeric(x))%>%data.frame() %>%
  apply(.,MARGIN = 1,min)%>%data.frame()
'''


#Creamos nuestra tabla de datos y le añadimos las columnas encontradas de distancias
datos=cbind(mapmuse,dist_vias)
datos=cbind(datos,dist_cmedico)
datos=cbind(datos,dist_cpoblado)

#añadimos la columna de fallecidos con 0 y 1
datos= mutate(datos, fallecido=ifelse(estado=='Muerto',1,0))

#Removemos columnas que no son de interés
datos = dplyr::select(datos,-estado,-cod_mpio)

#Observamos los datos
View(datos)


'''
#Verificar:
datosobj=readRDS("data/output/f_mapmuse.rds")

View(datosobj)
'''

#2.1

#Creamos nuestro linear model a partir de los datos que teniamos
ols=lm(fallecido~dist_cmedico+dist_cpoblado+
         dist_vias+as.factor(year)+as.factor(condicion)+
         as.factor(genero)+as.factor(tipo_accidente), data=datos)

#imprimimos el resumen de las observaciones
summary(ols)
#Por presentacion decidi omitir los meses (tampoco son muy relevantes)

#2.2

plot_summs(ols,scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

#2.3

logit = glm(fallecido~dist_cmedico+dist_cpoblado+
              dist_vias+as.factor(year)+as.factor(condicion)+
              as.factor(genero)+as.factor(tipo_accidente),
            family=binomial(link = "logit"), data=datos)
summary(logit)

probit= glm(fallecido~dist_cmedico+dist_cpoblado+
              dist_vias+as.factor(year)+as.factor(condicion)+
              as.factor(genero)+as.factor(tipo_accidente),
            family=binomial(link = "probit"), data=datos)
summary(probit)

#2.4
fitlist= list(ols, probit,logit)
tabla=outreg(fitlist)
View(tabla)

#2.5
#Dado que el estimado es crecano a 0, y la desviación estandar es 0, recrear valores quedó complicado. 
#No hay parece haber manera fácil de crear esta gráfica a no ser que se creara de ceros, pero 
#No tenia nada clara la idea de esta gráfica. 




#--------
#Punto 3
#--------

#definir la dirección objetivo
xml_document=read_html("https://es.wikipedia.org/wiki/Departamentos_de_Colombia")

#delimitamos el texto en una variable
texto= content %>% html_text()

#Recogemos el texto que pertenece al titulo
titulo= title[[1]]%>%substr(., 1, 25)

#imprimimos el titulo
print(titulo)

#delimitamos en nuestra variable tables todas las tablas de nuestro html
tables=content %>% html_table()

#Miramos nuestra tabla de interés
View(tables[[4]])


