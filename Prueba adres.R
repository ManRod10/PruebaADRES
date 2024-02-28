###################################################################
###################################################################
#
# Script de Solucion de la prueba
# Manuel Rodriguez
#
###################################################################
###################################################################


# Primero definir una ruta desde donde se tomaron los archivos

#setwd()

# Cargar librerias necesarias 

library(RSQL) 
library(RSQLite) 
library(tidyverse) 
library(readxl)
library(jsonlite)
library(factoextra)
library(NbClust)
library(plotly)
library(htmlwidgets)
library(sf)
library(ggpubr)
library(grid)   
library(patchwork)
library(leaflet)
library(webshot)
###################################################################
# Conexion a SQL
###################################################################

# Como no tengo acceso a un SQL Server
# Se realiza el ejercicio simulando 
# desde R

# Sin embargo la unica diferencia radicaria en el paso de conexion
# En donde deberia utilziar las credenciales del server
# Este seria el Ejemplo

 # con <- DBI::dbConnect(drv = odbc::odbc(),
 #                       Driver = "driver_name",
 #                       Server = "server_url",
 #                       Database = "database_name",
 #                       user = "user", #optional
 #                       password = "password") #optional

##############################################################
# Aqui se realiza la simulacion del SQL server

# Realizamos la conexion a la base de datos, que
# se almacena en la memoria del equipo.

 con <- dbConnect(drv = RSQLite::SQLite(),
                  dbname = ":memory:")
 
# Verificamos que esta funcional y no existan tablas 
 dbListTables(con)
 
 # Cargamos los archivos de excel a R
 mun <- read_excel("Municipios/Municipios.xlsx")
 pres <- read_excel("Prestadores/Prestadores.xlsx")
 
 # Generamos las tablas correspondientes en 
 # nuestro servidor local de SQL
 
 dbWriteTable(conn = con, 
              name = "mun",
              value = mun)
 
 dbWriteTable(conn = con, 
              name = "pres",
              value = pres)
 
 # Examinamos algunos registros de las bases de datos
 dbGetQuery(con,statement = "select * from mun limit 10")
 dbGetQuery(con,statement = "select * from pres limit 10")
 
 
 # Encontramos que hay problemas con los nombres de los municipios 
 # y departamentos en la tabla mun. Pero se observa que el codigo
 # DANE funciona perfectamente
 # Por lo que podemos utilizar la DIVIPOLA para ajustar estas variables
 
 # Traemos la DIVIPOLA
URL<- "https://www.datos.gov.co/resource/gdxc-w37w.json?$limit=2000"
dvpola <- fromJSON(URL)
 
# Se carga la tabla en la base
dbWriteTable(conn = con, 
              name = "dvpola",
              value = dvpola)

# Se generan los querys para combinar ambas tablas y en particular
# se crea una tabla que combine la DIVIOPLA con los datos municipales
dbSendStatement(conn = con,
                       statement = "CREATE TABLE mun_names AS
                       SELECT * from mun left join 
                dvpola on dvpola.cod_mpio == mun.Depmun")

# se eliminan las variables que tenian problemas
dbSendStatement(conn = con,
                statement = "ALTER TABLE mun_names
                            DROP Departamento") 

# se eliminan las variables que tenian problemas
dbSendStatement(conn = con,
                     statement = "ALTER TABLE mun_names
                            DROP Municipio") 

# se observa que el tipo de registros de prestadores tiene 4 categorias
# el analisis se centrara en las IPS, debido a que son las mas organizadas
# de las 4 categorias

dbGetQuery(con, "select clpr_codigo, clpr_nombre , count(clpr_nombre) as val 
           from pres group by clpr_nombre")

# Se realiza una agreacion del numero de prestadores del tipo 4 en cada municipio 

dbGetQuery(con,statement = "select depa_nombre , muni_nombre , count(*) as npres from pres 
           where clpr_codigo == '1' group by depa_nombre, muni_nombre")

# Se genera una nueva tabla que combina esta informacion 
dbSendStatement(con,statement = "create table muni_pres as select
            depa_nombre , muni_nombre , count(*) as npres from pres 
            where clpr_codigo == '1' group by depa_nombre, muni_nombre")

# las bases se envian a R para iniciar el analisis
df0 <- dbGetQuery(conn = con,
                statement = "SELECT * from mun_names")
df1 <- dbGetQuery(conn = con,
                 statement = "SELECT * from muni_pres")

# se examinan los datos nuevamente
View(df0)
# se examinan los datos nuevamente, se encuentra que el departamento esta en
# esta en mayusculas en df0 y en minuscula en df1
View(df1)

# el merge debe realizarse utilizando el nombre del municipio y el departamento
# debido a que existen municipios homonimos

# se ajusta el nombre del departamento para que sean iguales
df1$depa_nombre <- df1$depa_nombre %>% toupper()


# en la revision se encontraron problemas en la variable
# departamento, hay departamentos que estan en una base y en otra no
df1$depa_nombre[!(df1$depa_nombre %in% df0$dpto)] %>% unique()

# tambien hay municipios con el nombre del departamento equivocado
ciudades<-c("BOGOTÁ","CALI","BARRANQUILLA","CARTAGENA","SANTA MARTA", "BUENAVENTURA")
deptos <- c("BOGOTÁ. D.C.", "VALLE DEL CAUCA","ATLÁNTICO","BOLÍVAR","MAGDALENA", "VALLE DEL CAUCA")

for (x in 1:length(ciudades)){
df1$depa_nombre[df1$muni_nombre == ciudades[x]]<-deptos[x]
}

# se ajusta san andres
df1$depa_nombre[df1$depa_nombre == "SAN ANDRÉS Y PROVIDENCIA"] <-
  "ARCHIPIÉLAGO DE SAN ANDRÉS. PROVIDENCIA Y SANTA CATALINA"

for (x in c("depa_nombre","muni_nombre")){
df1[,x]<-chartr("ÁÉÍÓÚ", "AEIOU", toupper(df1[,x]))
}

for (x in c("dpto","nom_mpio")){
  df0[,x]<-chartr("ÁÉÍÓÚ", "AEIOU", toupper(df0[,x]))
}

# se ajusta manualmente una seria de municipios con nombres
# largos o con diferencias entre tablas

df1$muni_nombre[df1$muni_nombre == "BOGOTA"] <-"BOGOTA. D.C."
df1$muni_nombre[df1$muni_nombre == "TUMACO"] <-"SAN ANDRES DE TUMACO"
df1$muni_nombre[df1$muni_nombre == "CARTAGENA"] <-"CARTAGENA DE INDIAS"
df0$nom_mpio[df0$nom_mpio == "ITAGÜI"] <-"ITAGUI"
df1$muni_nombre[df1$muni_nombre == "SANTAFE DE ANTIOQUIA"] <-"SANTA FE DE ANTIOQUIA"
df1$muni_nombre[df1$muni_nombre == "CERRO SAN ANTONIO"] <-"CERRO DE SAN ANTONIO"
df0$nom_mpio[df0$nom_mpio == "CUASPUD CARLOSAMA"] <-"CUASPUD"
df0$nom_mpio[df0$nom_mpio == "SAN JOSE DE CUCUTA"] <-"CUCUTA"
df1$muni_nombre[df1$muni_nombre == "DON MATIAS"] <-"DONMATIAS"
df0$nom_mpio[df0$nom_mpio == "GÜICAN DE LA SIERRA"] <-"GÜICAN"
df0$nom_mpio[df0$nom_mpio == "PUERTO LEGUIZAMO"] <-"LEGUIZAMO"
df0$nom_mpio[df0$nom_mpio == "SAN SEBASTIAN DE MARIQUITA"] <-"MARIQUITA"
df1$muni_nombre[df1$muni_nombre == "MOMPOS"] <-"SANTA CRUZ DE MOMPOX"
df0$nom_mpio[df0$nom_mpio == "PIENDAMO - TUNIA"] <-"PIENDAMO"
df0$nom_mpio[df0$nom_mpio == "PURISIMA DE LA CONCEPCION"] <-"PURISIMA"
df0$nom_mpio[df0$nom_mpio == "SAN ANDRES DE SOTAVENTO"] <-"SAN ANDRES SOTAVENTO"
df0$nom_mpio[df0$nom_mpio == "SAN JUAN DE RIOSECO"] <-"SAN JUAN DE RIO SECO"
df1$muni_nombre[df1$muni_nombre == "SAN VICENTE"] <-"SAN VICENTE FERRER"
df1$muni_nombre[df1$muni_nombre == "SINCE"] <-"SAN LUIS DE SINCE"
df1$muni_nombre[df1$muni_nombre == "TOLU VIEJO"] <-"SAN JOSE DE TOLUVIEJO"

# se verifica cuantas municipios del REPS no 
# estan en la base general
# Se encuentra que unicamente CHIBOLO y GUACHENE 
# estarian faltando. 

a<-df1$muni_nombre %>% unique() 
b<-df0$nom_mpio %>% unique()

a[!(a %in% b)] %>% sort()


# se generan las llaves para hacer el merge
df1$key <- paste(df1$depa_nombre,df1$muni_nombre,sep = "_")
df0$key <- paste(df0$dpto,df0$nom_mpio,sep = "_")

# debido a que df0 tiene mas informacion
# se realiza un left join
df <- merge(df0,df1, by = "key" , all.x = T)


# Se genera la tabla 1. La cual corresponde a un ranking municipal
df <- arrange(df, -npres) 

tab1<-df[1:20,c("depa_nombre","nom_mpio","cod_mpio",
          "Poblacion","Superficie","densidad","npres")]

colnames(tab1)<-c("Departamento","Municipio","COD","Poblacion","Superficie","Densidad",
                  "Número de Prestadores")

# Se guarda el resultado
writexl::write_xlsx(tab1,"Tablas/Tabla1.xlsx")

# Se genera la figura 1A la cual corresponde a un ranking
# departamental 

deptos<-df %>% group_by(depa_nombre,Region) %>% 
  summarise(sum = sum(npres)) %>%
  as.data.frame()

deptos$depa_nombre[deptos$depa_nombre == 
                     "ARCHIPIELAGO DE SAN ANDRES. PROVIDENCIA Y SANTA CATALINA"] <-
  "ARCHIPIELAGO DE SAN ANDRES"

deptos<-deptos %>% filter(!is.na(depa_nombre))


png("Figuras/fig1A.png",width = 1200,height = 800)

ggplot(deptos, aes(x = reorder(depa_nombre,sum) , y = sum, fill = Region)) + 
  geom_bar(stat = "identity") +
 xlab("") + ylab("Número de IPS") +
 coord_flip() + theme_minimal()+
theme(text = element_text(size=25))

dev.off()

### Se inicia el analisis entre densidad poblacional y numero 
### de prestadores
# Se utiliza una regresion tipo loess (locally estimated scatterplot smoothing)
# para captar cualquier tipo de relacion que pueda
# existir entre las variables

  # se generan dos indicadores
df$presper <- df$npres/df$Poblacion
df$densidad <- df$Poblacion/df$Superficie

# se incia con un proceso de visualizacion de la realcion
p1<-ggplot(df,aes(y = npres, densidad, color = Region)) + 
  geom_text(label = df$muni_nombre, size = 8) + theme_minimal()+
  theme(text = element_text(size=20)) + 
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(ylab = "Número de IPS", xlab = "Densidad de Población", 
       title = "Completa")

# se examina quienes estan en los niveles mas bajos
p2<-df %>% filter(npres < 500 & densidad < 5000) %>% 
  ggplot(aes(y = npres, densidad, color = Region)) + 
   geom_text(aes(label = muni_nombre),size = 8)+ theme_minimal()+
  theme(text = element_text(size=20)) + 
  geom_smooth(method = "loess", se = FALSE, color = "blue") + 
  labs(ylab = "Número de IPS", xlab = "Densidad de Población", 
       title = "Densidad < 5000 & Número IPS < 500")

p3<-df %>% filter(npres < 100 & densidad < 2000) %>% 
  ggplot(aes(y = npres, densidad, color = Region)) + 
   geom_text(aes(label = muni_nombre),size = 8)+ theme_minimal()+
  theme(text = element_text(size=20)) + 
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(ylab = "Número de IPS", xlab = "Densidad de Población", 
       title = "Densidad < 2000 & Número IPS < 100")

# Se combinan los 3 graficos en uno solo

p4<-ggarrange(p1+ rremove("ylab") + rremove("xlab"), p2+rremove("ylab") + rremove("xlab"),
          p3+rremove("ylab") + rremove("xlab"), ncol=3, nrow=1, 
          common.legend = TRUE, legend="bottom")

png("Figuras/fig1.png",width = 1500,height = 1000)

annotate_figure(p4, left = textGrob("Número de IPS", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Densidad Poblacional", gp = gpar(cex = 1.3)))


dev.off()

# Se guardan los graficos individuales

png("Figuras/fig2A.png",width = 1200,height = 800)
p1
dev.off()

png("Figuras/fig2B.png",width = 1200,height = 800)
p2
dev.off()

png("Figuras/fig2C.png",width = 1200,height = 800)
p3
dev.off()

# Posteriormente surge la idea de realizar un
# analisis de clusters

# la variable region se tiene que convertir en 
# varias dummies para poderla ingresar adecuadamente
# se utiliza un hot encoding

dummy <- model.matrix(~ Region-1, df)  

# se agregan las variables al df
df<-cbind(df,dummy)

# se realiza un filtro debido a que
# el metodo de cluster no admite NA

df$lpres <- log(df$npres)

dfc<-df %>% filter(!is.na(Superficie) & !is.na(npres)) %>% 
  select(densidad,Irural,lpres,starts_with("RegionRegión")) 

# se utiliza el metodo del codo para identificar 
# el numero de clusters, vemos que este podria ser 2 O 3
fviz_nbclust(dfc, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# seleccionamos el 3
cluster<-kmeans(dfc,centers = 3)

#Se examinan la separacion de los clusters

fviz_cluster(cluster, data = dfc,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#A9B990"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


# se prueba con el 2
cluster<-kmeans(dfc,centers = 2)

#Se examinan la separacion de los clusters

fclus<-fviz_cluster(cluster, data = dfc,
             palette = c("#2E9FDF","#A9B990"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

png("Figuras/fig3.png",width = 1200,height = 800)
fclus
dev.off()

#Se examinan los centroides de cada clusters

cluster$centers

writexl::write_xlsx(as.data.frame(cluster$centers), "Tablas/Tabla2.xlsx")

# Se observa que en los grupos 1 hay municipios con bajos niveles de ruralidad
# y una alta densidad poblacional, el numero de prestadores tambien es alto,
# en el cluster 2 se ubican los municipios predoninantemente rurlaes 
# y con bajas densidades menos de 150 personas por kilometro cuadrado. 

# incluimos variables necesarias para el grafico (muni_nombre)
dfc<-df %>% filter(!is.na(Superficie) & !is.na(npres)) %>% 
  select(Poblacion,Superficie,Irural,npres,lpres,densidad,
         muni_nombre,starts_with("RegionRegión")) 

# agregamos los clusters
df_clust <- cbind(dfc,cluster = as.factor(cluster$cluster))

# generamos el grafico y utilizamos el log para ver de otra forma los datos
# de ajustar la relacion polinomio de segundo orden y 

# posteriormente se utiliza plotly para tener un grafico interactivo
# y mas facil de explorar

figclus<-df_clust %>% 
  ggplot(aes(y = npres, densidad, color = cluster)) + 
  geom_text(aes(label = muni_nombre),size = 8)+ theme_minimal()+
  theme(text = element_text(size=20)) + 
  labs(ylab = "Número de IPS", xlab = "Densidad de Población", 
       title = "Relacion Número IPS vs Densidad")


png("Figuras/fig_clust.png",width = 1200,height = 800)

figclus

dev.off()


fig <- plot_ly(type = "scatter",mode = 'markers',alpha = 2,
  df_clust, y = ~lpres, x = ~densidad,
  text = ~paste("N pres: ", npres, '<br>Densidad:', round(densidad,2),
                '<br>Municipio',muni_nombre ),
  color = ~cluster, size = ~npres,fill = ~densidad
)

fig 

saveWidget(fig, file = "Figuras/fig_log.html")

### Posteriormente se realiza un grafico departamental
# en donde se muestra el numero total de prestadores
# en cada departamento 

# se carga el shapefile
shapefile <- read_sf("Shape/Departamentos.shp")
summary(shapefile)

# se agregan los datos a nivel departamental
deptos<-df %>% group_by(cod_depto,depa_nombre) %>% 
  summarise(sum = sum(npres,na.rm = T)) %>%
  as.data.frame()

# se eliminan algnas filas sin nombre
# de departamento
deptos<-deptos[!is.na(deptos$depa_nombre),]

# se generan las llaves
shapefile$key <- shapefile$DPTO_CCDGO
deptos$key <- deptos$cod_depto

# se realiza el merge
shf <- merge(shapefile,deptos,by= "key",all.x = T)

# se genera el mapa
p6 <- ggplot(data = shf, aes(fill = sum)) +  geom_sf() +
  theme_bw() +
  labs(fill = "Cantidad",
       title = "IPS en Cololmbia",
       subtitle = "Año 2023")+
  theme(text = element_text(size=20)) +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(1, "lines"),
    legend.key.height = unit(100, "lines")
  ))) +  scale_fill_distiller(palette = "Spectral", direction = -1) 


png("Figuras/fig_mapa.png",width = 1200,height = 800)

p6

dev.off()


################################################################################
# Se genera un ejercicio adicional de georeferenciacion de las IPS
################################################################################

# Se realiza un proceso de georeferenciacion para los prestadores
# de la ciudad de Armenia en Quindio 

# Se realiza una solicitud a la base
# para obtener los datos de armania con direcciones
REPS<-dbGetQuery(con,statement = "select * from pres 
           where clpr_codigo == '1' 
           and depa_nombre == 'Quindío' and muni_nombre == 'ARMENIA' ")

# se empieza a generar los valores de la API
query <- "https://maps.googleapis.com/maps/api/geocode/json?address="
key = "AIzaSyBjorJeYWdKTYRI08zJbVRmqyR3rLorZSc" # Se oculta la key de la API debido a esta es de pago.

# Se reemplazan algunos caracteres que generan problemas
REPS$direccion <- gsub(pattern = "#",replacement = "",x = REPS$direccion)
REPS$busqueda <- gsub(pattern = " ",replacement = "+",x = REPS$direccion)

# se genera la URL y se agregan los valores de quindio y armenia para
# facilitar la consulta
URl<-paste(query,REPS$busqueda[1],"+QUINDIO+ARMENIA+","&key=",key,sep ="")

# se realiza la solicitud
jsonfile <- fromJSON(URl)
# se verifica que funciona
jsonX[1] <- list(jsonfile)

# se genera una lista vacia
jsonX <- list()

# se realiza la georeferenciacon y se almacenan los valores
# en la lista anterior
for (i in 1:length(REPS$direccion)) {
  print(i)
  URl<-paste(query,REPS$busqueda[i],"+QUINDIO+ARMENIA+","&key=",key,sep ="")
  print(REPS$nombre_prestador[i])
  jsonfile <- fromJSON(URl)
  jsonX[i] <- list(jsonfile)

}

# se genera un df para almacenar la latiud y longitud
geo<-as.data.frame(matrix(nrow = length(jsonX),ncol = 3,data = 0))

# se agregan los nombres de las variables
colnames(geo)<-c("lat","lng","PRESTADOR")

# se hace uso de la estructura del json para extraer la informacion
# relevante

for (i in 1:length(jsonX)){
  print(i)
  if (jsonX[[i]][["status"]] != "ZERO_RESULTS"){
    geo$lat[i]<-jsonX[[i]][["results"]][["geometry"]][["location"]][["lat"]]
    geo$lng[i]<-jsonX[[i]][["results"]][["geometry"]][["location"]][["lng"]]
  }
  if (jsonX[[i]][["status"]] == "ZERO_RESULTS"){
    geo$lat[i]<-NA
    geo$lng[i]<-NA  
  }
  geo$PRESTADOR[i]<-REPS$nombre_prestador[i]
  
}

# se realiza el mapa
mapa <- leaflet(data = geo) %>%
  addTiles() %>%  # Agregar capa de teselas (tiles) base
  addMarkers(lng = ~lng, lat = ~lat, popup = ~PRESTADOR)  # Agregar marcadores con etiquetas emergentes

# Visualizar el mapa

mapa

# Se guarda como html
saveWidget(mapa, file = "Figuras/REPS_IPS_ARMENIA.html")

# se guarda como PNG
webshot("REPS_IPS_ARMENIA.html", file = "Figuras/fig_ext.png",
        cliprect = "viewport")


## Extensiones con esta informacion se podrian realizar ejercicios adicionales
# tales como 

# 1. Georeferenciacion de los prestadores y evaluar su cobertura en terminos
#    de poblacion (utilizando informacion del Censo Nacional de Poblacion y Vivienda)
# 2. Agregar mas informacion departamentan para generar un modelo mas robusto que permita
# explicar las diferencias en el numero de prestadores, tambien se podrian incluir mas años
# y evaluar como ha sido el crecimieto de esta variable en el tiempo y a que se pueda
# deber que en unas regiones crezca mas que en otras, 
# 3. Un aplicativo para que cualquier usuario, sin conocimientos de programacion
# pueda generar y descargar un boletin para su municipio/departamento de eleccion
