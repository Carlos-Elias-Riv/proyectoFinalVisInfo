library(dplyr)
library(sf)
library(units)

library(stringr)

crimenes1623 <- read.csv('/Users/cerivera/Downloads/carpetasFGJ.csv')

## vamos a hacer la limpieza qeu aplicó diego valle para que todos los datos
## nos sean iguales a los de él

crimenes1623 <- filter(crimenes1623, categoria %in% c(
  "HOMICIDIO DOLOSO",
  "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO",
  "ROBO A CASA HABITACIÓN CON VIOLENCIA",
  "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA",
  "ROBO A NEGOCIO CON VIOLENCIA",
  "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA",
  "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA",
  "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA",
  "ROBO A REPARTIDOR CON Y SIN VIOLENCIA",
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA",
  "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA",
  "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA",
  "SECUESTRO",
  "VIOLACIÓN"))


crimenes1623 %>%
  group_by(delito, categoria) %>%
  summarise(n = n())

crimenes1623$crime <- crimenes1623$categoria


df <- crimenes1623
df <- df %>% rename(Delito = delito)
df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON|C/V")] <- "ROBO DE VEHÍCULO CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN|S/V")] <- "ROBO DE VEHÍCULO SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A PASAJERO A BORDO DE MICROBUS CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A PASAJERO A BORDO DE MICROBUS SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A REPARTIDOR CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A REPARTIDOR CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A REPARTIDOR SIN VIOLENCIA"

df$crime[str_detect(df$crime,
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <-
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA"
df$crime[str_detect(df$crime,
                    "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <-
  "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA"

df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "CON")] <- "ROBO A TRANSPORTISTA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "C/V")] <- "ROBO A TRANSPORTISTA CON VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "SIN")] <- "ROBO A TRANSPORTISTA SIN VIOLENCIA"
df$crime[str_detect(df$crime, "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA") &
           str_detect(df$Delito, "S/V")] <- "ROBO A TRANSPORTISTA SIN VIOLENCIA"


df$crime <- str_replace_all(df$crime, "CON VIOLENCIA", "C.V.")
df$crime <- str_replace_all(df$crime, "SIN VIOLENCIA", "S.V.")
df$crime <- str_replace_all(df$crime,
                            "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO",
                            "LESIONES POR ARMA DE FUEGO")
df$crime <- str_replace_all(df$crime,
                            "ROBO A PASAJERO A BORDO DEL METRO C.V.",
                            "ROBO A BORDO DE METRO C.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A PASAJERO A BORDO DEL METRO S.V.",
                            "ROBO A BORDO DE METRO S.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A PASAJERO A BORDO DE MICROBUS C.V.",
                            "ROBO A BORDO DE MICROBUS C.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A PASAJERO A BORDO DE MICROBUS S.V.",
                            "ROBO A BORDO DE MICROBUS S.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO C.V.",
                            "ROBO A CUENTAHABIENTE C.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A TRANSEUNTE EN VÍA PÚBLICA C.V.",
                            "ROBO A TRANSEUNTE C.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A TRANSEUNTE EN VÍA PÚBLICA S.V.",
                            "ROBO A TRANSEUNTE S.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO DE VEHÍCULO C.V.",
                            "ROBO DE VEHICULO AUTOMOTOR C.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO DE VEHÍCULO S.V.",
                            "ROBO DE VEHICULO AUTOMOTOR S.V.")
df$crime <- str_replace_all(df$crime,
                            "ROBO A PASAJERO A BORDO DE TAXI C.V.",
                            "ROBO A BORDO DE TAXI C.V.")

crimenes1623 <- df

## todos los datos completos se quedan en df

crimenes1623 <- df[, c('fecha_hecho', 'hora_hecho', 'latitud', 'longitud', 'crime')]
crimenes1623 <- crimenes1623 %>% arrange(fecha_hecho)

## Los datos son desde 1915 :O, pero no todos nos sirven
## vamos a trabajar con los datos de los últimos 20 años

## filtraremos que empiece desde el primero de enero de 

enero03 <- as.Date('2003-01-01')
crimenes0323 <- crimenes1623 %>% filter(fecha_hecho >= enero03)

## finalmente crimenes0323 tiene los crimenes ocurridos en la ciudad de mexico
## en los ultimos 20 años

crimenes0323 <- crimenes0323 %>% rename(date = fecha_hecho, hour = hora_hecho, lat = latitud, long = longitud)

## vamos a quitar los rows donde no hay datos en longitud o latitud

crimenes0323 <- crimenes0323 %>% filter(!is.na(lat))

## quitamos acentos

crimenes0323$crime <- iconv(crimenes0323$crime, from='UTF-8', to='ASCII//TRANSLIT')
crimenes0323$crime <- gsub("VIOLACI'ON", "VIOLACION", crimenes0323$crime)


alcaldias <- st_read("poligonos_alcaldias_cdmx.shp", quiet = T)

alvobj <- alcaldias[alcaldias$NOMGEO %in% c("Álvaro Obregón", "Benito Juárez"), ]

crimenes <- crimenes0323
## una vez que tenemos cargados los polígonos ahora vamos a hacer un join con puntos

## vamos a crear copias de las columnas de las posiciones para no perderlas
## se pierden los datos cuando hacemos drop_geometry
crimenes$long1 <- crimenes$long

crimenes$lat1 <- crimenes$lat

crimes <- st_as_sf(crimenes, coords=c('long', 'lat'), crs=4326)

alcadcrimen <- st_join(crimes, alvobj)

## este nos da solamente aquellos donde aparezca alvaro obregon

sinNaalcad <- alcadcrimen[complete.cases(alcadcrimen$NOMGEO), ]

## volvemos a un dataframe normalito 

alvobcrime <- st_drop_geometry(sinNaalcad) 

## quitamos los nombres especiales a latitud y longitud

alvobcrime$long <- alvobcrime$long1
alvobcrime$lat <- alvobcrime$lat1

## quitamos las columnas que no nos sirven y guardamos como csv

alvobcrime <- subset(alvobcrime, select= -c(CVEGEO, CVE_ENT, CVE_MUN, NOMGEO, long1, lat1))

## guardamos como csv
# write.csv(alvobcrime, "crimenAO.csv")

## ahora veamos una visualizaciones para confirmar que hayamos hecho bien el filtro

library(leaflet)
library(dplyr)

valores_filtrar <- unique(alvobcrime$crime)

dfs_filtrados <- list()

for (valor in valores_filtrar){
  data_filtrado <- alvobcrime[alvobcrime$crime == valor, ]
  dfs_filtrados[[valor]] <- data_filtrado
}

cdmx<- st_read("map.geojson")

vis <- leaflet(cdmx) %>% 
  setView(lng = -99.14162521304014, lat= 19.414010001214344, zoom = 11) %>% 
  addProviderTiles("CartoDB.Voyager")

for (val in valores_filtrar){
  vis <- vis %>% 
    addMarkers(data=dfs_filtrados[[val]], popup = paste0(
      "<strong>Hora: </strong>", dfs_filtrados[[val]]$hour, "<br>",
      "<strong>Fecha: </strong>", dfs_filtrados[[val]]$date, "<br>"), 
      clusterOptions = markerClusterOptions(), group = val
    )
}
vis <- vis %>% 
  addLayersControl(overlayGroups = valores_filtrar, options = layersControlOptions(collapsed = TRUE))

vis
