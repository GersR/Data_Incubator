library(ggmap)
library(maps)
library(mapproj)
library(rgeos)
library(maptools)
library(rgdal)
library(sp)
library(gpclib)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#Sys.setlocale('LC_ALL','en_US.UTF-8')

datos <- readRDS('datos_imputados.rds')
qplot(datos$lat)
qplot(datos$lon)

latq <- quantile(datos$lat,c(0.38))
lonq <- quantile(datos$lon,c(0.28))

df <- get_map(location = c(lonq,latq),
              maptype = 'roadmap', zoom = 12)
ggmap(df, extent = 'panel') + 
  geom_point(aes(x=lon,y=lat),colour = 'hotpink', alpha = 1,
             size = 2, data = datos)


#leer shapefiles
mun_shp <- readOGR("./Municipios", layer = "Municipios")
shp <- spTransform(mun_shp, CRS("+proj=longlat +datum=WGS84"))
shp@data$id = rownames(shp@data)
df_shp <- subset(shp, CVE_ENT == '09' & CVE_MUN %in% c("014","015","016"))
df_df <- df_shp %>% 
  fortify(region = "id")

ggmap(df, extent = 'panel') + 
  geom_point(aes(x=lon,y=lat),colour = 'hotpink', alpha = 1,
             size = 2, data = datos) + 
  geom_polygon(data = df_df, aes(long, lat, group = group), colour='black',alpha=0) +
  scale_x_continuous(breaks = round(seq(min(datos$lon), max(datos$lon), by = 0.01),2)) +
  scale_y_continuous(breaks = round(seq(min(datos$lat), max(datos$lat), by = 0.01),2)) +
  theme(axis.text.x = element_text(angle = 90))

#Quitamos los que están muy afuera de las delegaciones seleccionadas
datos_2 <- datos %>%
  filter((lon>(-99.27)) & (lon<(-99.12)) & (lat>19.35) & (lat<19.48))

latq <- quantile(datos_2$lat,c(0.38))
lonq <- quantile(datos_2$lon,c(0.28))
df <- get_map(location = c(lonq,latq),
              maptype = 'roadmap', zoom = 12)
ggmap(df, extent = 'panel') + 
  geom_point(aes(x=lon,y=lat),colour = 'hotpink', alpha = 1,
             size = 2, data = datos_2) + 
  geom_polygon(data = df_df, aes(long, lat, group = group), colour='black',alpha=0)

# Cuantiles
quantile(datos_2$precio, prob = seq(0, 1, length = 11), type = 5,na.rm=T)

datos_2 <- datos_2 %>%
  mutate(precio.c=cut(precio,breaks=c(-Inf,2850,9500,12800,15500,18500,21000,25000,
                                      30000,36000,57000,172500,Inf),
                      labels=c("0-2850","2851-9500","9501-12800","12801-15500",
                               "15501-18500","18501-21000","21001-25000","25001-30000",
                               "30001-36000","36001-57000","57001-172500",
                               "Más de 172500")))                     

ggmap(df, extent = 'panel') + 
  geom_point(aes(x=lon,y=lat,colour=precio.c),
             size = 3, data = datos_2) + 
  geom_polygon(data = df_df, aes(long, lat, group = group), colour='black',alpha=0) +
  scale_colour_brewer(palette="RdYlBu")

datos_2$precio4 <- NULL
datos_2 <- datos_2[datos_2$idg!=135,]
datos_2 <- datos_2[datos_2$idg!=306,]


# Es común centrar las coordenadas
# geográficas restando la media.


datos_2 <- datos_2 %>%
  mutate(media_lon = mean(datos$lon),
         media_lat = mean(datos$lat),
         x = lon - mean(lon) + rnorm(1000,1e-10,1e-10), 
         y = lat - mean(lat) + rnorm(1000,1e-10,1e-10),
         precio_log = log(precio))

saveRDS(datos_2,'datos_finales.rds')
