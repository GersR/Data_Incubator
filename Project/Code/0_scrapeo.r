
library(methods)
library(rvest)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(rworldmap)
library(plyr)
Sys.setlocale('LC_ALL','en_US.UTF-8')
etwd("~/Documents/Maestria/Multivariada y datos categoricos/Final")

base_url <-"http://propiedades.com/miguel-hidalgo/departamentos-renta"
pagina<-html(base_url)

#Rentas en Miguel Hidalgo
k<-2
all_rent<-as.character()
all_rent[1]<-"http://propiedades.com/miguel-hidalgo/departamentos-renta?"
for (i in 2:44){
  all_rent[k] <-paste0("http://propiedades.com/miguel-hidalgo/departamentos-renta?pagina=",i)
    k=k+1}

#Páginas y Costos

info_rent<-data.frame()
for (i in 1:44){
  
pagina<-html(all_rent[i])
tabla1 <- pagina %>%
  html_nodes(xpath="//*[@id='inner-listing']/div/div/a")%>%
  html_attr('href')

tabla2 <- pagina %>%
  html_nodes(xpath="//*[@id='inner-listing']/div/div/a/div/div[4]/div[4]")%>%
  html_attr('data-value')

tabla3 <- pagina %>%
  html_nodes(xpath="//*[@id='inner-listing']/div/div/a/div/div[4]/div[4]")%>%
  html_text()
tabla3<-str_trim(gsub("\n","",tabla3))

paginas<-as.data.frame(cbind(tabla1,tabla2,tabla3))
info_rent<-rbind(info_rent,paginas)
}

info_rent$id<-1:nrow(info_rent)
colnames(info_rent)<-c("link","precio","moneda","id")

#Guardamos lo que llevamos
saveRDS(info_rent,"info_rent.rds")
saveRDS(all_rent,"all_rent.rds")

#Información por página

detalle_all<-data.frame()
for (i in 1:nrow(info_rent))
{
pag<-html(as.character(info_rent[i,"link"]))

tabla4 <- pag %>%
  html_nodes(xpath="/html/body/div[2]/div[3]/div[1]/div/div[3]/div[2]/ul")%>%
  html_text()
tabla4<-str_trim(gsub("\n","",tabla4))

tabla5 <- pag %>%
  html_nodes(xpath="/html/head/meta[9]")%>%
  html_attr("content")

tabla6<-pag%>%
  html_nodes(xpath="/html/body/div[2]/div[1]/div/div[2]/span")%>%
  html_text()
tabla6<-str_trim(gsub("\n","",tabla6))

tabla7<-pag%>%
  html_nodes(xpath="/html/body/div[2]/div[3]/div[1]/div[1]/div[1]/h1")%>%
  html_text()
tabla7<-str_trim(gsub("\n","",tabla7))
tabla7<-str_trim(tabla7)

detalle<-as.data.frame(cbind(i,tabla4,tabla5,tabla6,tabla7))
colnames(detalle)<-c("id","detalle","coordenadas","calle","direccion")

detalle_all<-rbind(detalle_all,detalle)
}

