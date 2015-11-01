
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
setwd("~/Documents/Maestria/Multivariada y datos categoricos/Final")

mh<-readRDS("detalle_all_MH.rds")
mh$delegacion<-"Miguel Hidalgo"
mhc<-readRDS("info_rent_MH.rds")
mhc$id<-as.factor(mhc$id)
mh<-inner_join(mh,mhc)

bj<-readRDS("detalle_all_BJ.rds")
bj$delegacion<-"Benito Juárez"
bjc<-readRDS("info_rent_BJ.rds")
bjc$id<-as.factor(bjc$id)
bj<-inner_join(bj,bjc)

ct<-readRDS("detalle_all_CT.rds")
ct$delegacion<-"Cuauhtémoc"
ctc<-readRDS("info_rent_CT.rds")
ctc$id<-as.factor(ctc$id)
ct<-inner_join(ct,ctc)

vc<-readRDS("detalle_all_VC.rds")
vc$delegacion<-"Venustiano Carranza"
vcc<-readRDS("info_rent_VC.rds")
vcc$id<-as.factor(vcc$id)
vc<-inner_join(vc,vcc)

#Limpiamos
all_del<-rbind(mh,bj,ct)
all_del<-filter(all_del,calle!="")
all_del$detalle<-gsub(" +","|",all_del$detalle,perl=TRUE)

#Récamaras
all_del$rec<-as.numeric(substr(all_del$detalle,regexpr("Recámaras",all_del$detalle)+10,regexpr("Recámaras",all_del$detalle)+10))

#Baños
all_del$banios<-as.integer(as.numeric(substr(all_del$detalle,regexpr("Baños",all_del$detalle)+6,regexpr("Baños",all_del$detalle)+6)))

#Medios Baños
all_del$med_banios<-as.integer(as.numeric(substr(all_del$detalle,regexpr("Medios",all_del$detalle)+13,regexpr("Medios",all_del$detalle)+13)))

#Estacionamiento
all_del$est<-as.numeric(substr(all_del$detalle,regexpr("Estacionamiento",all_del$detalle)+16,regexpr("Estacionamiento",all_del$detalle)+16))

#Construcción
aux<-as.integer(regexpr("construcción",all_del$detalle))
aux2<-as.integer(regexpr("m2",all_del$detalle))
all_del$const<-as.numeric(substr(all_del$detalle,aux+12,aux2-2))

#Mantenimiento
aux<-as.integer(regexpr("Mantenimiento:",all_del$detalle))
aux2<-as.integer(regexpr("MXN",all_del$detalle))
all_del$mant<-as.numeric(substr(all_del$detalle,aux+15,aux2-2))

#Manteniiento sin punto
aux<-as.integer(regexpr("Mantenimiento",all_del$detalle))
aux2<-substr(all_del$detalle,aux+14,aux+14)
all_del$mant_incl<-1*(aux2=="I")

#Indicadora de Moneda
all_del$idg <- 1:nrow(all_del)
all_del <- all_del %>%
  mutate(usd = 1*(idg %in% grep("USD",moneda))) 

all_del$precio2 <- as.numeric(all_del$precio)
claves <- data.frame(precio2=1:273,precio3=as.numeric(levels(all_del$precio)))
all_del <- all_del %>%
  left_join(claves,by='precio2')
all_del[all_del$usd==1,'precio4'] <- all_del[all_del$usd==1,'precio3']*0.15
all_del[all_del$usd==0,'precio4'] <- all_del[all_del$usd==0,'precio3']

#Filtros
#Sin recamaras
all_del <- all_del[!is.na(all_del$rec),]
#precios mayores a 200000
qplot(all_del$precio4[all_del$precio4<200000])
all_del <- all_del[all_del$precio4<=200000,]

saveRDS(all_del,"all_del.rds")

#Examinar duplicados
unicos <-  all_del[unique(all_del$direccion),]
repetidos <- anti_join(all_del,unicos,by="idg")
repetidos$calle <- as.character(repetidos$calle)
repetidos <- repetidos[with(repetidos,order(calle)),]

#Andreu
aux <- duplicated(all_del$direccion)
> duplicados <- duplicados %>% arrange(calle)
> View(duplicados)