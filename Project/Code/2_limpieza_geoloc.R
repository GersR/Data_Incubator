library(ggmap)
library(dplyr)
Sys.setlocale('LC_ALL','en_US.UTF-8')

#Lectura de datos
all_del<-readRDS('all_del.rds')
all_del$calle <- as.character(all_del$calle)
all_del$direccion <- as.character(all_del$direccion)

#Limpieza de direcciones
#CALLE
all_del <- all_del %>%
  mutate(aux = as.integer(regexpr("Int\\.",all_del$calle, perl=TRUE))) %>%
  mutate(direccion2 = substr(calle,1,regexpr("Int\\.",all_del$calle, perl=TRUE)-1))
all_del[all_del$aux==-1,'direccion2'] <- all_del[all_del$aux==-1,'calle']

all_del$direccion2<-gsub("[\\#\\,\\-\\/].*","",all_del$direccion2,perl=TRUE)
all_del$direccion2<-gsub("\\.","",all_del$direccion2,perl=TRUE)
all_del$direccion2<-gsub("\\(","",all_del$direccion2,perl=TRUE)
all_del$direccion2<-gsub("\\)","",all_del$direccion2,perl=TRUE)
all_del$direccion2<-str_replace_all(all_del$direccion2,"[^[:alnum:]]"," ")
all_del$direccion2<-gsub(" +"," ",all_del$direccion2,perl=TRUE)

#Colonia
aux<-as.numeric(regexpr("Col\\.",all_del$direccion, perl=TRUE))
aux2<-substr(x = all_del$direccion,start = aux, stop = aux + 100)
all_del$colonia<-gsub("[\\,].*","",aux2,perl=TRUE)
all_del$colonia<-substr(x = all_del$colonia,start = 6, stop = 1000)
all_del$colonia<-gsub(" +"," ",all_del$colonia,perl=TRUE)
all_del[all_del$idg==804,]$colonia<-'Del Valle Centro'
all_del[all_del$idg==847,'colonia']<-'Narvarte'

#CP.
aux<-as.numeric(regexpr("CP\\.",all_del$direccion, perl=TRUE))
all_del$cp<-substr(x = all_del$direccion,start = aux + 3, stop = aux + 7)


all_del[all_del$idg==650,'direccion2']<-"Avenida 96"
all_del[all_del$idg==78,'direccion2']<-"Paseo de la Reforma 2233"

all_del <- all_del %>%
  mutate(dir=paste(direccion2,colonia,cp,'Distrito Federal'))

for(i in 1:nrow(all_del)){
  print(i)
  dat <- geocode(as.character(all_del[i,'dir']))
  all_del[i,'lat']<-dat$lat
  all_del[i,'lon']<-dat$lon
}

saveRDS(all_del,"all_del_v2.rds")

#Eliminamos los Departamentos Duplicados.
#Examinar duplicados
aux <- base::duplicated(all_del$direccion)
duplicados <- all_del[aux,] %>% arrange(calle)

calles_duplicadas <- as.character(unique(duplicados$calle))

base_duplicados<-data.frame()

for(i in 1:length(calles_duplicadas)){
  x<-calles_duplicadas[i]
  filtro <- all_del %>% filter(calle==x)
  base_duplicados <-rbind(base_duplicados,filtro)
  base_duplicados
}

seleccionadas <- c(2,11,15,29,44,73,458,497,16,21,60,80,86,101,154,579,102,403,560,103,104,106,240,423,585,
                   113,121,123,165,189,205,352,119,172,304,124,132,379,404,125,126,129,127,216,287,406,
                   128,228,130,222,307,350,131,330,382,400,401,476,606,134,155,243,136,202,137,214,218,
                   139,149,158,317,374,147,385,148,153,293,544,607,193,446,192,219,360,224,226,289,380,
                   227,229,239,241,271,581,286,395,329,343,461,387,468,410,570,441,450,444,460,452,604,
                   474,610,482:488,509,541,559,586,587,589,591,623,682,683,766,687,842,691,697,709,745,
                   720,737,883,721,728,817,851,729,756,816,822,734,735,741,744,797,767,826,776,811,815,
                   782,852,891,900,901,914,956,1097,902,923,903,904,913,919,933,978,937,953,947,971,1063,
                   987,992,1004,1061,1064,1092,979,1117,1001,1016,1098,1005,1144,1006,1142,1010,1026,1106,
                   1031,1130,1132,1048,1049,1094,1050,1062,1088,1055,1057,1058,1110,1163)

base_duplicados_seleccionadas <- base_duplicados %>% filter(idg %in% seleccionadas)

previa <- anti_join(all_del,base_duplicados,by="idg")
previa <- rbind(previa,base_duplicados_seleccionadas)%>%
  select(idg) #1,074 Datos :D


#all_del<-readRDS("all_del_v2.rds")
all_del<-inner_join(all_del,previa,by="idg")


# Ahora analizamos que las coordenadas sean "válidas"
all_del_buenas <- all_del %>%
  filter(lat >= 19.2 & lat < 19.8 & lon < -98)

#Limpiamos y Geolocalizamos de nuevo.

all_del_malas <- anti_join(all_del,all_del_buenas, by = 'idg')

all_del_malas$dir<-gsub("Sección","",all_del_malas$dir,perl=TRUE)
all_del_malas$dir<-gsub(", Dis","",all_del_malas$dir,perl=TRUE)

for(i in 1:nrow(all_del_malas)){
  print(i)
  dat <- geocode(as.character(all_del_malas[i,'dir']))
  all_del_malas[i,'lat']<-dat$lat
  all_del_malas[i,'lon']<-dat$lon
}

all_del_malas_b <- all_del_malas %>%
  filter(lat >= 19.2 & lat < 19.8 & lon < -98)
all_del_malas_m <- anti_join(all_del_malas,all_del_malas_b, by = 'idg')


#Tabla Semifinal
all_del_final<-rbind(all_del_buenas,all_del_malas_b)%>%
  select(idg,delegacion,rec,banios,med_banios,est,const,mant,mant_incl,usd,precio4,dir,lat,lon)

#Homologamos valores.
all_del_final$med_banios[is.na(all_del_final$med_banios)]<-0
all_del_final$est[is.na(all_del_final$est)]<-0
all_del_final$mant[33]<-2000
all_del_final$mant[is.na(all_del_final$mant)]<-0
all_del_final$precio<-all_del_final$precio4+all_del_final$mant

all_del_final$banios[36]<-NA
all_del_final$banios[all_del_final$idg==922]<-NA

saveRDS(all_del_final,"all_del_final.rds")

