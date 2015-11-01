library(mi)
library(arm)
library(dplyr)

datos <- readRDS('all_del_final.rds')
datos$id <- 1:nrow(datos)
datos_2 <- datos %>%
  dplyr::select(id,precio,usd,rec,mant_incl,lat,lon,med_banios,est,banios,const)
datos_2$banios <- as.factor(datos_2$banios)
datos_2$med_banios <- as.factor(datos_2$med_banios)
mu_const <- mean(datos_2$const,na.rm = T)
sd_const <- sd(datos_2$const, na.rm = T)
#datos_2$const <- (datos_2$const-mu_const)/sd_const
(info <- mi.info(datos_2))

par(mfrow=c(1,2))
mp.plot(datos_2,clustered=FALSE)
mp.plot(datos_2,x.order=TRUE,y.order=TRUE,clustered=TRUE)

IMP<-mi(datos_2,rand.imp.method = "bootstrap", n.iter = 50)
plot(IMP)

(IMP.bugs1<-bugs.mi(IMP))
plot(IMP.bugs1)
IMP.dat.all <- mi.completed(IMP)
datos_3 <- data.frame(IMP.dat.all)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

datos_3$banios.m <- apply(with(datos_3,cbind(banios,banios.1,banios.2)),1,Mode)
datos_3$const.m <- datos_3$const.2
aux <- as.data.frame(rbind(datos_3$const[datos_3$const.m < 8],datos_3$const.1[datos_3$const.m < 8]))
datos_3$const.m[datos_3$const.m < 8] <- apply(aux,2,max)

datos <- datos %>%
  left_join(datos_3[,c('id','banios.m','const.m')], by = 'id') %>%
  dplyr::select(-id)

saveRDS(datos,'datos_imputados.rds')
