library(dplyr)
library(ggplot2)
library(akima)
library(gstat)
library(sp)
library(geoR)
library(ggmap)
library(arm)
library(corrplot)

datos_2 <- readRDS('datos_finales.rds')
datos_2$xx<-with(datos_2,x+media_lon)
datos_2$yy<-with(datos_2,y+media_lat)
qmplot(xx, yy, data=datos_2, 
       size=precio.c, alpha=I(0.8), color=precio_log,
       geom=c('point'), source='google')

datos <- datos_2 %>% 
  dplyr::select(rec,banios.m,const.m,est,precio_log,x,y)

M <- cor(datos)
corrplot(M, method = "shade")

# Kriging

# Estimacion de la variacion a gran escala
# Polinomio de tercer orden
fma <- precio_log ~ 1 + x + y + rec + banios.m + const.m + est
mod.1 <- lm(formula = fma, data = datos)
summary(mod.1)

# Estimacion de la variacion de peque?a escala
datos$mu_hat <- mod.1$fitted.values
datos <- mutate(datos, eta_hat = precio_log - mu_hat)

renta_datos <- data.frame(x = datos$x, y = datos$y, eta_hat = datos$eta_hat)
coordinates(renta_datos) = ~ x + y 
emp_variog <- variogram(eta_hat ~ 1, data = renta_datos, width = 0.001)

ggplot(emp_variog, aes(x = dist, y = gamma)) +
  geom_point() + ylim(0, 0.2) +
  labs(title = expression("Semivariograma empi?rico"), 
       x = "distancia", y = "semivarianza")

sph.variog <- function(sigma2, phi, tau2, dist){
  n <- length(dist)
  sph.variog.vec <- rep(0,n)
  for(i in 1:n){
    if(dist[i] < phi){
      sph.variog.vec[i] <- tau2 + (sigma2 * (((3 * dist[i]) / (2 * phi)) - 
                                               ((dist[i] ^ 3)/(2 * (phi ^ 3)))))
    }
    if(dist[i] >= phi){
      sph.variog.vec[i] <- tau2 + sigma2  
    }
  }
  return(sph.variog.vec)
}

sph_variog.w <- fit.variogram(emp_variog, 
                              vgm(psill= 0.05, model="Sph", range = 0.03, nugget = 0.1), 
                              fit.method = 7)
sigma2 <- sph_variog.w$psill[2]
phi <- sph_variog.w$range[2]
tau2 <- sph_variog.w$psill[1]
dist <- sph_variog.w$dist # vector de distancias

dat <- data.frame(x = seq(0, 0.06, 0.0001), y = seq(0, 0.12, 0.0002))
ggplot(dat, aes(x = x, y = y)) +  ylim(0, 0.2) +
  labs(title = expression("Semi-variograma esferico ajustado"),
       x = "distancia", y = "semivarianza") +
  stat_function(fun = sph.variog, args = list(sigma2 = sigma2, phi = phi, tau2 = tau2),
                colour = "green3") + 
  geom_point(data = emp_variog,  aes(x = dist, y = gamma))

# Estimacion de beta utilizando la matriz estimada de covarianzas
trend <- ~ 1 + datos$x + datos$y + datos$rec + 
  datos$banios.m + datos$const.m + datos$est
depas_geo <- as.geodata(obj = datos, coords.col = c(6, 7), data.col = 5)
depas_reml <- likfit(depas_geo, trend = trend, cov.model = "spherical", 
                     ini.cov.pars = c(sigma2, phi), nugget = tau2, 
                     fix.nugget = FALSE, lik.met = "REML")
saveRDS(depas_reml,'depas_reml.rds')

depas_reml <- readRDS('depas_reml.rds')
depas_reml$beta
mod.1$coeff

sigma2.reml <- depas_reml$sigmasq
phi.reml <- depas_reml$phi
tau2.reml <- depas_reml$tausq

#Kriging
kc.control <- krige.control(type.krige = "ok", 
                            trend.d = trend,
                            trend.l = trend, 
                            obj.model=depas_reml,
                            cov.model = "spherical",
                            cov.pars=c(sigma2.reml, phi.reml), 
                            nugget = tau2.reml)
loc <- matrix(c(datos$x,datos$y), nrow=nrow(datos), ncol=2)

#Prediccion
kc.sk.s0 <- krige.conv(depas_geo,
                       locations=loc,
                       krige=kc.control)

datos$ks_precio<-kc.sk.s0$predict

datos$pks_precio.c <- quantileCut(datos$ks_precio,5)

ggplot(datos, aes(x = x, y = y, colour = pks_precio.c)) +
  geom_point(size = 2.3) +
  scale_colour_brewer(palette = "Blues")

mean_lon <--99.18264
mean_lat <- 19.41507

aux <- datos %>% 
  dplyr::select(x, y, ks_precio,pks_precio.c) %>%
  mutate(lon = x + mean_lon, lat= y + mean_lat )

qmplot(lon, lat, data=aux,
       size=pks_precio.c, alpha=I(0.8), color=ks_precio,
       geom=c('point'), source='google')


saveRDS(datos,'datos_prediccion.rds')

#######################################################
# Mi Depa
rec = 3
banios.m = 3
est = 1
x = -99.18737 - mean(datos_2$lon)
y = 19.36801 - mean(datos_2$lat)
const.m = 209
usd = 0
loc.2 <- matrix(c(x),c(y),nrow=1,ncol=2)
kc.control.2 <- krige.control(type.krige = "ok", 
                              trend.d = trend,
                              trend.l = ~ c(x) + c(y) + 
                                c(rec) + c(banios.m) + 
                                c(const.m) + c(est), 
                              obj.model = depas_reml,
                              cov.model = "spherical",
                              cov.pars=c(sigma2.reml, phi.reml), 
                              nugget = tau2.reml)

kc.sk.s2 <- krige.conv(depas_geo,
                       locations=loc.2,
                       krige=kc.control.2)
exp(kc.sk.s2$predict)