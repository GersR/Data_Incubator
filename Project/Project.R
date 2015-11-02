lapply(c('dplyr','maps','mapproj','rgeos','maptools','rgdal','sp','ggmap','ggplot2','knitr','akima','gstat','geoR','arm','corrplot','spBayes','fields','lsr','reshape','parallel'), require, character.only=T)
Sys.setlocale('LC_ALL','en_US.utf-8')

datos <- readRDS('datos_finales.rds')

kable(datos %>%
        dplyr::select(dir,rec,banios.m,const.m,est,precio,lon,lat)%>%
        sample_n(20))

datos <- readRDS('datos_finales.rds')

latq <- quantile(datos$lat,c(0.38))
lonq <- quantile(datos$lon,c(0.28))

df <- get_map(location = c(lonq,latq),
              maptype = 'roadmap', zoom = 12)

ggmap(df, extent = 'panel') + 
  geom_point(aes(x=lon,y=lat),colour = 'blue', alpha = 1,
             size = 2, data = datos)

datos <- readRDS('datos_finales.rds')

qmplot(lon, lat, data=datos, 
       size=precio.c, alpha=I(0.8), color=precio_log,
       geom=c('point'), source='google')

fma <- precio_log ~ 1 + x + y + I(x*y) + I(x^2) + I(y^2) + 
  I(x^2*y) + I(x*y^2) + I(x^3) + I(y^3) 
mod.1 <- lm(formula = fma, data = datos)
summary(mod.1)

surf <- akima::interp(x=datos$x, y=datos$y, z=datos$precio_log, 
                      xo = seq(min(datos$x), max(datos$x), length = 50), 
                      yo = seq(min(datos$y), max(datos$y), length = 50),
                      linear = FALSE, extrap = TRUE,
                      duplicate = "median")

surf.r <- cbind(expand.grid(surf$x, surf$y), c(surf$z))
colnames(surf.r) <- c("x", "y", "z")
surf.r$z <- predict(mod.1, surf.r)
bks <- as.numeric(quantile(surf.r$z))
surf.r$pr <- as.factor(cut(surf.r$z,breaks = bks))
ggplot(surf.r, aes(x = x, y = y, z = z)) + 
  geom_tile(aes(fill = pr)) + 
  scale_fill_brewer(palette = "Blues") + 
  stat_contour()

datos_2 <- readRDS('datos_finales.rds')
datos_2$xx<-with(datos_2,x+media_lon)
datos_2$yy<-with(datos_2,y+media_lat)

datos <- datos_2 %>% 
  dplyr::select(rec,banios.m,const.m,est,precio_log,x,y)

fma <- precio_log ~ 1 + x + y + rec + banios.m + const.m + est
mod.1 <- lm(formula = fma, data = datos)
summary(mod.1)

datos$mu_hat <- mod.1$fitted.values
datos <- mutate(datos, eta_hat = precio_log - mu_hat)

renta_datos <- data.frame(x = datos$x, y = datos$y, eta_hat = datos$eta_hat)
coordinates(renta_datos) = ~ x + y 
emp_variog <- variogram(eta_hat ~ 1, data = renta_datos, width = 0.001)

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
  labs(title = expression("Semi-variograma esférico ajustado"),
       x = "distancia", y = "semivarianza") +
  stat_function(fun = sph.variog, args = list(sigma2 = sigma2, phi = phi, tau2 = tau2),
                colour = "green3") + 
  geom_point(data = emp_variog,  aes(x = dist, y = gamma))

trend <- ~ 1 + datos$x + datos$y + datos$rec + 
  datos$banios.m + datos$const.m + datos$est
depas_geo <- as.geodata(obj = datos, coords.col = c(6, 7), data.col = 5)

depas_reml <- readRDS('depas_reml.rds')
kable(depas_reml$beta)
kable(mod.1$coeff)

sigma2.reml <- depas_reml$sigmasq
phi.reml <- depas_reml$phi
tau2.reml <- depas_reml$tausq

kc.control <- krige.control(type.krige = "ok", 
                            trend.d = trend,
                            trend.l = trend, 
                            obj.model=depas_reml,
                            cov.model = "spherical",
                            cov.pars=c(sigma2.reml, phi.reml), 
                            nugget = tau2.reml)
loc <- matrix(c(datos$x,datos$y), nrow=nrow(datos), ncol=2)

kc.sk.s0 <- krige.conv(depas_geo,
                       locations=loc,
                       krige=kc.control)
datos$ks_precio<-kc.sk.s0$predict

datos<- readRDS('datos_prediccion.rds')

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

model.1 <- readRDS('model.rds')

kable(model.1$p.theta.samples[1:5,])

plot(model.1$p.theta.samples)
print(summary(model.1$p.theta.samples[1001:5000,]))
print(summary(model.1$p.theta.samples[1001:5000,]))

apply(model.1$p.theta.samples[1001:5000,],2,quantile,c(0.025,0.975))

model.1.recov <- readRDS('model.1.recov.rds')

# Graficas y resumenes de la distribucion marginal posterior
# de los coeficientes beta (parametros de regresion)
plot(model.1.recov$p.beta.recover.samples[,1:3])
plot(model.1.recov$p.beta.recover.samples[,4:5])
plot(model.1.recov$p.beta.recover.samples[,6:7])
summary(model.1.recov$p.beta.recover.samples)

eta.hat <- t(model.1.recov$p.w.recover.samples)

model.1.eta.summary <- summary(mcmc(t(model.1.recov$p.w.recover.samples)))$quantiles[,c(3,1,5)]
  model.1.eta.summary[1:5,]


pred <- readRDS('pred.rds')
str(pred$p.y.predictive.samples)

 
post.pred.mean <- rowMeans(pred$p.y.predictive.samples)
(post.pred.mean[1:10])

datos <- readRDS('datos_finales.rds')
datos_pred <- readRDS('datos_prediccion.rds')

# Regresion lineal
fma <- precio_log ~ 1 + x + y + rec + banios.m + const.m + est
mod.1 <- lm(formula = fma, data = datos)
summary(mod.1)


N <- 8100
xo <- seq(min(datos$x), max(datos$x), length = 90)
yo <- seq(min(datos$y), max(datos$y), length = 90)
grid <- akima::interp(x=datos$x, y=datos$y, z=datos$precio_log, 
                      xo = xo, yo = yo,
                      linear = FALSE, extrap = TRUE,
                      duplicate = "median")
grid <- cbind(expand.grid(grid$x, grid$y), c(grid$z))
colnames(grid) <- c('x','y','z')
coords <- grid[,c('x','y')]
predcov <- matrix(cbind(rep(1,N),coords$x,coords$y,
                        sample(datos$rec,N,replace=T),
                        sample(datos$banios.m,N,replace=T),
                        rgamma(N,2.62,0.017),
                        sample(datos$est,N,replace=T)),
                  nrow=N,ncol=7)

model.1 <- readRDS('model.rds')
model.0 <- readRDS('depas_reml.rds')
sigma2.reml <- model.0$sigmasq
phi.reml <- model.0$phi
tau2.reml <- model.0$tausq
trend <- ~ 1 + datos$x + datos$y + datos$rec + 
  datos$banios.m + datos$const.m + datos$est
depas_geo <- as.geodata(obj = datos, coords.col = c(20, 21), data.col = 22)
trend_pred <- ~ 1 + coords$x + coords$y + predcov[,4] + 
  predcov[,5] + predcov[,6] + predcov[,7]
kc.control <- krige.control(type.krige = "ok", 
                            trend.d = trend,
                            trend.l = trend_pred, 
                            obj.model = model.0,
                            cov.model = "spherical",
                            cov.pars=c(sigma2.reml, phi.reml), 
                            nugget = tau2.reml)

kc.sk.s0 <- krige.conv(depas_geo,
                       locations=coords,
                       krige=kc.control)
pred_ok_media <- kc.sk.s0$predict
pred_ok_var <- kc.sk.s0$krige.var

N <- 8100
pred_grid_bayesiano <- readRDS('pred_grid_bayesiano.rds')
post_pred_bayesiano <- rowMeans(pred_grid_bayesiano$p.y.predictive.samples)
post_pred_95ci_bayes<-t(apply(pred_grid_bayesiano$p.y.predictive.samples,1,quantile,c(0.25,0.975)))
dat <- data.frame(id=1:N,media=post_pred_bayesiano,post_pred_95ci_bayes)
dat <- cbind(dat,predcov)
colnames(dat)<-c('id','media','lb_bayes','ub_bayes','int','x','y','rec','banios','const','est')
dat$lon <- dat$x + mean_lon
dat$lat <- dat$y + mean_lat
dat$ks_precio<-kc.sk.s0$predict

lonq <- median(dat$lon)
latq <- median(dat$lat)

df <- readRDS('df.rds')
ggmap(df, extent = 'panel') +
  geom_tile(data = dat,aes(x = lon, y = lat, z = media,
                fill = media, alpha=1)) +
  stat_contour(data=dat,aes(z = media),binwidth=10,size=0.5)+ 
  scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", 
                       midpoint = median(dat$media), space = "rgb", guide = "colourbar")

qmplot(lon, lat, data=dat,
       size=media, alpha=I(0.5), color=media,
       geom=c('point'), source='google') +
  scale_color_gradient(low='pink', high='red')


dat$pred_ok <- pred_ok_media
dat$lb_ok <- pred_ok_media - 2*sqrt(pred_ok_var)
dat$ub_ok <- pred_ok_media + 2*sqrt(pred_ok_var)

ggplot(dat[sample(1:nrow(dat),100),]) + 
  geom_pointrange(aes(x = id, y=pred_ok, 
                      ymin=lb_ok, ymax=ub_ok), color='red')+
  geom_pointrange(aes(x = id, y=media, 
                      ymin=lb_bayes, ymax=ub_bayes), color='blue') 

