library(akima)
library(reshape2)
library(ggplot2)
library(ggmap)
library(geoR)
library(spBayes)
library(dplyr)
library(parallel)

mean_lon <- (-99.18264)
mean_lat <- 19.41507

# Predicciones de kriging
datos <- readRDS('datos_finales.rds')
datos_pred <- readRDS('datos_prediccion.rds')

# Regresion lineal
fma <- precio_log ~ 1 + x + y + rec + banios.m + const.m + est
mod.1 <- lm(formula = fma, data = datos)
summary(mod.1)


# Predicciones de kriging bayesiano
N <- 8100
xo <- seq(min(datos$x), max(datos$x), length = 90)
yo <- seq(min(datos$y), max(datos$y), length = 90)
grid <- akima::interp(x=datos$x, y=datos$y, z=datos$precio_log, 
                      xo = xo, yo = yo,
                      linear = FALSE, extrap = TRUE,
                      duplicate = "median")
grid <- cbind(expand.grid(grid$x, grid$y), c(grid$z))
colnames(grid) <- c('x','y','z')
predcov <- matrix(cbind(rep(1,N),coords$x,coords$y,
                        sample(datos$rec,N,replace=T),
                        sample(datos$banios.m,N,replace=T),
                        rgamma(N,2.62,0.017),
                        sample(datos$est,N,replace=T)),
                  nrow=N,ncol=7)
coords <- grid[,c('x','y')]
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

#Prediccion
pred_grid_bayesiano <- spPredict(model.1, 
                  pred.coords=coords, 
                  pred.covars=predcov,
                  start=500,
                  thin=10)

saveRDS(pred_grid_bayesiano,'pred_grid_bayesiano.rds')

pred_grid_bayesiano <- readRDS('pred_grid_bayesiano.rds')
post_pred_bayesiano <- rowMeans(pred_grid_bayesiano$p.y.predictive.samples)
post_pred_95ci_bayes<-t(apply(pred_grid_bayesiano$p.y.predictive.samples,1,quantile,c(0.25,0.975)))
dat <- data.frame(id=1:N,media=post_pred_bayesiano,post_pred_95ci_bayes)
dat <- cbind(dat,predcov)
colnames(dat)<-c('id','media','lb_bayes','ub_bayes','int','x','y','rec','banios','const','est')
dat$lon <- dat$x + mean_lon
dat$lat <- dat$y + mean_lat


lonq <- median(dat$lon)
latq <- median(dat$lat)

df <- get_map(location = c(lonq,latq),
              maptype = 'roadmap', zoom = 13)
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


# Comparacion de predicciones entre kriging oridinario y
# kriging bayesiano
# Agregamos los intervalos de prediccion de kriging ordinario
dat$pred_ok <- pred_ok_media
dat$lb_ok <- pred_ok_media - 2*sqrt(pred_ok_var)
dat$ub_ok <- pred_ok_media + 2*sqrt(pred_ok_var)

ggplot(dat[sample(1:nrow(dat),100),]) + 
  geom_pointrange(aes(x = id, y=pred_ok, 
                      ymin=lb_ok, ymax=ub_ok), color='red')+
  geom_pointrange(aes(x = id, y=media, 
                      ymin=lb_bayes, ymax=ub_bayes), color='blue') 
  
# Validacion cruzada con kriging bayesiano
# Valores iniciales
beta.ini <- rep(0,5)
sigma2.ini <- 1/rgamma(1,2,1)
tau2.ini <- 1/rgamma(1,2,0.5)
phi.ini <- runif(1,min=0.0008,max=0.06)
estimacion <- function(i){
  train <- datos[datos$id != i,]
  test <- datos[datos$id == i,]
  fma <- train$precio_log ~ train$x + train$y + train$rec + 
    train$banios.m + train$const.m + train$est
  coords <- as.matrix(cbind(train$x,train$y),nrow=999,ncol=2)
  model.1 <- spLM(formula = fma, 
                  coords=coords,starting=list("phi"=phi.ini,"sigma.sq"=sigma2.ini, 
                                              "tau.sq"=tau2.ini),
                  tuning=list("phi"=0.03, "sigma.sq"=0.19, "tau.sq"=0.07),
                  priors=list("phi.Unif"=c(0.0008, 0.06), "sigma.sq.IG"=c(2, 1),
                              "tau.sq.IG"=c(2, 0.5)), cov.model="spherical",
                  n.samples=5000, n.report=10)
  pred <- spPredict(model.1, 
                    pred.coords=matrix(c(train$x,train$y),1,2), 
                    pred.covars=matrix(with(train,c(1,x,y,rec,
                                                    banios.m,const.m,est)),1,7),
                    start=1000,
                    thin=2)
  media <- mean(pred$p.y.predictive.samples)
  lb <- quantile(pred$p.y.predictive.samples,c(0.025))
  ub <- quantile(pred$p.y.predictive.samples,c(0.975))
  data.frame(media_est=media,lb=lb,ub=ub,precio_log=test$precio_log)
}
ids <- unique(datos$id)
set.seed(109057)
muestra<-ids[sample(1:1000,1)]
system.time(
  lista_res <- mclapply(muestra,estimacion,mc.cores=7)
)

pred <- rbind_all(lista_res)
pred$id <- 1
ggplot(pred) + 
  geom_pointrange(aes(x = id, y=media_est, 
                      ymin=lb, ymax=ub), color='red')+
  geom_point(aes(x = id, y=precio_log), color='blue') 
