library(spBayes)
library(dplyr)
library(fields)

datos <- readRDS('datos_finales.rds') %>% 
  dplyr::select(rec,banios.m,const.m,est,precio_log,x,y)

N <- nrow(datos)
coords <- as.matrix(cbind(datos$x,datos$y),nrow=N,ncol=2)
Y <- datos$precio_log

Dist.mat <- rdist(coords)
max(Dist.mat)

# Valores iniciales
beta.ini <- rep(0,5)
sigma2.ini <- 1/rgamma(1,2,1)
tau2.ini <- 1/rgamma(1,2,0.5)
phi.ini <- runif(1,min=0.0008,max=0.06)

# Ajuste del modelo
fma <- Y ~ datos$x + datos$y + datos$rec + datos$banios.m +
  datos$const.m + datos$est
# no correr
model.1 <- spLM(formula = fma, 
                coords=coords,starting=list("phi"=phi.ini,"sigma.sq"=sigma2.ini, "tau.sq"=tau2.ini),
                tuning=list("phi"=0.05, "sigma.sq"=0.1, "tau.sq"=0.05),
                priors=list("phi.Unif"=c(0.0008, 0.06), "sigma.sq.IG"=c(2, 1),
                            "tau.sq.IG"=c(2, 0.5)), cov.model="spherical",
                n.samples=5000, verbose=TRUE, n.report=100)
saveRDS(model.1,'model.rds')

model.1 <- readRDS('model.rds')

names(model.1)
model.1$p.theta.samples[1:5,]

# Graficas y resumen de la distribucion posterior marginal 
# de los parametros de covarianza
plot(model.1$p.theta.samples)
print(summary(model.1$p.theta.samples[1001:5000,]))
print(summary(model.1$p.theta.samples[1001:5000,]))

# Intervalo de 95% de probabilidad para parametros de covarianza
apply(model.1$p.theta.samples[1001:5000,],2,quantile,c(0.025,0.975))

# Recuperar coeficientes de estimacion con burning de 1000
model.1.recov <- spRecover(model.1, start=1000, verbose=TRUE)
saveRDS(model.1.recov, 'model.1.recov.rds')

model.1.recov <- readRDS('model.1.recov.rds')

# Graficas y resumenes de la distribucion marginal posterior
# de los coeficientes beta (parametros de regresion)
plot(model.1.recov$p.beta.recover.samples[,1:3])
plot(model.1.recov$p.beta.recover.samples[,4:5])
summary(model.1.recov$p.beta.recover.samples)

# eta prima 
eta.hat <- t(model.1.recov$p.w.recover.samples)
dim(eta.hat)

model.1.eta.summary <- summary(mcmc(t(model.1.recov$p.w.recover.samples)))$quantiles[,c(3,1,5)]
model.1.eta.summary[1:5,]

# Hacemos la prediccion
# Matriz de covariables
predcov <- matrix(cbind(rep(1,N),datos$x,datos$y,datos$rec,
                        datos$banios.m,datos$const.m,datos$est),
                        nrow=N,ncol=7)
# Utiliza muestreo por composicion con MCMC a partir de 
# la distribucion predictiva a posteriori
# El burning es de 1000 muestras y se hacen predicciones cada
# dos muestras (parametro thin)
pred <- spPredict(model.1, 
                  pred.coords=coords, 
                  pred.covars=predcov,
                  start=1000,
                  thin=2)
saveRDS(pred, 'pred.rds')

pred <- readRDS('pred.rds')
str(pred$p.y.predictive.samples)

## Media posterior de las predicciones. 
post.pred.mean <- rowMeans(pred$p.y.predictive.samples)
post.pred.mean[1:10]


