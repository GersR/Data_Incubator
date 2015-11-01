library(ggplot2)
library(dplyr)
library(ggmap)

datos <- readRDS('datos_finales.rds')
qplot(log(datos$precio))

(ggplot(data=datos)
 + geom_point(aes(x=lon, y=lat, z=precio_log, colour=precio_log, size=precio.c))
 + stat_density2d(aes(x=lon, y=lat, z=precio_log, colour=precio, size=precio.c),n = 500)
)

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
