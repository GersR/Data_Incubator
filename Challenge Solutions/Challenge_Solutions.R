library(dplyr)
library(ggplot2)
options(digits=10)

#Pregunta 1

#Supuestos 
#El turista nunca decide quedarse en el mismo lugar (es decir, siempre está en movimiento)
#El turista se mueve sin ningún tipo de plan (es decir, no tiene ningún tipo de preferencia)
#El turista siempre elige intersecciones

#Solución

cm<-function(n=100000){
  caminata <- data.frame(x=rep(0,n),y=rep(0,n))
  indice <- matrix(c(2:n,sample(1:2,n-1,T)),ncol=2)
  caminata[indice]<-sample(c(-1, 1), n-1, T)
  caminata[,1] <- cumsum(caminata[,1]) 
  caminata[,2] <- cumsum(caminata[,2]) 
  
  return(caminata)
}

#What is the probability that the tourist is at least 3 city blocks (as the crow flies) 
#from Broadway and Broadway after 10 moves?

p1<-function(){
  l <- cm(10)[10,]
  return(ifelse(sqrt(sum(l^2))>=3,1,0))
}

sum(sapply(1:10000,function(i){p1()}))/10000

#What is the probability that the tourist is at least 10 city blocks 
#(as the crow flies) from Broadway and Broadway after 60 moves?

p2<-function(){
  l <- cm(60)[60,]
  return(ifelse(sqrt(sum(l^2))>=10,1,0))
  return(n)
}

sum(sapply(1:10000,function(i){p2()}))/10000

# What is the probability that the tourist is ever at least 5 city blocks (as the crow flies) 
# from Broadway and Broadway within 10 moves?
p3<-function(){
  df <- cm(10) %>%
    mutate(u=ifelse(sqrt(x^2+y^2)>=5,1,0))
  return(ifelse(sum(df$u==1)!=0,1,0))
}

sum(sapply(1:10000,function(i){p3()}))/10000

# What is the probability that the tourist is ever at least 10 city blocks (as the crow flies)
# from Broadway and Broadway within 60 moves?
p4<-function(){
  df <- cm(60) %>%
    mutate(u=ifelse(sqrt(x^2+y^2)>=10,1,0))
  return(ifelse(sum(df$u==1)!=0,1,0))
}

sum(sapply(1:10000,function(i){p4()}))/10000

# What is the probability that the tourist is ever east of East 1st Avenue but ends up west 
# of West 1st Avenue in 10 moves?

p5<-function(){
  df <- cm(10) %>%
    mutate(l=ifelse(x>=1 & y<=-1,1,0))
  
  return(ifelse(any(df$l==1) & df[10,"x"]<=-1 & df[10,"y"]<=-1,1,0))
}

sum(sapply(1:10000,function(i){p5()}))/10000

# What is the probability that the tourist is ever east of East 1st Avenue but ends up west of
# West 1st Avenue in 30 moves?

p6<-function(){
  df <- cm(30) %>%
    mutate(l=ifelse(x>=1 & y<=-1,1,0))
  
  return(ifelse(any(df$l==1) & df[30,"x"]<=-1 & df[30,"y"]<=-1,1,0))
}

sum(sapply(1:10000,function(i){p6()}))/10000

# What is the average number of moves until the first time the tourist is at least 10 city 
# blocks (as the crow flies) from Broadway and Broadway.

p7<-function(){
  df <- cm(1000) %>%
    mutate(l=ifelse(sqrt(x^2+y^2)>=10,1,0))
  
  indices <- which(df$l!=0)
  return(min(indices))
}

sum(sapply(1:10000,function(i){p7()}))/10000


# What is the average number of moves until the first time the tourist is at least 60 city 
# blocks (as the crow flies) from Broadway and Broadway

p8<-function(){
  df <- cm(10000) %>%
    mutate(l=ifelse(sqrt(x^2+y^2)>=60,1,0))
  
  indices <- which(df$l!=0)
  
  if(length(indices)==0){
    p8()
  }else{
    return(min(indices))
  }
}

sum(sapply(1:10000,function(i){p8()}))/10000

graf_caminata<-function(n=100000){
  df<-cm(n)
  mx <- max(max(abs(df$x)),max(abs(df$y)))
  ggplot(df,aes(x,y)) +
    geom_path() +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    xlim(c(-mx,mx)) +
    ylim(c(-mx,mx)) +
    xlab("") +
    ylab("")
}



graf_caminata<-function(n=100000){
  df<-cm(n)
  
  ggplot(df,aes(x,y)) +
    geom_path() +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    xlim(c(-mx,mx)) +
    ylim(c(-mx,mx)) +
    xlab("") +
    ylab("")
}

#Número de pasos
n<-10000
#Periodo de calentamiento
b<-.1*n

#Siempre comenzamos en el (0,0)
caminata<-data.frame(u_x=rep(0,n+1),u_y=rep(0,n+1))

for(i in 1:n){
  #Movimiento sobre el eje x
  m_x<-runif(1)
  if(m_x<=.3){#Izquierda
    x<--1
  }else{#Mantiene
    if(m_x>.3 && m_x<=.6){
      x<-0
    }else{#Derecha
      x<-1
    }
  }
  #Actualizamos la ubicación del eje x
  caminata[i+1,1] <- caminata[i,1] + x
  
  #Movimiento sobre el eje y
  m_y <- runif(1)
  if(x== 1 | x== -1){
    if(m_y<=.3){#Abajo
      y <- -1
    }else{#Mantiene
      if(m_y>.3 && m_y<=.6){
        y <- 0
      }else{#Arriba
        y <- 1
      }
    }
  }else{#Arriba o abajo con proba .5
    ifelse(m_y<=.5,y <- -1,y <- 1)
  }
  
  #Actualizamos la ubicación del eje y
  caminata[i+1,2] <- caminata[i,2] + y
}

ggplot(caminata,aes(x=u_x,y=u_y)) +
  geom_point() 

#Pregunta 2

df<-read.csv("data/muestra.csv",header=T,stringsAsFactors = F)

#What fraction of complaints are associated with the 2nd most popular agency?
apariciones <- data.frame(table(df$Agency.Name)) %>%
  mutate(Porc=Freq/sum(Freq)) %>%
  arrange(desc(Porc))

#Respuesta
# New York City Police Department 0.207736

#What is the most 'surprising' complaint type when conditioned on a borough? 
#That is, what is the largest ratio of the conditional probability of a complaint type 
#given a specified borough divided by the unconditioned probability of that complaint type?

borough<- df %>%
  group_by(Borough,Complaint.Type) %>%
  summarise(counts=n()) %>%
  group_by(Complaint.Type) %>%
  mutate(complaint_count=sum(counts),
         complaint_ratio=counts/complaint_count,
         complaint_max=max(complaint_ratio)) %>%
  group_by(Borough) %>%
  mutate(borough_count=sum(counts),
         borough_ratio=counts/borough_count,
         borough_max=max(borough_ratio),
         score=borough_ratio*complaint_ratio,
         score_max=max(score)) %>%
  filter(score == score_max)

#Respuesta
#QUEENS Blocked Driveway 0.4166136676

#What is the distance (in degrees) between the 90% and 10% percentiles of degrees latitude?
#Respuesta
quantile(df$Latitude,0.9,na.rm = T) - quantile(df$Latitude,0.1,na.rm = T) 

#Let's estimate the area that 311 supports. Suppose calls are 2D normally distributed on the
#surface of the earth with mean and standard deviation given by those of the latitude and 
#longitude. How many square kilometers is the single-standard-deviation ellipse?

df2<-df %>%
  filter(!is.na(Latitude))

area <- (pi/nrow(df2))*sqrt(sum(df2$Latitude^2)*
                              sum(df2$Longitude^2)-
                              (sum(df2$Latitude*df2$Longitude))^2)

#What is the difference between the expected number of calls received during the most and 
#least popular whole hours of the day? (Remove points which do not seem to accurately 
#reflect the actual time they were reported.)
head(df$Created.Date)

aux <- df %>%
  mutate(hora=paste(substr(Created.Date,12,13),substr(Created.Date,21,22),sep="_"),
         fecha=(substr(Created.Date,1,10))) %>%
  group_by(fecha,hora) %>%
  summarise(cuenta=n()) %>%
  group_by(hora) %>%
  mutate(cuenta_hora=sum(cuenta)) %>%
  ungroup() %>%
  filter(cuenta_hora == max(cuenta_hora) | cuenta_hora == min(cuenta_hora)) %>%
  group_by(hora) %>%
  mutate(esperado=round(sum(cuenta)/n(),digits=0))
#Respuesta
(aux[2,"esperado"] - aux[1,"esperado"]) 

# What is the standard deviation in seconds of the time between consecutive calls? 
# (Remove points which do not seem to accurately reflect the actual time they were reported.)

fechas <- df %>%
  mutate(previous_call=as.POSIXct(strptime(Created.Date, "%m/%d/%Y %I:%M:%S %p"))) %>%
  select(previous_call) %>%
  filter(!is.na(.)) %>%
  arrange(previous_call)

fechas$next_call<-c(fechas$previous_call[-1],fechas$previous_call[1])
fechas$difference<-as.numeric(fechas$next_call-fechas$previous_call)

#Respuesta
sd(fechas$difference[-nrow(fechas)])
