#Częsć 2 projektu
library(ggplot2)
wawel <- read.csv("C:/Users/kryst/InformatykaPraktyczna/sem3/ModMat/Projekt2/nke_de_d.csv")
zywiec <- read.csv("C:/Users/kryst/InformatykaPraktyczna/sem3/ModMat/Projekt2/ads_de_d.csv")

datawawel <- subset(wawel,wawel$Data %in% zywiec$Data)
datazywiec <- subset(zywiec,zywiec$Data %in% wawel$Data)

datazywiec_closed <- datazywiec$Zamkniecie
datawawel_closed <- datawawel$Zamkniecie

library(ggExtra)
library(mnormt)

logDataWawel <- log(datawawel_closed)
logDataZywiec <- log(datazywiec_closed)

log_zwrotyWawel <- diff(logDataWawel)
log_zwrotyZywiec <- diff(logDataZywiec)

df <- data.frame(wawel=log_zwrotyWawel,zywiec=log_zwrotyZywiec)
p <- ggplot(df,aes(x=wawel,y=zywiec))+geom_point()
ggMarginal(p,type="histogram")

mu <- colMeans(df)
Sigma <- cov(df)
P <- cor(df)
mu; Sigma;P



s1 <- sqrt(Sigma[1])

s2 <- sqrt(Sigma[4])

x     <- seq(-3*s1, 3*s1, 0.1) 
y     <- seq(-3*s2, 3*s2, 0.1)
x;y

f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)
persp(x=x, y=y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")
