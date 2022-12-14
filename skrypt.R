#Częsć 2 projektu
library(ggplot2)
nike <- read.csv("nke_de_d.csv")
addidas <- read.csv("ads_de_d.csv")

data_nike <- subset(nike,nike$Data %in% addidas$Data)
data_addidas <- subset(addidas,addidas$Data %in% nike$Data)

data_addidas_closed <- data_addidas$Zamkniecie
data_nike_closed <- data_nike$Zamkniecie

library(ggExtra)
library(mnormt)

logDataNike <- log(data_nike_closed)
logDataAddidas <- log(data_addidas_closed)

log_zwroty_nike <- diff(logDataNike)
log_zwroty_addidas <- diff(logDataAddidas)

df <- data.frame(nike=log_zwroty_nike,addidas=log_zwroty_addidas)
p <- ggplot(df,aes(x=nike,y=addidas))+geom_point()
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

