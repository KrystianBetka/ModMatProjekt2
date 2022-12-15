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
#dzienne log zwroty
logDataNike <- log(data_nike_closed)
logDataAddidas <- log(data_addidas_closed)

log_zwroty_nike <- diff(logDataNike)
log_zwroty_addidas <- diff(logDataAddidas)


#wykres rozrzutu z histogramami rozkladow brzegowych
df <- data.frame(nike=log_zwroty_nike,addidas=log_zwroty_addidas)
p <- ggplot(df,aes(x=nike,y=addidas))+geom_point()
ggMarginal(p,type="histogram")


#estymujemy parametry rozkladu normalnego 
#wektor srednich, macierz kowariancji/korelacji
mu <- colMeans(df)
Sigma <- cov(df)    #estymator nieobciażony macierzy korelacji
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



#---------porownujemy wykresy rozrzutu

#generujemy probe z  rozkladu N(mu,Sigma)
n <- nrow(df); n

set.seed(100)
Z <-MASS::mvrnorm(n,mu=mu,Sigma=Sigma)

#wykresy rozrzutu
par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))


#QQ-ploty Mahalanobisa/test chi(2)
dM <- mahalanobis(df,mu,Sigma)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#testujemyhipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)
ks.test(dM,'pchisq',2)


#dodatkowe ???? czy punkt 3???
#jednoczesnie z testowaniem normalnosci rozkladow brzegowych
#z wykorzystaniem statystyki Andersona-Darlinga
library(MVN)
par(mfrow=c(1,2))
result = mvn(data = df , mvnTest = "mardia",
             univariateTest = "AD", univariatePlot = "qq",
             multivariatePlot = "qq")
