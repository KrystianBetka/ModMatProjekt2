#Częsć 2 projektu
library(ggplot2)
nike <- read.csv("nke_de_d.csv")
addidas <- read.csv("ads_de_d.csv")
#nike <- read.csv("11b_d.csv")
#addidas <- read.csv("cdr_d.csv")


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

#-------komentarz
#Marginalny histogram - ułatwia ocenę rozkładu i korelacji pomiędzy dwoma zestawami danych

#estymujemy parametry rozkladu normalnego 
#wektor srednich, macierz kowariancji/korelacji
mu <- colMeans(df) #wektor srednich ˆμ
Sigma <- cov(df)    #estymator nieobciażony macierzy korelacji
P <- cor(df)
mu; 
Sigma;P



s1 <- sqrt(Sigma[1])

s2 <- sqrt(Sigma[4])

x     <- seq(-3*s1, 3*s1, 0.0025) 
y     <- seq(-3*s2, 3*s2, 0.0025)
x;y

f     <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)  
z     <- outer(x, y, f)
persp(x=x, y=y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")



#---------porownujemy wykresy rozrzutu

#generujemy probe z  rozkladu N(mu,Sigma)
n <- nrow(df); n

set.seed(100000)
Z <-MASS::mvrnorm(n,mu=mu,Sigma=Sigma)

#wykresy rozrzutu
par(mfrow=c(1,2))
plot(df, xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))
plot(Z,xlim=c(-0.15,0.15),ylim=c(-0.10,0.10))


#QQ-ploty Mahalanobisa/test chi(2)
dM <- mahalanobis(df,mu,Sigma)
par(mfrow=c(1,1))
hist(dM,prob=TRUE)

n <- dim(df)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(dM,alpha)
q_teo <- qchisq(alpha,df=2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#testujemyhipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)
ks.test(dM,'pchisq',2)

 
#jednoczesnie z testowaniem normalnosci rozkladow brzegowych
#z wykorzystaniem statystyki Andersona-Darlinga
library(MVN)
par(mfrow=c(1,2))
result = mvn(data = df , mvnTest = "mardia",
             univariateTest = "AD", univariatePlot = "qq",
             multivariatePlot = "qq")



##### CZĘŚĆ 3 projektu

#1 PRZEDZIAŁY UFNOŚĆI
#obliczamy przedziały ufnośi na poziomie ufności 95%
# przwdziały ufności - wzór strona 7 wykładu
#NIKE
średnia_nike <- mean(log_zwroty_nike)
liczebność_nike <- length(log_zwroty_nike)
sqrt_nike <- sqrt(length(log_zwroty_nike))
sd_nike <- sd(log_zwroty_nike)

alpha <- 0.05
quantile <- qnorm(1 - alpha/2) #   kwantyl rzędu 1−α/2 rozkładu N(0, 1) 
#nie trzeba wpisywać mean=0,sd=1, poniważ tak jest przyjęte tutaj od razu w tej funckji chyba,że podamy inaczej

#przedział ufności nike dla µ
left_nike <- średnia_nike - quantile * sd_nike/sqrt_nike
right_nike <- średnia_nike + quantile * sd_nike/sqrt_nike


#ADIDAS
średnia_adidas <- mean(log_zwroty_addidas)
liczebność_addidas <- length(log_zwroty_addidas)
sqrt_adidas <- sqrt(length(log_zwroty_addidas))
sd_adidas <- sd(log_zwroty_addidas)

#przedział ufności adidas dla µ
left_adidas <- średnia_adidas - quantile * sd_adidas/sqrt_adidas
right_adidas <- średnia_adidas + quantile * sd_adidas/sqrt_adidas

#2 REGRESJA LINIOWA
#x <- log_zwroty_nike
#y <-log_zwroty_addidas


#estymatory wspolczynnikow
beta1 <- cov(log_zwroty_nike,log_zwroty_addidas)/var(log_zwroty_nike)
beta0 <- mean(log_zwroty_addidas)-mean(log_zwroty_nike)*beta1
beta1; beta0
#beta1 =  0.6535549
#beta0 = -0.0009209255
#wyznacz prostą regresji y =  0.6535549  -0.0009209255 · x,


#linia regresji na  wykresie #y = beta0 + beta1 * x
qplot(log_zwroty_nike, log_zwroty_addidas, data = df,
    ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(colour = "blue", size = 1.5) +
  geom_abline(intercept = beta0, slope = beta1, color="red",size=1)




df <- data.frame(log_zwroty_nike=log_zwroty_nike,log_zwroty_addidas=log_zwroty_addidas)

log_zwroty_lm <- lm(log_zwroty_addidas~log_zwroty_nike,data=df)
log_zwroty_lm
summary <- summary(log_zwroty_lm)
summary
