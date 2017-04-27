
####
####      Usando o Filtro HP (Hodrick-Prescott)

library(mFilter)
library(quantmod)
library(xts)
getSymbols('GDP',src='FRED')
GDP <- gdp
hpf <- hpfilter(log(GDP), freq = 6400)
hpf <- hpfilter(log(GDP),freq = 1600)
hpf
GDP
#################################################################
##    Para transformar um data frame em um arquivo de base xts

dfx = xts(dados$PIB, order.by=as.Date(dados$DATA))
dfx
#################################################################

out <- xts(cbind(hpf$x, hpf$trend, hpf$cycle), index(dfx))
colnames(out) <- c("x", "trend", "cycle")

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(out[,"x"], t= "n", main = paste(hpf$title, "of", hpf$xname))
lines(out[,"x"], col = "steelblue")
lines(out[,"trend"], col = "red")
legend("bottomright", legend = c(hpf$xname, "trend"), col = c("steelblue", "red"), lty = rep(1, 2), ncol = 2)
plot(out[,"cycle"], t = "n", main = "Cyclical component (deviations from trend)")
lines(out[,"cycle"], col = "steelblue")


##########
##  para extrair o hiato do produto (hPIB)
##  tomar a diferença entre o PIB e o PIB potencial
##  PIB potencial é a tendencia que foi calculado pelo filtro HP
##########

PIBprevisto <- out$x
PIBpotencial <- out$trend
plot(PIBprevisto, main = "PIB Previsto")
plot(PIBpotencial, main = "PIB Potencial")

hPIB <- c(PIBprevisto - PIBpotencial)
hPIB
plot(hPIB)

View(PIBprevisto)
View(PIBpotencial)
View(hPIB)
