##  install.packages("readxl") # CRAN version, para instalar o pacote.
##  devtools::install_github("hadley/readxl") # versão de desenvolvimento.

##    Carregando o Pacote "readxl"
library(readxl)
# A planilha "dados" contém as variáveis: IPCA, PIB e UN. 
# read_excel, lê arquivos xls e xlsx.
dados <- read_excel("data.xlsx")

##    Criando as variáveis para o modelo.
price <- dados$IPCA # Índice de Preços, usado para inflação.
gdp <- dados$PIB  # Produto Interno Bruto da Economia.
un <- dados$UNP   # Taxa de Desemprego do País.

price.ts <- ts(price,start=c(2002,1), frequency = 12)
price.ts  # para mostrar a série temporal.

gdp.ts <- ts(gdp,start=c(2002,1),frequency=12)
gdp.ts   # para mostrar a série temporal.

un.ts <- ts(un, start = c(2002,1), frequency = 12)
un.ts   # para mostrar a série temporal.

frequency = 12   ## significa que existem 12,
## observações (12 meses) por unidade de tempo (ano).
## Para plotar as séries temporais:

plot(gdp.ts, ylab="Brazil Gross Domestic Product (GDP)", frame.plot=FALSE)
plot(un.ts,ylab="Brazil Uneployment Rate",frame.plot=FALSE)
plot(price.ts, ylab = "Broad National Consumer Price Index (IPCA)",
     frame.plot = FALSE)

## para plotar uma série de tempo com aplicação de logaritmos naturais.

loggdp <- log(gdp.ts) # Criando uma variável contendo o log do PIB.
plot(loggdp, ylab="Brazil Gross Domestic Product in Log (logGDP)",
     frame.plot=FALSE)


## Para calcular e traçar uma tendência de tempo linear:

plot(gdp.ts, ylab="Gross Domestic Product (GDP)", frame.plot=FALSE)
fitted.time.trend <- lm(gdp.ts ~ time(gdp.ts))
abline(fitted.time.trend, lty="dashed")


## Para calcular e traçar uma tendência de tempo linear usando serie com log:
plot(loggdp, ylab="Gross Domestic Product in Log (lnGDP)", frame.plot=FALSE)
fitted.time.trend <- lm(loggdp ~ time(loggdp))
abline(fitted.time.trend, lty="dashed")
##  lty stands for “line type” (in this case, a dashed line). 
##  To display the coefficients and other statistics of the time trend use:

summary(fitted.time.trend)

##  To lag the time series by one period (in this case, one year) do:
lag(gdp.ts, k = -1)

##  To obtain the quarter-on-quarter growth rate of GDP do:
(gdp.ts-lag(gdp.ts,k=-1))/lag(gdp.ts,k=-1)

##  To obtain the year-on-year growth rate of quarterly GDP do:
(gdp.ts-lag(gdp.ts,k=-12))/lag(gdp.ts,k=-12)  

##  Multiply by 100 to obtain percentage growth rates.
##  R can also work with multivariate time series. The relationship between
##  unemployment and inflation is known as the Phillips curve. 
##  The following data are annual time series of the unemployment rate and 
##  the inflation rate for a country starting in 1993:


#   unemployment <- c(10.6,9.5,8.2,8.2,8.3,7.7,6.9,6.3,6.8,6.4,6.1)
#   inflation    <- c( 1.4,0.8,1.5,2.2,1.5,0.2,0.5,4.3,3.8,2.9,2.8)

##  Create the time series variables:

inflation <- price.ts
unemployment <- un.ts

inflation.ts    <- ts(inflation   ,start=c(2002,1),frequency=12)
unemployment.ts <- ts(unemployment,start=c(2002,1),frequency=12)

inflation.ts
unemployment.ts

##  Plotting both series in a single diagram makes sense because 
##  they are measured on the same scale and use the same units (percent). 
##  To plot both series over time in a single time series diagram do:

ts.plot(inflation.ts,unemployment.ts,lty=c(1:2),frame.plot=FALSE)
legend("top",legend=c("Inflation","Unemployment"),lty=c(1:2))


##  The argument lty=c(1:2) uses two different line types for the two series; 
##  to use different colors do:

ts.plot(inflation.ts,unemployment.ts, col=c(1:2),frame.plot=FALSE)
legend("topright",legend=c("Inflation","Unemployment"),lty=1,col=c(1:2))

##  You can explore the relationship between two time series by making a 
##  scatter plot and drawing the line of best fit. The command

plot(unemployment.ts,inflation.ts,frame.plot=FALSE)

##  will generate a scatter plot with points labeled by time and connected 
##  by lines showing the time path. To obtain a plain vanilla scatter 
##  plot (with points indicating the observations, no time labels, no time path), 
##  use:

plot(unemployment.ts, inflation.ts, frame.plot=FALSE, xy.labels=FALSE) 
##  (I omitted the xlab and ylab arguments for clarity.) To add the line 
##  of best fit

##  and compute the coefficient of correlation, do:
Phillips.curve.fitted <- lm(inflation.ts ~ unemployment.ts)
abline(Phillips.curve.fitted)
# unemployment.ts <- as.numeric(unemployment.ts)
# inflation.ts <- as.numeric(inflation.ts)

cor(inflation.ts, unemployment.ts, use="pairwise.complete.obs", method="pearson")
  
change.in.inflation.ts <- inflation.ts - lag(inflation.ts, k = -1)
change.in.inflation.ts

data.Phillips <- ts.union(change.in.inflation.ts,unemployment.ts,dframe=TRUE)
data.Phillips

data.Phillips$change.in.inflation.ts
data.Phillips$unemployment.ts

augmented.Phillips.curve.fitted <- lm(data.Phillips$change.in.inflation.ts ~ data.Phillips$unemployment.ts)
summary(augmented.Phillips.curve.fitted)
cor(data.Phillips$unemployment.ts,data.Phillips$change.in.inflation.ts,
    use="pairwise.complete.obs")
plot(data.Phillips$unemployment.ts,data.Phillips$change.in.inflation.ts,
     xy.labels=FALSE)
abline(augmented.Phillips.curve.fitted)



############################

Q  <-  seq(0,150)
# Then define the supply and demand functions:
  Ps <-  -(10/40) + (1/40)*Q
Pd <-  (100/20) - (1/20)*Q

plot(Q,Ps,xlab="Quantity (kg)", ylab="Price (euros per kg)",type="l")

lines(Q,Pd)

curve(-(10/40)+(1/40)*x,0,150, xlab="Quantity (kg)", ylab="Price (euros per kg)")
curve((100/20)-(1/20)*x,add=TRUE) # adds the demand curve

text(130,2.6,"Supply")
text(120,0,"Demand")

points(70,1.5)
segments(70, 0  , 70, 1.5, lty="dashed", col="grey") # vertical
segments( 0, 1.5, 70, 1.5, lty="dashed", col="grey") # horizontal

abline(a = 120/20, b = -1/20, lty = "dashed")
curve(120/20-(1/20)*x, lty = "dashed", add=TRUE)



  