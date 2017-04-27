

#1 Baixar série historica do dolar de 5 anos
install.packages("quantmod")
library(quantmod)


datainicial = "2012-03-16"
datafinal = "2017-03-16"
USD_BRL = getFX("USD/BRL", from=datainicial, to=datafinal, env=NULL)
lineChart(USD_BRL)

