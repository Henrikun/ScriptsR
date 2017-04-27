install.packages("readxl") # CRAN version
devtools::install_github("hadley/readxl") # development version


  library(readxl)

# read_excel reads both xls and xlsx files
dados <- read_excel("data.xlsx")
read_excel("my-new-spreadsheet.xlsx")

# Specify sheet with a number or name
read_excel("my-spreadsheet.xls", sheet = "data")
read_excel("my-spreadsheet.xls", sheet = 2)

# If NAs are represented by something other than blank cells,
# set the na argument
read_excel("my-spreadsheet.xls", na = "NA")


View(dados)


df <- data.frame( Safra = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015), DF = c(2.1, 0,	0,	0,	0.7,	0,	0,	0,	0,	0), GO = c(105.9,	106,	95,	87.4,	162.5,	128.7,	70.3,	83,	52.2,	35.1), MS = c(69,	68.6,	57.2,	55.8,	89.2,	84.6,	68.1,	63.3,	55.3,	48.3), MT = c(783.2, 830.4,	614.2,	583.5,	934.8,	1046.5,	731.3,	1005.9,	921.7,	880.5))

library(ggplot2)

ggplot(dados, aes(IPCA)) + 
  geom_line(aes(y = UN, colour = "Price Index"))

plot1

require(lattice)
xyplot(MT ~ MS + GO, data=df, type = c('l','l'), col = c("blue", "red"), auto.key=T)

require("reshape")
require("ggplot2")

mdf <- melt(df, id="Safra")  # convert to long format
ggplot(mdf, aes(x=Safra, y=Pluma em tonelada, colour=Estados)) +
  geom_line() + 
  theme_bw()

ggplot(df, aes(x = Safra)) + 
  geom_line(aes(y = MT , Estados = "MT")) + 
  geom_line(aes(y = MS), colour = "grey") + 
  ylab(label=" Pluma em tonelads ") + 
  xlab("Safra")


library(dplyr)
library(tidyr)
library(quantmod) # for getSymbols for share price data
library(ggseas)   # for nzbop data

dualplot(x1 = time(datt$Exportation), y1 = datt$Year,
         x2 = time(datt$Importation), y2 = datt$Year, 
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", 
         main = "Price per share of two key firms\n(finishing at same vertical position)",
         ylim.ref = c(nrow(bhp), nrow(fonterra)))
