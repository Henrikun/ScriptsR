
# instalar pacote
#install.packages("readxl")

library(xlsx)
library(readxl)

# funcao para ler todas as abas de um arquivo excel
# salvando em dimensoes de uma lista
# fonte: http://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}



# -----------------------
# ideia trabalhar com todos os arquivos do excel de uma vez
list.files(pattern='.xls')

for(i in 1:length(a)){
  
  mysheets <- read_excel_allsheets(a[[1]])

  # continuar com os comandos abaixo dentre deste loop
  
}
# -----------------------





mysheets <- read_excel_allsheets("mg.xlsx")
length(mysheets)

# converter tudo para data.frame; renomear as variaveis; acrescentar variavel ano
for(i in 1:length(mysheets)){
  mysheets[[i]] <- as.data.frame(mysheets[[i]])
  names(mysheets[[i]]) <- c('produto','exportacao')
  mysheets[[i]]['ano'] <- 1999+i
}

# eliminar os espacos antes e depois de cada string
for(i in 1:length(mysheets)){
  txt <- mysheets[[i]][1]
  txt <- gsub("^[[:space:]]+|[[:space:]]+$", "", as.character(texto))
  mysheets[[i]]['produto'] <- txt
}

class(mysheets[[1]])
head(mysheets[[1]])

# converter uma lista em data frame
mydata <- do.call("rbind", mysheets)
row.names(mydata) <- 1:dim(mydata)[1]

str(mydata)
head(mydata)

# obter as frequencias
mydata2 <- as.data.frame(table(mydata$produto))
head(mydata2)

