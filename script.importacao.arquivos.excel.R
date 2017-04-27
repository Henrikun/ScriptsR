
# instalar o pacote
# fazer esse procedimento apenas uma vez
# install.packages("readxl")

# carregar o pacote
library(readxl)

## EXEMPLO
## exemplo de como ler apenas um arquivo de excel
## por padrão a funcao read_excel le apenas a aba 1
#t1 <- read_excel("CANCELADA.xls", na='NA')
#t1

## excluir as linhas que tem NA em todas as celulas
#t1 = t1[!!rowSums(!is.na(t1)),]
#t1
##


# funcao para ler todos os arquivos .xls de pasta de trabalho
arquivos.excel <- list.files(pattern='.xls')

# ter todos os arquivos excel para uma lista, onde cada elemento da lista e' um arquivo do excel que foi importado
lista.excel <- lapply(arquivos.excel, function(x) read_excel(x, na='NA'))

lista.excel.na = list()

# excluir as linhas que tem NA em todas as celulas para cada elemento das lista
for(i in 1:length(lista.excel)){
  lista.excel.na[[i]] = lista.excel[[i]][!!rowSums(!is.na(lista.excel[[i]])),]
} 

arquivos = do.call("rbind", lista.excel.na)

write.csv2(arquivos,file='arquivos.csv')

