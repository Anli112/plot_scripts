
library(tidyverse)
setwd('C:/Users/li.pan/Desktop/售后汇总/26年3月/DZOE2025050500_沙雯君老师_有参转录组售后分析_260312/LPS-HG-1-vs-CON/4.GO富集')
data <- read.delim('1.GO_enrichment/LPS-HG-1-vs-CON/enrichment-go-LPS-HG-1-vs-CON-Total.xls',
                   header = T,
                   sep = '\t',
                   check.names = F)
genes <- c('STAT3',
           'ACSL4',
           'GPX4',
           'SLC7A11'  )
df <- function(dt,k){
  j <- c()
  for ( i in 1:length(dt$id)){
   m<- str_split(dt[i,"geneID"],pattern = ';')%>%
      unlist()
   
   if (sum(k%in%m)>0){
     j <- c(j,i)
   }
  }
  return(j)
}
index <- c()
for ( i in 1:length(genes)){

    index <- c(index,df(data,genes[i]))
     index <- unique(index)
}
data <- data[index,]

library(writexl)
write_xlsx(data, "GO_enrichment_results.xlsx")
