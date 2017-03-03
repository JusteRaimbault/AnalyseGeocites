
library(dplyr)

setwd(paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites'))

test<-as.tbl(read.csv(file='test.csv',sep=';',stringsAsFactors = FALSE,header=FALSE))

# get year
year = sapply(unlist(test[,2]),function(s){l=strsplit(s,',')[[1]];return(as.numeric(l[length(l)]))})

write.table(data.frame(test$V1,rep("",nrow(test)),year),file='data.csv',sep = ';',quote = FALSE,row.names = FALSE,col.names = FALSE)

