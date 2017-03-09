
library(dplyr)

setwd(paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites/Models'))

test<-as.tbl(read.csv(file='data/raw.csv',sep=';',quote ="",stringsAsFactors = FALSE,header=TRUE))

# get year
year = sapply(unlist(test[,1]),function(s){l=strsplit(s,',')[[1]];res=as.numeric(gsub(" ","",l[length(l)]));if(is.na(res)){show(s)};return(res)})
#titles = gsub("","",test[,2])

# export data for citation network construction
#write.table(data.frame(test$V1,rep("",nrow(test)),year),file='data.csv',sep = ';',quote = FALSE,row.names = FALSE,col.names = FALSE)
write.table(data.frame(test$titre,rep("",nrow(test)),year),file='data/fulldata.csv',sep = ';',quote = FALSE,row.names = FALSE,col.names = FALSE)


# get authors
authors = sapply(unlist(test[,1]),function(s){
  l=strsplit(s,',')[[1]];return(sapply(l[1:(length(l)-1)],function(a){return(gsub(".","",gsub(" ","",a),fixed=TRUE))}))
  })

# umr authors from author file
# -> where is the fucking link table between authors, axes and equipes ?


