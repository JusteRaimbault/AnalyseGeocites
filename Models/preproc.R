
library(dplyr)
library(stringdist)

setwd(paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites/Models'))

raw<-as.tbl(read.csv(file='data/raw.csv',sep=';',quote ="",stringsAsFactors = FALSE,header=TRUE))

# get year
year = sapply(test$surtitre,function(s){l=strsplit(s,',')[[1]];res=as.numeric(gsub(" ","",l[length(l)]));if(is.na(res)){show(s)};return(res)})
#titles = gsub("","",test[,2])

# export data for citation network construction
#write.table(data.frame(test$V1,rep("",nrow(test)),year),file='data.csv',sep = ';',quote = FALSE,row.names = FALSE,col.names = FALSE)
write.table(data.frame(raw$titre,rep("",nrow(raw)),year),file='data/citation/fulldata.csv',sep = ';',quote = FALSE,row.names = FALSE,col.names = FALSE)


delstrings<-function(todel,s){
  res=s
  for(del in todel){
    res=gsub(del,"",res,fixed=TRUE)
  }
  return(res)
}

# get authors
authorsid = sapply(raw$surtitre,function(s){
  l=strsplit(s,',')[[1]];
  simpl = sapply(l[1:(length(l)-1)],function(a){return(delstrings(c("(",")","COORD","Dir","dir","ed.","."," "),a))})
  inds = sapply(simpl,function(s){nchar(s)>3})
  return(simpl[inds])
}) 
   
authors = sapply(raw$surtitre,function(s){
  l=strsplit(s,',')[[1]];
  simpl = sapply(l[1:(length(l)-1)],function(a){return(trimws(delstrings(c("(",")","COORD","Dir","dir"),a),which="both"))})
  inds = sapply(simpl,function(s){nchar(s)>3})
  return(simpl[inds])
}) 

# umr authors from author file
# ->link table between authors, axes and equipes in the same tab.
# 

# links
links = as.tbl(read.csv(file='data/links.csv',sep=";",stringsAsFactors = FALSE,header = TRUE))

# properties
properties = as.tbl(read.csv(file='data/properties.csv',sep=";",stringsAsFactors = FALSE,header = TRUE))

#

equipe = c();thematic=c();axe=c();typepubli=c()
for(id in raw$id){
  show(id)
  currentlinks = links[links$id_article==id,3];
  currentproperties = properties[properties$id%in%currentlinks$id_mot,]
  #show(currentproperties)
  equipe = append(equipe,paste0(currentproperties$nameshort[currentproperties$type=='equipe'],collapse = ","))
  thematic = append(thematic,paste0(currentproperties$nameshort[currentproperties$type=='thematique'],collapse = ","))
  axe = append(axe,paste0(currentproperties$nameshort[currentproperties$type=='axe'],collapse = ","))
  typepubli = append(typepubli,paste0(currentproperties$nameshort[currentproperties$type=='typepubli'],collapse = ","))
}

collauthors = sapply(authorsid,function(s){paste0(s,collapse=',')})


# write data
write.table(data.frame(id=raw$id,titre=raw$titre,auteurs=collauthors,year=year,typepubli=typepubli,equipe=equipe,thematic=thematic,axe=axe),file='data/publications.csv',sep=";",quote = FALSE,row.names = FALSE,col.names = TRUE)


## auteurs table

authorsid = unique(unlist(authorsid))
authorsbase = unlist(authors)

umrids = sapply(properties$nameshort[properties$type=="author"],function(s){gsub(" ","",gsub(".","",s,fixed=TRUE))})
umrnames = sapply(properties$name[properties$type=="author"],trimws)
umrshortnames = properties$nameshort[properties$type=="author"]


externals = setdiff(authorsid,umrids)

fullnames=c()
for(id in authorsid){
  fullnames=append(fullnames,authorsbase[amatch(id,authorsbase,maxDist=nchar(id)-2)])
}
names(fullnames)<-authorsid
#length(fullnames[externals])

# get status
status <- as.tbl(read.csv(file='data/status.csv',sep=';',quote ="",stringsAsFactors = FALSE,header=TRUE))
#left_join(status,data.frame(MembreUMR=umrnames,id=umrids,stringsAsFactors = FALSE))
#status$id = sapply(status$MembreUMR,function(s){gsub(" ","",gsub(".","",s,fixed=TRUE))})
#jstatus = semi_join(status,data.frame(MembreUMR=umrshortnames,id=umrids),by="MembreUMR")
#length(which(!is.na(jstatus$Statut)))
#nopubli=c("HAULE S.","DIDELON C.","ZIOLKOWSKA J.","ERKAN D.","CHASSET P.-O.","GOURDON P.","HERCULE C.",
#          "LASSAUBE U","HAYAT F.","WANG P.","REISER C.","MIGOZZI J.","LOSAVIO C.","GENEAU I.","PREVELAKIS G.",
#          "JACQUET R.","JEAN-JACQUES  S.","LASSAUBE U.","LE ROUZIC V.","MONDAIN M.","METGE M.","CAILLOL D.",
#          "VILA VAZQUEZ J.","NAVARO A.","AUGISEAU V.","RIGOLLET N.","SAADIA S.","LI M.")
#setdiff(status$MembreUMR[which(sapply(status$MembreUMR,function(s){!(s%in%umrshortnames)}))],nopubli)
nstatus = status$Statut
names(nstatus)<-status$MembreUMR

write.table(data.frame(id=c(umrids,externals),fullname=c(umrnames,fullnames[externals]),umr=c(rep(1,length(umrids)),rep(0,length(externals))),status=c(nstatus[umrshortnames],rep(NA,length(externals)))),file='data/authors.csv',sep=";",quote = FALSE,row.names = FALSE,col.names = TRUE)











