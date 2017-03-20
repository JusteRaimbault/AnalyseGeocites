
# simplified analysis

# load data


library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites/Models'))


# authors
authors <- as.tbl(read.csv('data/authors_desamb.csv',sep=";",stringsAsFactors = FALSE))
# publications
publis <- as.tbl(read.csv('data/publications.csv',sep=";",stringsAsFactors = FALSE))
# authorship
authorship <- as.tbl(read.csv('data/authorship.csv',sep=";",stringsAsFactors = FALSE))
authorship = authorship[sapply(authorship$AuteurDesamb,nchar)>0,]

publitypes = c("article_revue","article","ouvrage","chapitre","proceedings","actes_congres_national")

publis = publis[publis$typepubli%in%publitypes,]

# nbr publis
nrow(publis)

team_authorship = left_join(authorship,authors,by=c('AuteurDesamb'='NOM'))
team_authorship = team_authorship[team_authorship$id_article%in%publis$id,]
# multi team ?
#duplicated(team_authorship[,1:2])
byteam = team_authorship%>%group_by(id_article)%>%
  summarise(paris=as.numeric(sum(as.numeric("P.A.R.I.S"%in%EQUIPE))>0),
            cria=as.numeric(sum(as.numeric("C.R.I.A"%in%EQUIPE))>0),
            ehgo=as.numeric(sum(as.numeric("E.H.GO"%in%EQUIPE))>0),
            nbauthors = n()
            )
sum(byteam$paris)
sum(byteam$cria)
sum(byteam$ehgo)


# number of authors
#team_authorship %>% group_by(id_article)%>%summarise(nbauthors = n())
length(which(byteam$nbauthors==1))/nrow(byteam)
length(which(byteam$nbauthors==1&byteam$paris>0))/sum(byteam$paris)
length(which(byteam$nbauthors==1&byteam$cria>0))/sum(byteam$cria)
length(which(byteam$nbauthors==1&byteam$ehgo>0))/sum(byteam$ehgo)



# intern/extern
authorship$AuteurDesamb%in%authors$NOM



