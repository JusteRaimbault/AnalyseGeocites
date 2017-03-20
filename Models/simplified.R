
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
            internonly=as.numeric(prod(as.numeric(AuteurDesamb%in%authors$NOM))==1),
            nbauthorsintern = sum(as.numeric(AuteurDesamb%in%authors$NOM)),
            nbauthors = n(),
            nbequipes = length(unique(EQUIPE))
            )

# PB : remove NAs !

# count by teams
sum(byteam$paris)
sum(byteam$cria)
sum(byteam$ehgo)


# number of authors
#team_authorship %>% group_by(id_article)%>%summarise(nbauthors = n())
length(which(byteam$nbauthors==1))/nrow(byteam)
length(which(byteam$nbauthors==1&byteam$paris>0))/sum(byteam$paris)
length(which(byteam$nbauthors==1&byteam$cria>0))/sum(byteam$cria)
length(which(byteam$nbauthors==1&byteam$ehgo>0))/sum(byteam$ehgo)



# intern - extern
#authorship$AuteurDesamb%in%authors$NOM
length(which(byteam$internonly!=1))/length(which(byteam$nbauthors>1))
length(which(byteam$internonly!=1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
length(which(byteam$internonly!=1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
length(which(byteam$internonly!=1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))


# rate intern / nbauthors

rateall = byteam$nbauthorsintern[byteam$nbauthors>1]/byteam$nbauthors[byteam$nbauthors>1]
mean(rateall)
sd(rateall)

rateparis = byteam$nbauthorsintern[byteam$nbauthors>1&byteam$paris>0]/byteam$nbauthors[byteam$nbauthors>1&byteam$paris>0]
mean(rateparis)
sd(rateparis)

ratecria = byteam$nbauthorsintern[byteam$nbauthors>1&byteam$cria>0]/byteam$nbauthors[byteam$nbauthors>1&byteam$cria>0]
mean(ratecria)
sd(ratecria)

ratehgo = byteam$nbauthorsintern[byteam$nbauthors>1&byteam$ehgo>0]/byteam$nbauthors[byteam$nbauthors>1&byteam$ehgo>0]
mean(ratehgo)
sd(ratehgo)


####

# prop interequipes
length(which(byteam$nbequipes>1&byteam$nbauthors>1))/length(which(byteam$nbauthors>1))







