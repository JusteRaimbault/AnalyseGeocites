
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

publitypes = c("article_revue","article","ouvrage","chapitre","proceedings","actes_congres_national","dir_ouvrage")
years = 2012:2016

publis = publis[publis$typepubli%in%publitypes,]

# nbr publis
nrow(publis)

team_authorship = left_join(authorship,authors,by=c('AuteurDesamb'='NOM'))
team_authorship$EQUIPE[is.na(team_authorship$EQUIPE)]="EXT"
team_authorship$STATUT[is.na(team_authorship$STATUT)]=666
team_authorship = team_authorship[team_authorship$id_article%in%publis$id&team_authorship$EQUIPE!=""&team_authorship$id_article!=6522,] # 6522 : side effect of removing desjardins ; only one with one ext left alone.
# multi team ?
#duplicated(team_authorship[,1:2])
byteam = team_authorship%>%group_by(id_article)%>%
  summarise(paris=as.numeric(sum(as.numeric("P.A.R.I.S"%in%EQUIPE))>0),
            cria=as.numeric(sum(as.numeric("C.R.I.A"%in%EQUIPE))>0),
            ehgo=as.numeric(sum(as.numeric("E.H.GO"%in%EQUIPE))>0),
            internonly=as.numeric(prod(as.numeric(AuteurDesamb%in%authors$NOM))==1),
            nbauthorsintern = sum(as.numeric(AuteurDesamb%in%authors$NOM)),
            nbauthors = n(),
            nbequipes = length(unique(EQUIPE[EQUIPE!="EXT"])), # ! remove NAs !
            withDoctorant = as.numeric(1%in%STATUT),
            onlyDoctorants = prod(STATUT)==1
            )

# add nb axes
nbaxes = sapply(publis$axe, function(s){length(strsplit(s,split=',')[[1]])})
publis$nbaxes=nbaxes
byteam=left_join(byteam,publis[,c(1,4,9)],by=c('id_article'='id'))


# count by teams
nrow(byteam)
sum(byteam$paris)
sum(byteam$cria)
sum(byteam$ehgo)

# same by year
counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$year==year&byteam$paris>0)));types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$year==year&byteam$cria>0)));types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$year==year&byteam$ehgo>0)));types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(publis=counts,year=cyears,type=types),aes(x=year,y=publis,col=type,group=type))+geom_point()+geom_line()



# number of authors
#team_authorship %>% group_by(id_article)%>%summarise(nbauthors = n())
length(which(byteam$nbauthors==1))/nrow(byteam)
length(which(byteam$nbauthors==1&byteam$paris>0))/sum(byteam$paris)
length(which(byteam$nbauthors==1&byteam$cria>0))/sum(byteam$cria)
length(which(byteam$nbauthors==1&byteam$ehgo>0))/sum(byteam$ehgo)

# same by year
counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$year==year))/length(which(byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$paris>0&byteam$year==year))/sum(byteam$paris[byteam$year==year]));types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$cria>0&byteam$year==year))/sum(byteam$cria[byteam$year==year]));types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$ehgo>0&byteam$year==year))/sum(byteam$ehgo[byteam$year==year]));types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(singleauthorrate=counts,year=cyears,type=types),aes(x=year,y=singleauthorrate,col=type,group=type))+geom_point()+geom_line()




# intern - extern
#authorship$AuteurDesamb%in%authors$NOM
length(which(byteam$internonly!=1))/length(which(byteam$nbauthors>1))
length(which(byteam$internonly!=1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
length(which(byteam$internonly!=1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
length(which(byteam$internonly!=1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))


counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$internonly!=1&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$internonly!=1&byteam$paris>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$paris>0&byteam$year==year))
);types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$internonly!=1&byteam$cria>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$cria>0&byteam$year==year))
);types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$internonly!=1&byteam$ehgo>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$ehgo>0&byteam$year==year))
);types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(externalrate=counts,year=cyears,type=types),aes(x=year,y=externalrate,col=type,group=type))+geom_point()+geom_line()
# NOTE : for ehgo in 2013, 3 publis only with nbauthors > 1


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



rates = c();cyears=c();types=c()
for(year in years){
  allrows=byteam$nbauthors>1&byteam$year==year
  rates=append(rates,byteam$nbauthorsintern[allrows]/byteam$nbauthors[allrows]);types=append(types,rep('all',length(which(allrows))));cyears=append(cyears,rep(year,length(which(allrows))))
  parisrows=byteam$nbauthors>1&byteam$year==year&byteam$paris>0
  rates=append(rates,byteam$nbauthorsintern[parisrows]/byteam$nbauthors[parisrows]);types=append(types,rep('paris',length(which(parisrows))));cyears=append(cyears,rep(year,length(which(parisrows))))
  criarows=byteam$nbauthors>1&byteam$year==year&byteam$cria>0
  rates=append(rates,byteam$nbauthorsintern[criarows]/byteam$nbauthors[criarows]);types=append(types,rep('cria',length(which(criarows))));cyears=append(cyears,rep(year,length(which(criarows))))
  ehgorows=byteam$nbauthors>1&byteam$year==year&byteam$ehgo>0
  rates=append(rates,byteam$nbauthorsintern[ehgorows]/byteam$nbauthors[ehgorows]);types=append(types,rep('ehgo',length(which(ehgorows))));cyears=append(cyears,rep(year,length(which(ehgorows))))
}
ggplot(data.frame(publiinternrate=rates,year=cyears,type=types),aes(x=year,y=publiinternrate,color=type))+geom_point(size=0.5)+stat_smooth()+ylab("Publication internal rate")



####
# proportion avec doctorants

length(which(byteam$withDoctorant==1))/nrow(byteam)
length(which(byteam$withDoctorant==1&byteam$paris>0))/sum(byteam$paris)
length(which(byteam$withDoctorant==1&byteam$cria>0))/sum(byteam$cria)
length(which(byteam$withDoctorant==1&byteam$ehgo>0))/sum(byteam$ehgo)

# doctorants seuls - un auteur
length(which(byteam$withDoctorant==1&byteam$nbauthors==1))/nrow(byteam)
length(which(byteam$withDoctorant==1&byteam$paris>0&byteam$nbauthors==1))/sum(byteam$paris)
length(which(byteam$withDoctorant==1&byteam$cria>0&byteam$nbauthors==1))/sum(byteam$cria)
length(which(byteam$withDoctorant==1&byteam$ehgo>0&byteam$nbauthors==1))/sum(byteam$ehgo)

# only doctorants - any authors
length(which(byteam$onlyDoctorants))/nrow(byteam)
length(which(byteam$onlyDoctorants&byteam$paris>0))/sum(byteam$paris)
length(which(byteam$onlyDoctorants&byteam$cria>0))/sum(byteam$cria)
length(which(byteam$onlyDoctorants&byteam$ehgo>0))/sum(byteam$ehgo)



counts= c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$year==year))/length(which(byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$paris>0&byteam$year==year))/sum(byteam$paris[byteam$year==year]));types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$cria>0&byteam$year==year))/sum(byteam$cria[byteam$year==year]));types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$ehgo>0&byteam$year==year))/sum(byteam$ehgo[byteam$year==year]));types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(withdoctorantrate=counts,year=cyears,type=types),aes(x=year,y=withdoctorantrate,col=type,group=type))+geom_point()+geom_line()+ylab("Taux publis avec doctorant")

# effectif de doctorants par equipes
length(which(authors$STATUT==1))
length(which(authors$STATUT==1))/length(which(authors$MEMBRE==1))

length(which(authors$STATUT==1&authors$EQUIPE=="P.A.R.I.S"))
length(which(authors$STATUT==1&authors$EQUIPE=="P.A.R.I.S"))/length(which(authors$MEMBRE==1&authors$EQUIPE=="P.A.R.I.S"))
length(which(authors$STATUT==1&authors$EQUIPE=="C.R.I.A"))
length(which(authors$STATUT==1&authors$EQUIPE=="C.R.I.A"))/length(which(authors$MEMBRE==1&authors$EQUIPE=="C.R.I.A"))
length(which(authors$STATUT==1&authors$EQUIPE=="E.H.GO"))
length(which(authors$STATUT==1&authors$EQUIPE=="E.H.GO"))/length(which(authors$MEMBRE==1&authors$EQUIPE=="E.H.GO"))



####

# prop interequipes
length(which(byteam$nbequipes>1&byteam$nbauthors>1))/length(which(byteam$nbauthors>1))
#byteam[byteam$nbequipes==3,]
#data.frame(publis[publis$id==7216,])

length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))

# paris \inter cria length(which(byteam$nbauthors>1&byteam$paris>0&byteam$cria>0))
# paris \inter ehgo length(which(byteam$nbauthors>1&byteam$paris>0&byteam$ehgo>0))
# cria \inter ehgo length(which(byteam$nbauthors>1&byteam$cria>0&byteam$ehgo>0))
#length(which(byteam$nbequipes==1&byteam$nbauthors>1&byteam$paris>0))+
#  length(which(byteam$nbequipes==1&byteam$nbauthors>1&byteam$cria>0))+
#  length(which(byteam$nbequipes==1&byteam$nbauthors>1&byteam$ehgo>0))+
#  length(which(byteam$nbauthors>1&byteam$paris>0&byteam$cria>0))+
#  length(which(byteam$nbauthors>1&byteam$paris>0&byteam$ehgo>0))+
#  length(which(byteam$nbauthors>1&byteam$cria>0&byteam$ehgo>0))
#length(which(byteam$nbequipes==2&byteam$nbauthors>1&byteam$paris>0))
#length(which(byteam$nbauthors>1&byteam$paris>0))
#which(byteam$paris+byteam$cria+byteam$ehgo==0)


counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$paris>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$paris>0&byteam$year==year))
);types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$cria>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$cria>0&byteam$year==year))
);types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$ehgo>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$ehgo>0&byteam$year==year))
);types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(interteam=counts,year=cyears,type=types),aes(x=year,y=interteam,col=type,group=type))+geom_point()+geom_line()+ylab("Taux publis interéquipes")




# prop interaxes
length(which(byteam$nbaxes>1&byteam$nbauthors>1))/length(which(byteam$nbauthors>1))

length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))

counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$paris>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$paris>0&byteam$year==year))
  );types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$cria>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$cria>0&byteam$year==year))
  );types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$ehgo>0&byteam$year==year))/length(which(byteam$nbauthors>1&byteam$ehgo>0&byteam$year==year))
  );types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(interaxe=counts,year=cyears,type=types),aes(x=year,y=interaxe,col=type,group=type))+geom_point()+geom_line()+ylab("Taux publis interaxes")



