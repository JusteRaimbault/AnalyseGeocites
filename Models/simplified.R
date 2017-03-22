
# simplified analysis

# load data


library(dplyr)
library(ggplot2)


setwd(paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites/Models'))
resdir = paste0(Sys.getenv('CS_HOME'),'/Misc/AnalyseGeocites/Results/measures/')

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
ggsave(filename = paste0(resdir,'publi_count.pdf'),width=15,height=10,units = 'cm')


# number of authors
#team_authorship %>% group_by(id_article)%>%summarise(nbauthors = n())
100*length(which(byteam$nbauthors==1))/nrow(byteam)
100*length(which(byteam$nbauthors==1&byteam$paris>0))/sum(byteam$paris)
100*length(which(byteam$nbauthors==1&byteam$cria>0))/sum(byteam$cria)
100*length(which(byteam$nbauthors==1&byteam$ehgo>0))/sum(byteam$ehgo)

# same by year
counts = c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$year==year))/length(which(byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$paris>0&byteam$year==year))/sum(byteam$paris[byteam$year==year]));types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$cria>0&byteam$year==year))/sum(byteam$cria[byteam$year==year]));types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$nbauthors==1&byteam$ehgo>0&byteam$year==year))/sum(byteam$ehgo[byteam$year==year]));types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(singleauthorrate=counts,year=cyears,type=types),aes(x=year,y=singleauthorrate,col=type,group=type))+geom_point()+geom_line()
ggsave(filename = paste0(resdir,'single_author_rate.pdf'),width=15,height=10,units = 'cm')




# intern - extern
#authorship$AuteurDesamb%in%authors$NOM
100*length(which(byteam$internonly!=1))/length(which(byteam$nbauthors>1))
100*length(which(byteam$internonly!=1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
100*length(which(byteam$internonly!=1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
100*length(which(byteam$internonly!=1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))


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
ggsave(filename = paste0(resdir,'external_rate.pdf'),width=15,height=10,units = 'cm')


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
ggsave(filename = paste0(resdir,'publiinternrate.pdf'),width=15,height=10,units = 'cm')



####
# proportion avec doctorants

100*length(which(byteam$withDoctorant==1))/nrow(byteam)
100*length(which(byteam$withDoctorant==1&byteam$paris>0))/sum(byteam$paris)
100*length(which(byteam$withDoctorant==1&byteam$cria>0))/sum(byteam$cria)
100*length(which(byteam$withDoctorant==1&byteam$ehgo>0))/sum(byteam$ehgo)

# doctorants seuls - un auteur
100*length(which(byteam$withDoctorant==1&byteam$nbauthors==1))/nrow(byteam)
100*length(which(byteam$withDoctorant==1&byteam$paris>0&byteam$nbauthors==1))/sum(byteam$paris)
100*length(which(byteam$withDoctorant==1&byteam$cria>0&byteam$nbauthors==1))/sum(byteam$cria)
100*length(which(byteam$withDoctorant==1&byteam$ehgo>0&byteam$nbauthors==1))/sum(byteam$ehgo)

# only doctorants - any authors
100*length(which(byteam$onlyDoctorants))/nrow(byteam)
100*length(which(byteam$onlyDoctorants&byteam$paris>0))/sum(byteam$paris)
100*length(which(byteam$onlyDoctorants&byteam$cria>0))/sum(byteam$cria)
100*length(which(byteam$onlyDoctorants&byteam$ehgo>0))/sum(byteam$ehgo)



counts= c();cyears=c();types=c()
for(year in years){
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$year==year))/length(which(byteam$year==year)));types=append(types,'all');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$paris>0&byteam$year==year))/sum(byteam$paris[byteam$year==year]));types=append(types,'paris');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$cria>0&byteam$year==year))/sum(byteam$cria[byteam$year==year]));types=append(types,'cria');cyears=append(cyears,year)
  counts=append(counts,length(which(byteam$withDoctorant==1&byteam$ehgo>0&byteam$year==year))/sum(byteam$ehgo[byteam$year==year]));types=append(types,'ehgo');cyears=append(cyears,year)
}
ggplot(data.frame(withdoctorantrate=counts,year=cyears,type=types),aes(x=year,y=withdoctorantrate,col=type,group=type))+geom_point()+geom_line()+ylab("Taux publis avec doctorant")
ggsave(filename = paste0(resdir,'taux_doctorant.pdf'),width=15,height=10,units = 'cm')


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
100*length(which(byteam$nbequipes>1&byteam$nbauthors>1))/length(which(byteam$nbauthors>1))
#byteam[byteam$nbequipes==3,]
#data.frame(publis[publis$id==7216,])

100*length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
100*length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
100*length(which(byteam$nbequipes>1&byteam$nbauthors>1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))

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
ggsave(filename = paste0(resdir,'tauxpublisinterequipe.pdf'),width=15,height=10,units = 'cm')




# prop interaxes
100*length(which(byteam$nbaxes>1&byteam$nbauthors>1))/length(which(byteam$nbauthors>1))

100*length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$paris>0))/length(which(byteam$nbauthors>1&byteam$paris>0))
100*length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$cria>0))/length(which(byteam$nbauthors>1&byteam$cria>0))
100*length(which(byteam$nbaxes>1&byteam$nbauthors>1&byteam$ehgo>0))/length(which(byteam$nbauthors>1&byteam$ehgo>0))

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
ggsave(filename = paste0(resdir,'tauxpublisinteraxes.pdf'),width=15,height=10,units = 'cm')




########
### Interaxes collaboration matrix

axes = unique(unlist(sapply(publis$axe[publis$id%in%byteam$id_article],function(s){strsplit(s,',')[[1]]})))
collabmatrix = matrix(0,length(axes),length(axes));rownames(collabmatrix)=axes;colnames(collabmatrix)=axes

for(axestr in publis$axe[publis$id%in%byteam$id_article]){
  if(nchar(axestr)>0){
    currentaxes = strsplit(axestr,',')[[1]];
    if(length(currentaxes)==1){collabmatrix[currentaxes[1],currentaxes[1]]=collabmatrix[currentaxes[1],currentaxes[1]]+1}
    else{for(i1 in 1:length(currentaxes)){for(i2 in 1:length(currentaxes)){collabmatrix[currentaxes[i1],currentaxes[i2]]=collabmatrix[currentaxes[i1],currentaxes[i2]]+1}}}
  }
}

write.table(collabmatrix,file = paste0(resdir,'axescollab.csv'),quote = FALSE,sep = ',',row.names = T,col.names = T)


###
## Modularities

allyears=2012:2016

names = authors$NOM
teamprobas = as.matrix(data.frame(paris = as.numeric(authors$EQUIPE=="P.A.R.I.S"),
                        cria = as.numeric(authors$EQUIPE=="C.R.I.A"),
                        ehgo=as.numeric(authors$EQUIPE=="E.H.GO")))
rownames(teamprobas)<-names

nrepet=50

mods= c();years=c();types=c()
rdmods=c();rdsdmods=c()
for(year in allyears){
  show(year)
  adjacency =coAuthorshipAdj(year)
  axesprobas = authorAxesProbas(year)#authorsProbas(V(g)$name,8,axes,c(year))
  #teamprobas = #authorsProbas(V(g)$name,6,teams,c(year))
  axeadj=adjacency[rownames(axesprobas),rownames(axesprobas)]
  mods=append(mods,overlappingmodularity(axesprobas,axeadj))
  mods=append(mods,overlappingmodularity(teamprobas,adjacency))
  years=append(years,c(year,year));types=append(types,c("axes","equipes"))
  # randomized networks
  ramod=c();rtmod=c()
  for(i in 1:nrepet){
    ramod=append(ramod,overlappingmodularity(axesprobas,axeadj[sample.int(nrow(axeadj)),sample.int(ncol(axeadj))]))
    rtmod=append(rtmod,overlappingmodularity(teamprobas,adjacency[sample.int(nrow(adjacency)),sample.int(ncol(adjacency))]))
  }
  rdmods=append(rdmods,mean(ramod));rdmods=append(rdmods,mean(rtmod))
  rdsdmods=append(rdsdmods,sd(ramod));rdsdmods=append(rdsdmods,sd(rtmod))
}

g=ggplot(data.frame(mod=mods,year=years,type=types,rdmod=rdmods,sdrdmod=rdsdmods))
g+geom_point(aes(x=year,y=mod,colour=type))+geom_line(aes(x=year,y=mod,colour=type,group=type))+
  geom_point(aes(x=year,y=rdmod,colour=type))+geom_line(aes(x=year,y=rdmod,colour=type,group=type),linetype=2)+
  geom_errorbar(aes(x=year,y=rdmod,ymin=rdmod-sdrdmod,ymax=rdmod+sdrdmod,colour=type))
  




