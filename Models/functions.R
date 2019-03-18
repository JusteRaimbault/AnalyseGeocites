
library(rgexf)
library(igraph)



exportGraph <- function(nodesid,linkcolumn,out,type=c("gexf","igraph-year","igraph")){
  # id,label
  nodes = data.frame(id=nodesid,label=nodesid)
  edges=matrix(rep("",2*length(nodesid)^2),nrow =length(nodesid)^2);k=1
  for(a1 in 1:length(nodesid)){show(a1)
    for(a2 in 1:length(nodesid)){edges[k,1]=nodesid[a1];edges[k,2]=nodesid[a2];k=k+1}}
  # edges : id1,id2
  # weights -> by publi number ; or by author ? -> the first is the second but weighted
  counts=list();
  #for(a1 in nodesid){show(a1)
  #  for(a2 in nodesid){for(y in unique(publis$year)){counts[[paste0(a1,a2,y)]]=0}}}
  for(i in 1:nrow(publis)){
    show(i)
    raxes = as.character(publis[i,linkcolumn]);
    currentaxes = strsplit(raxes,',')[[1]]
    if(length(currentaxes)>1){
      for(a1 in currentaxes){for(a2 in currentaxes){
        key=paste0(a1,a2,publis$year[i])
        if(!(key%in%names(counts))){counts[[key]]=0}
        counts[[key]]=counts[[key]]+1
      }}
    }
  }
  
  weights=c();years=c();j=1
  alledges=data.frame()
  for(y in unique(publis$year)){
    show(y)
    j=1;inds=c();currentweights=c()
    for(a1 in nodesid){
      show(a1)
      for(a2 in nodesid){
        #show(counts[[paste0(a1,a2,y)]])
        key=paste0(a1,a2,y)
        if(key%in%names(counts)){
          inds=append(inds,j);currentweights = append(currentweights,counts[[paste0(a1,a2,y)]])
          years=append(years,y)
        }
        j=j+1
      }}
    alledges=rbind(alledges,edges[inds,])
    weights=append(weights,currentweights)
    
    if(type=="igraph-year"){
      g = graph_from_data_frame(cbind(edges[inds,],currentweights),directed = FALSE)
      save(g,file = paste0('res/graph_latest_',y,'.RData'))
    }
    
  }
  
  if(type=="gexf"){
    write.gexf(nodes=nodes,
               edges=alledges,
               edgesWeight=weights,
               edgeDynamic=data.frame(years,years+0.99),
               output=out
    )
  }
}



library(Matrix)

overlappingmodularity <- function(probas,adjacency){#,linkfun=function(p1,p2){return(p1*p2)}){
  show(paste0('Computing overlapping modularity : dim(probas)=',dim(probas)[1],' ',dim(probas)[2],' ; dim(adjacency)=',dim(adjacency)[1],' ',dim(adjacency)[2]))
  m = sum(adjacency)
  n=nrow(probas)
  kout=rowSums(adjacency)
  kin=colSums(adjacency)
  res=0
  for(c in 1:ncol(probas)){
    if(sum(probas[,c])>0){
      if(c%%100==0){show(c/ncol(probas))}
      a1 = Diagonal(x=probas[,c])%*%adjacency%*%Diagonal(x=probas[,c])
      a2 = sum(kout*probas[,c])*sum(kin*probas[,c])*((sum(probas[,c])/n)^2)/m
      res = res + sum(a1) - a2
      rm(a1);gc() # loose time to call gc at each step ?
    }
  }
  return(res/m)
}

directedmodularity<-function(membership,adjacency){
  # sum([A_ij - k_iout k_j in/m ]\delta (c_i,c_j))
  # 
  #deltac = sparseMatrix(1:nrow(adjacency),1:ncol(adjacency),x=0)
  #inds=c()
  #for(c in unique(membership)){inds = append(inds,which(membership==c))}
  m=sum(adjacency)
  kout=rowSums(adjacency);kin=colSums(adjacency)
  res = 0;k=length(unique(membership))
  for(c in unique(membership)){
    #if(c%%100==0){show(c/k)}
    inds=which(membership==c)
    res = res + sum(adjacency[inds,inds]) - sum(kin[inds])*sum(kout[inds])/m 
    gc()
  }
  return(res/m)
}


coAuthorshipAdj <- function(year){
  names = authors$NOM#[authors$EQUIPE%in%c("P.A.R.I.S","C.R.I.A","E.H.GO")]
  adj = matrix(0,length(names),length(names))
  rownames(adj)=names;colnames(adj)=names
  for(pubid in unique(byteam$id_article[byteam$year==year])){
    currentauths = team_authorship$AuteurDesamb[team_authorship$id_article==pubid]
    currentauths=currentauths[currentauths%in%names]
    if(length(currentauths)>1){for(i1 in 1:length(currentauths)){for(i2 in i1:length(currentauths)){if(i1!=i2){adj[currentauths[i1],currentauths[i2]]=adj[currentauths[i1],currentauths[i2]]+1}}}}
  }
  return(adj)
}

authorAxesProbas <- function(year){
  names = authors$NOM
  axes = unique(unlist(sapply(publis$axe[publis$id%in%byteam$id_article],function(s){strsplit(s,',')[[1]]})))
  axesprobas = matrix(0,length(names),length(axes))
  rownames(axesprobas)<-names;colnames(axesprobas)<-axes
  for(row in which(publis$id%in%byteam$id_article)){
    axestr = publis$axe[row]
    if(nchar(axestr)>0){
      currentaxes = strsplit(axestr,',')[[1]];
      currentauths = team_authorship$AuteurDesamb[team_authorship$id_article==publis$id[row]]
      for(auth in currentauths){
        if(auth%in%names){
          for(axe in currentaxes){
            axesprobas[auth,axe]=axesprobas[auth,axe]+1
          }
        }
      }
    }
  }
  axesprobas=axesprobas[rowSums(axesprobas)>0,]
  return(t(apply(axesprobas,1,function(r){r/sum(r)})))
}








