
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
      save(g,file = paste0('res/graph_',y,'.RData'))
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


