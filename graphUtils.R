
library(RJSONIO)
library(dplyr)
library(plyr)

# given Json go model, parse to gMCP graph object

goJson2gmcp <- function(goJsonDiagram){
  
  
  jr <- RJSONIO::fromJSON(goJsonDiagram)
  
  nnodes <- length(jr[[2]])
  
  parseNodes <- function(nodeDA){
    if(length(nodeDA$text))
      c(unlist(strsplit(nodeDA$text, "\n")), nodeDA$key)
  }
  
  nodes <- ldply(jr[[2]], .fun = parseNodes)
  names(nodes) <- c("Node","id")
  nodes$n <- 1:nrow(nodes)
  print(nodes)
  
  parseEdges <- function(linkDA){
    unlist(linkDA[c("from", "to", "text")])
  }
  edges <- ldply(jr[[3]], .fun = parseEdges)
  names(edges) <- c("from", "to", "text")
  
  
  edges$fromID <- edges$from
  edges$toID <- edges$to
  
  for(i in 1:nrow(edges)){
    edges$fromID[i] <- nodes$n[match(edges$from[i], nodes$id)]
    edges$toID[i] <- nodes$n[match(edges$to[i], nodes$id)]
  }
  
  #print(edges)
  
  m <- matrix(rep(0,nnodes**2), nrow = nnodes)
  
  for(i in 1:nrow(edges)) {
    row <- as.numeric(edges$fromID[i])
    col <- as.numeric(edges$toID[i])
    m[row,col] <- as.numeric(edges$text[i])
  }
  
  dimnames(m)<- list(nodes$Node, nodes$Node)
  #       
  #       print("edge matrix")
  #       print(m)
  
  
  return(list(nodes=nodes, edges=edges, m=m))
}


ajson <- '{ "class": "go.GraphLinksModel",
  "nodeDataArray": [ 
    {"text":"Primary", "key":-1, "loc":"-261 96"},
    {"text":"Secondary1", "loc":"-421 237", "key":-2},
    {"text":"Secondary2", "loc":"-112 232", "key":-3}
    ],
  "linkDataArray": [ 
    {"from":-1, "to":-2, "text":"0.5", "points":[-244.02567647988656,134.51929528982814,-306.18836582691426,188.35195298885904,-334.3756483655494,215.27652130211462,-352.1281083692315,237.1139192898001]},
    {"from":-1, "to":-3, "text":"0.5", "points":[-194.34277278291884,134.14399658439226,-152.24710104114172,162.955587945603,-112.97517005768951,195.25293961180256,-77.40074367717432,232.13938496391782]},
    {"from":-2, "to":-3, "points":[-315.11588953391004,249.24685319304018,-247.51246799093065,239.97564694294994,-179.80674060694074,238.87652175912345,-111.99742570484233,245.94225552226843], "text":"1"},
    {"from":-3, "to":-2, "points":[-98.00216366733035,270.4107628923533,-198.59326471897478,319.4439393963423,-237.8387929040328,312.08408091658407,-325.05882690103,274.724985348484], "text":"1"}
    ]}'



#goj <- goJson2gmcp(ajson)


# static parse, given gMCP graph object, export to Json go graph link model
gmcpGraph2goJson <- function(fullGraphJSON, gmcpGraphObj, goJson2gmcpObj) {
  
  m <- gmcpGraphObj@m
  rn <- row.names(m)
  
  edgeMatrix <- NULL
  
  for(i in 1:length(rn)){
    for( j in 1:length(rn)){
      if(m[i,j]>0){
        r <- c("fromName"=rn[i], "toName"=rn[j], "text"=m[i,j])
        edgeMatrix <- rbind(edgeMatrix, r)
      }
    }
  }
  
  #print(edgeMatrix)
  #print(goJson2gmcpObj$nodes)
  
  newGraphJSON <- fullGraphJSON
  
  if(sum(m)>0){
    #edgeMatrix
    newEdge <- data.frame(edgeMatrix)%>%left_join(goJson2gmcpObj$nodes, by=c("fromName"="Node"))%>%
      rename(from=id, fromID=n)%>%left_join(goJson2gmcpObj$nodes, by=c("toName"="Node"))%>%rename(to=id, toID=n)%>%
      mutate(text=as.character(text))
    #newEdge
    
    
    goJson2gmcpObj$edges$index <- 1:nrow(goJson2gmcpObj$edges)
    goJson2gmcpObj$edges$key <- paste0(goJson2gmcpObj$edges$from, goJson2gmcpObj$edges$to)
    
    newEdge$key <- paste0(newEdge$from, newEdge$to)
    newEdge2 <- goJson2gmcpObj$edges%>%select(index,from, to, key)%>%right_join(newEdge%>%select(key, text), by="key")
    
    edge2 <- fullGraphJSON[[3]][newEdge2$index]
    
    for(i in 1:nrow(newEdge2)){
      edge2[[i]]$text <- newEdge2$text[i]
    }
    
    #edge2
    
    
    newGraphJSON[[3]] <- edge2
    
  } else if (sum(m)==0){
    newGraphJSON[[3]] <- ""
  }
  
  noquote(toJSON(newGraphJSON))   
  
}

#gmcpGraph2goJson(jr, ggg[[2]], goj)
#gmcpGraph2goJson(one, two[[3]], three)