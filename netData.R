# laad data voor zorgnet visualisatie


require("igraph", quietly=T)
require("jsonlite", quietly=T)
require("r2d3", quietly=T)
require("d3r", quietly=T)
require("tidyverse", quietly=T)

# laad de data  een bepaald perspectief. Elk perspectief omvat nodes.
#de functie levert een list op van twee dataframes - de nodes en de links.
#als het argument nodeslinks niet null is worden de ingelezen nodes en links toegevoegd 
#aan de meegegeven argumenten zodat er uiteindelijk 1 graaf ontstaat
#
loadNetworkLayer <- function(nodesedges=NULL, name) {
  cat('load layer: ', name, "\n")
  
  nls = fromJSON(paste0("Data/", name, ".json"))
  lnodes=nls$nodes
  ledges=nls$edges 
  
  # lnodes <- read.csv2(paste0("Data/", naam, "-Nodes.csv"), header=T, colClasses="character", sep=";")
  # ledges <- read.csv2(paste0("Data/", naam, "-edges.csv"), header=T, colClasses="character", sep=";")
  
  oln = c()
  nodenames = lnodes$id
  if (!is.null(nodesedges)) {
    oln = nodesedges$nodes 
    nodenames = c(nodenames, oln$id)
  }
#  browser()
  err = FALSE
  n = checkNodesInedges(nodenames, ledges)
  if (length(n) > 0) {
    simpleMessage(cat("Layer ", naam, ":  unknown nodes in edges file: ", unique(n), ". Layer skipped.\n"))
    err = TRUE
  }
  if (err) {
    return(nodesedges)
  }
  
  if (is.null(nodesedges)) {
    return(list(nodes=lnodes, edges=ledges))
  }
  else {
    oedges = nodesedges$edges 
    onodes = nodesedges$nodes 
    rnodes = dplyr::bind_rows(onodes, lnodes)
    redges = dplyr::bind_rows(oedges, ledges)
    return( list(nodes=rnodes, edges=redges) )
  }
}

#check of edges verwijzen naar bekende nodes
#todo: accepteer onbekende nodes, maak er forward references van.
# of doe de checkedges pas aan het eind.
#
checkNodesInedges <- function(nodenames, edges) {
  nofrom = !(edges$from %in% nodenames)
  noto = !(edges$to %in% nodenames)
  c(edges$from[nofrom], edges$to[noto])
}

# Read all layers
readLayers <- function(names) {
  #cat("n = ", namen)
  lnl = NULL
  for (i in names) {
    lnl = loadNetworkLayer(lnl, i)
  }
  lnl
}


# Read the network data and collect all info in a list. -------------------
# The list has a few elements: 
# $nodes - the nodes
# $edges - the edges

readNetworkData <-  function(layers) {
  nls = readLayers(layers)
  return(prepareNetworkDataForBrowsing(nls))
}

#augment network data with attributes for browsing
prepareNetworkDataForBrowsing <- function(nls) {
  #browser()
  # add some columns for graph handling
  nodes = nls$nodes
  nodes$name = nodes$id
  
  edges = nls$edges
  edges$eid = getEidForEdge(edges$from,edges$to, edges$label)
  
  return(list(nodes=nodes, edges=edges))
}

# produce an eid based edge characteristics
getEidForEdge <- function(from, to, label) {
  return(str_c(from,label,to, sep="-"))
}

# Read the network data from a single json file and collect all info in a list. -------------------
# The list has three entries:
# $nodes - the vertices
# $edges - the edges
# $network - the igraph
# $layers - the layers loaded
readNetworkDataFromJSON <-  function(jsonfile) {
  nls = fromJSON(jsonfile)
  return(prepareNetworkDataForBrowsing(nls))
}

#add additional data to an existing network structure. Should be in "raw" format, should also be checked. 
#Avoids rereading all data
combineNetworks <- function(net1, net2) {
  net2names = net2$nodes$name
  net2eids = net2$edges$eid
  nda = subset(net1$nodes, !(net1$nodes$name %in% net2names))
  eda = subset(net1$edges, !(net1$edges$eid %in% net2eids ))
  net1$nodes = dplyr::bind_rows(nda, net2$nodes)
  net1$edges = dplyr::bind_rows(eda, net2$edges)
  return(net1)
}

# Add derived information to the basic network data
#
addDerivedNetworkData <-  function(net) {
  ds = unique(net$nodes$domain)
  ds = ds[ds != ""]
  lt = unique(net$edges$linktype)
  lt = lt[lt != ""]
  nt = unique(net$nodes$nodetype)
  nt = nt[nt != ""]
  
  net$domains = ds
  net$linktypes = lt
  net$nodetypes = nt
  return(net)
}

# make a data frame suitable for presenting in a tableoutput
flattenedDataFrameForTable <- function(df) {
  r = flatten(df)
  # for (c in 1:length(df)) {
  #   if (is.list(r[[c]])) {
  #     r[[c]] = vapply(r[[c]], toString, character(1L))
  #   }
  # }
  return(r)
}



