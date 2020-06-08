# laad data voor zorgnet visualisatie


require("igraph", quietly=T)
require("jsonlite", quietly=T)

# laad de data  een bepaald perspectief. Elk perspectief omvat nodes.
#de functie levert een list op van twee dataframes - de nodes en de links.
#als het argument nodeslinks niet null is worden de ingelezen nodes en links toegevoegd 
#aan de meegegeven argumenten zodat er uiteindelijk 1 graaf ontstaat
#
loadNetworkLayer <- function(nodeslinks=NULL, naam) {
  cat('load layer: ', naam, "\n")
  
  nls = fromJSON(paste0("Data/", naam, ".json"))
  lnodes=nls$nodes
  llinks=nls$links
  

  # lnodes <- read.csv2(paste0("Data/", naam, "-Nodes.csv"), header=T, colClasses="character", sep=";")
  # llinks <- read.csv2(paste0("Data/", naam, "-Links.csv"), header=T, colClasses="character", sep=";")
 
  oln = c()
  nodenames = lnodes$id
  if (!is.null(nodeslinks)) {
    oln = nodeslinks$nodes 
    nodenames = c(nodenames, oln$id)
  }
 # browser()
  dupn = oln$id[oln$id %in% lnodes$id]
  err = FALSE
  if (length(dupn) > 0) {
    simpleMessage(cat("Warning - Layer ", naam, ":  duplicate node in nodes file: ", unique(dupn), ".\n"))
    err = FALSE
  }
  n = checkNodesInLinks(nodenames, llinks)
  if (length(n) > 0) {
    simpleMessage(cat("Layer ", naam, ":  unknown nodes in links file: ", unique(n), ". Layer skipped.\n"))
    err = TRUE
  }
  if (err) {
    return(nodeslinks)
  }
  
  if (is.null(nodeslinks)) {
    return(list(nodes=lnodes, links=llinks))
  }
  else {
    olinks = nodeslinks$links 
    onodes = nodeslinks$nodes 
    rnodes = dplyr::bind_rows(onodes, lnodes)
    rlinks = dplyr::bind_rows(olinks, llinks)
    return( list(nodes=rnodes, links=rlinks) )
  }
}

#check of links verwijzen naar bekende nodes
#todo: accepteer onbekende nodes, maak er forward references van.
# of doe de checklinks pas aan het eind.
#
checkNodesInLinks <- function(nodenames, links) {
  nofrom = !(links$from %in% nodenames)
  noto = !(links$to %in% nodenames)
  c(links$from[nofrom], links$to[noto])
}

# Lees alle aangegeven perspectieven. Produceer een lijst bestaande uit nodes en links voor de graaf.
readLayers <- function(namen) {
  #cat("n = ", namen)
  lnl = NULL
  for (i in namen) {
    lnl = loadNetworkLayer(lnl, naam=i)
  }
  lnl
}


# Read the network data and collect all info in a list. -------------------
# The list has three entries:
# $nodes - the vertices
# $links - the edges
# $network - the igraph
# $layers - the layers loaded
readNetworkData <-  function(layers) {
  nls = readLayers(layers)
  #browser()
  rawnodes = nls$nodes
  rawlinks = nls$links
  # add some columns for graph handling
  nodes = nls$nodes
  nodes$name = nodes$id
  
  links = nls$links
  links$van = links$from
  links$naar = links$to
  
  network = graph_from_data_frame(d=links, vertices=nodes, directed=T)
  list(rawnodes = rawnodes, rawlinks = rawlinks, nodes=nodes, links=links, network=network, layers=layers)
}



# Read the network data from a single json file and collect all info in a list. -------------------
# The list has three entries:
# $nodes - the vertices
# $links - the edges
# $network - the igraph
# $layers - the layers loaded
readNetworkDataFromJSON <-  function(jsonfile) {
  nls = fromJSON(jsonfile)
  #browser()
  rawnodes = nls$nodes
  rawlinks = nls$links
  # add some columns for graph handling
  nodes = nls$nodes
  nodes$name = nodes$id
  
  links = nls$links
  links$van = links$from
  links$naar = links$to
  network = graph_from_data_frame(d=links, vertices=nodes, directed=T)
  list(rawnodes = rawnodes, rawlinks = rawlinks, nodes=nodes, links=links, network=network)
}

#add additional data to an existing network structure. Should be in "raw" format, should also be checked. 
#Avoids rereading all data
addAdditionalData <- function(netinfo, additionaldata) {
  addrawnodes = additionaldata$nodes
  addrawlinks = additionaldata$links
  
  #allow new colums through the use of bind_rows. New node defs override existing
  netinfo$rawnodes = netinfo$rawnodes[!(netinfo$rawnodes$id %in% addrawnodes$id),]
  netinfo$rawnodes= dplyr::bind_rows(netinfo$rawnodes, addrawnodes)
  netinfo$rawlinks = dplyr::bind_rows(netinfo$rawlinks, addrawlinks)
  #Note: rawlinks may contain duplicate edges
  #So: remove them. Unclear which is removed (should be the older)
  netinfo$rawlinks = unique(netinfo$rawlinks)
  
  addnodes = additionaldata$nodes
  addlinks = additionaldata$links

  addnodes$name = addnodes$id
  addlinks$van = addlinks$from
  addlinks$naar = addlinks$to
  netinfo$nodes = dplyr::bind_rows(netinfo$nodes[!(netinfo$nodes$name %in% addnodes$name),], addnodes)
  netinfo$links = dplyr::bind_rows(netinfo$links, addlinks)
  netinfo$links = unique(netinfo$links)
  
  netinfo$network = graph_from_data_frame(d=netinfo$links, vertices=netinfo$nodes, directed=T)
  netinfo
}

# Add derived information to the basic network data
#
addDerivedNetworkData <-  function(nstruct) {
  ns = nstruct
  ds = unique(V(ns$network)$domain)
  ds = ds[ds != ""]
  lt = unique(E(ns$network)$linktype)
  lt = lt[lt != ""]
  nt = unique(V(ns$network)$nodetype)
  nt = nt[nt != ""]
  
  ns[["domains"]] = ds
  ns[["linktypes"]] = lt
  ns[["nodetypes"]] = nt
  ns
}

# make a data frame suitable for presenting in a tableoutput
flattenedDataFrameForTable <- function(df) {
  r = flatten(df)
  for (c in 1:length(df)) {
    if (is.list(r[[c]])) {
      r[[c]] = vapply(r[[c]], toString, character(1L))
    }
  }
  r
}

# Used for conversion from CSV to Json format. Once only
convertCSVtoJSON   <- function(layers) {
  for (l in layers) {
    lnodes <- read.csv2(paste0("Data/", l, "-Nodes.csv"), header=T, colClasses="character", sep=";")
    llinks <- read.csv2(paste0("Data/", l, "-Links.csv"), header=T, colClasses="character", sep=";")
    lnodes$groups=list(rep(vector(), length(lnodes)))
    res = list(nodes=lnodes, links=llinks)
    writeLines(toJSON(res, pretty=T), paste0("Data/", l,".json"))
  }
}


