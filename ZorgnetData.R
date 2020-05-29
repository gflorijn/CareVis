# laad data voor zorgnet visualisatie


require("igraph", quietly=T)

# laad de data  een bepaald perspectief. Elk perspectief omvat een nodes en een links file.
#de functie levert een list op van twee dataframes - de nodes en de links.
#als het argument nodeslinks niet null is worden de ingelezen nodes en links toegevoegd 
#aan de meegegeven argumenten zodat er uiteindelijk 1 graaf ontstaat
#
loadNetworkLayer <- function(nodeslinks=NULL, naam) {
#  cat('laadPerspectief: ', naam, "\n")

  lnodes <- read.csv2(paste0("Data/", naam, "-Nodes.csv"), header=T, colClasses="character", sep=";")
  llinks <- read.csv2(paste0("Data/", naam, "-Links.csv"), header=T, colClasses="character", sep=";")
  #   #Voeg links toe voor de categorieen
  # categorie = lnodes$id[1]
  # targets = lnodes$id[2:nrow(lnodes)]
  # # cat("categorie = ", categorie, "\n")
  # # cat("targets = ", targets, "\n")
  # categorielinks = data.frame(from=categorie, to=targets, label="", linktype="_cat", weight=0)
  # llinks = rbind(llinks, categorielinks)
  # 
    # print(lnodes)
  # print(llinks) 
  llinks$van = llinks$from
  llinks$naar = llinks$to
  lnodes$naam = lnodes$id
  # lnodes$bron = naam
  # llinks$bron = naam

  nodenames = lnodes$naam
  if (!is.null(nodeslinks)) {
    oln = nodeslinks$nodes #[[1]]
    nodenames = c(nodenames, oln$naam)
  }

  n = checkNodesInLinks(nodenames, llinks)
  
  if (length(n) > 0) {
    cat("Laag ", naam, " ontbrekende node definities in links file: ", n, "\n")
  }
  
  if (is.null(nodeslinks)) {
    result = list(nodes=lnodes, links=llinks)
  }
  else {
    olinks = nodeslinks$links #[[2]]
    onodes = nodeslinks$nodes #[[1]]
    rnodes = rbind(onodes, lnodes)
    rlinks = rbind(olinks, llinks)
    result = list(nodes=rnodes, links=rlinks)
  }
  result
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
readNetworkData <-  function(perspectives) {
  nls = readLayers(perspectives)
  #browser()
  nodes = nls$nodes
  links = nls$links
  network = graph_from_data_frame(d=links, vertices=nodes, directed=T)
  list(nodes=nodes, links=links, network=network, layers=perspectives)
}

#add additional data to an existing network structure
#Avoids rereading all data
addAdditionalData <- function(netinfo, additionaldata) {
  nodes = netinfo$nodes
  links = netinfo$links
  nd2 = additionaldata$nodes
  lk2 = additionaldata$links
  nd2$naam = nd2$id
  lk2$van = lk2$from
  lk2$naar = lk2$to
  #browser()
  netinfo$nodes = rbind(nodes, nd2)
  netinfo$links = rbind(links, lk2)
  netinfo$network = graph_from_data_frame(d=netinfo$links, vertices=netinfo$nodes, directed=T)
  netinfo
}

# Add derived information to the basic network data
#
addDerivedNetworkData <-  function(nstruct) {
  ns = nstruct
  
  ds = unique(V(ns$network)$domein)
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
