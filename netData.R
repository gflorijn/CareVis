# laad data voor zorgnet visualisatie

require("jsonlite", quietly=T)
require("tidyverse", quietly=T)

# laad de data  een bepaald perspectief. Elk perspectief omvat nodes.
#de functie levert een list op van twee dataframes - de nodes en de links.
#als het argument nodeslinks niet null is worden de ingelezen nodes en links toegevoegd 
#aan de meegegeven argumenten zodat er uiteindelijk 1 graaf ontstaat
#
loadNetworkSlice <- function(nodesedges=NULL, name) {
  cat('load slice: ', name, "\n")
  
  newslice = fromJSON(paste0("Data/", name, ".json"))
  lnodes=newslice$nodes
  ledges=newslice$edges 
  
  oln = c()
  nodenames = lnodes$nid
  if (!is.null(nodesedges)) {
    oln = nodesedges$nodes 
    nodenames = c(nodenames, oln$nid)
  }
#  browser()
  err = FALSE
  n = checkNodesInedges(nodenames, ledges)
  if (length(n) > 0) {
    simpleMessage(cat("Slice ", name, ":  unknown nodes in edges file: ", unique(n), " - creating them.\n"))
    for (i in unique(n)) {
      lnodes = bind_rows(lnodes, createNewUndefinedNode(i))
    }
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

# Read all slices
readSlices <- function(names) {
  #cat("n = ", namen)
  lnl = NULL
  for (i in names) {
    lnl = loadNetworkSlice(lnl, i)
  }
  lnl
}


# Read the network data and collect all info in a list. -------------------
# The list has a few elements: 
# $nodes - the nodes
# $edges - the edges

readNetworkData <-  function(slices) {
  nls = readSlices(slices)
  return(prepareNetworkDataForBrowsing(nls))
}

#augment network data with attributes for browsing
prepareNetworkDataForBrowsing <- function(nls) {
  #browser()
  # add some columns for graph handling
  nodes = as_tibble(nls$nodes)
  edges = as_tibble(nls$edges)
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
readViewFromJSON <-  function(jsonfile) {
  view = fromJSON(jsonfile)
  view$nodes = as_tibble(view$nodes)
  view$edges = as_tibble(view$edges)
  view$edges$eid = getEidForEdge(view$edges$from,view$edges$to, view$edges$label)
  return(view)
}

#add additional data to an existing network structure. Should be in "raw" format, should also be checked. 
#Avoids rereading all data
combineNetworks <- function(net1, net2) {
  net2names = net2$nodes$nid
  net2eids = net2$edges$eid
  nda = subset(net1$nodes, !(net1$nodes$nid %in% net2names))
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


# Node/Edge creation/cloning ----------------------------------------------
makeNewUniqueNodeIdFor <- function(net, nid) {
  pat = "^(.+)_(\\d+)$"
  m = str_match(nid, pat)
  if (is.na(m[1,1])) { # no match
    num = 1
    # newnm = str_c(nid, "_1")  # should check whether newnm is unique in the network
    # return(list(cnt=1, name=newnm))
  } else {
    num = as.integer(m[1,3])
  }
    newnm = str_c(nid, "_", num)
    while(existsNodeInView(net, newnm)) {
      num = num+1
      newnm = str_c(m[1,2], "_", num)
    }
    return(list(cnt=num, name=newnm))
}

#produce a clone of node. Give it a unique id in the network
createCloneOfNode <- function(view, node) {
  net = view$net
  onid = node$nid
  newnid = makeNewUniqueNodeIdFor(view$net, onid)
  newnode = node
  newnode$nid = newnid$name
  newnode$label = newnode$nid
  return(newnode)
}

createNewUndefinedNode <- function(nid) {
  return(tibble(nid=nid, label=nid, icon="", url="", groups="", image="", domain="Undefined", nodetype="undefined"))
}
