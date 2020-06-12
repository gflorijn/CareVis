require("tidyverse", quietly = TRUE)

#
# These operations work on views on a network
# The view is a list consisting of:
#   name, net (the base network), nodes, edges
# The network  has the full list of nodes and edges, the view a selection.
#

# create a view on a network
newViewOnNetwork <- function(net, name) {
  return(
    list(name = name, net = net, nodes = tibble(), edges = tibble())
  )
}
restartViewOnNodesFromDomain <- function(view, d) {
#  browser()
  domnds = as_tibble(view$net$nodes)
  domnds = subset(domnds, domnds$domain == d)
  return(restartViewOnNodeIds(view, domnds$nid))
}

# Restart the view
restartViewOnNodeIds <- function(view, nodeids) {
  view$nodes = tibble()
  view$edges = tibble()
  view = addNodesToViewById(view, nodeids)
  return(view)
}

# Add nodes that are related to node via linktypes to the view
addFriendsOfNodeToView <- function(view, nodeid, linktypes) {
  #browser()
  basenet=view$net
  newedges = subset(basenet$edges, (basenet$edges$from == nodeid | basenet$edges$to == nodeid))
  newedges = subset(newedges, newedges$linktype %in% linktypes)
  # cat ('ns = ', ns, '\n')
  nodeids = unique(c(newedges$from, newedges$to))
  view = addNodesToViewById(view, nodeids)
  view = addEdgesToView(view, newedges)
  return(view)
}

addFriendsAndEdgesOfNodesInView <- function(view, linktypes) {
  nodeids = view$nodes$nid
  for (n in nodeids) {
    view = addFriendsOfNodeToView(view, n, linktypes)
  }
  return(view)
}

#
addNodesToViewById <-  function(view, nodeids) {
  #browser()
  basenet = view$net
  newnodes = subset(basenet$nodes, basenet$nodes$nid %in% nodeids)
  nondups = subset(view$nodes, !(view$nodes$nid %in% nodeids))
  view$nodes = bind_rows(nondups, newnodes)
  return(view)
}

addEdgesBetweenNodesInView <- function(view) {
  viewnodes = view$nodes$nid
  viewedges = subset(view$net$edges, (from %in% viewnodes)|(to %in% viewnodes))
  view$edges = viewedges
  return(view)
}

addEdgesToViewByEid <-  function(view, eids) {
  basenet = view$net
  newedges = subset(basenet$edges, basenet$edges$eid %in% eids)
  nondups = subset(view$edges, !(view$edges$eid %in% eids))
  view$edges = bind_rows(nondups, newedges)
  return(view)
}

# add a new node to the underlying network - similar to view
addNodesToNetwork <- function(net, nodes) {
  nda = subset(net$nodes, !(net$nodes$nid %in% nodes$nid))
  net$nodes = bind_rows(nda, nodes)
  return(net)
}

# add new edge(s) to the underlying network - similar to view
addEdgesToNetwork <- function(net, edges) {
  eda = subset(net$edges, !(net$edges$eid %in% edges$eid))
  net$edges = bind_rows(eda, edges)
  return(net)
}

addNodesToView <- function(view, nodes) {
#  browser()
  nda = subset(view$nodes, !(view$nodes$nid %in% nodes$nid))
  view$nodes = bind_rows(nda, nodes)
  return(view)
}

removeNodesFromViewById <-  function(view, nodeids) {
  newnodes = subset(view$nodes, !(view$nodes$nid %in% nodeids))
  view$nodes = newnodes
  return(view)
}

addEdgesToView <- function( view, newedges) {
#  browser()
  eda = subset(view$edges, !(view$edges$eid %in% newedges$eid ))
  view$edges = bind_rows(eda, newedges)
  return(view)
}

removeEdgesFromViewById <-  function(view, eids) {
  newedges = subset(view$edges, !(view$edges$eid %in% eids))
  view$edges = newnedges
  return(view)
}


getNodeById <-  function(view, n) {
  nd = subset(view$nodes, nid == n)
  return(nd)
}

existsNodeInView <- function(view, n) {
  return(nrow(subset(view$nodes, nid==n)) > 0)
}

getNodeIdsInView <- function(view) {
  return(view$nodes$nid)
}

getLinksIdsInView <- function(view) {
  return(view$edges$eid)
}

switchViewToNetwork <- function(view) {
  view$nodes = view$net$nodes
  view$edges = view$net$edges
  return(view)
}


