
# definieer bepaalde begrippen/lijstjes uit de data
#
zndef.netall <- zndata.netall
zndef.netUD <- zndata.netUD

# geef de basisgraaf voor visualisatie
zndef.basisgraaf <- function(ongericht=TRUE) {
  if (ongericht)
    zndef.netUD
  else
    zndef.netall
}

#Voor de default visualisatie van de nodes gebruiken de domeinen als basis
#
zndef.domeinen = unique(V(zndef.netall)$domein)

#perspectieven == domeinen?
zndef.perspectieven = unique(V(zndef.netall)$domein)
zndef.perspectieven = zndef.perspectieven[zndef.perspectieven != ""]

zndef.linksoorten = unique(E(zndef.netall)$linktype)
zndef.linksoorten = zndef.linksoorten[zndef.linksoorten != ""]
zndef.nodetypes = unique(V(zndef.netall)$nodetype)
zndef.nodetypes = zndef.nodetypes[zndef.nodetypes != ""]



# verwijder onverbonden nodes 
# TODO: verwijder ook nodes met alleen een self-link (simplify crasht daarop)
znops.zonderOnverbondenNodes <- function(g) {
  y = g - V(g)[degree(g, V(g)) == 0]
  y
}


# maak een graaf voor de geselecteerde perspectieven, linktypes en wel of niet verbonden nodes
# TODO:
#
znops.maakGraafVoorPerspectieven <- function(net, perspectives, linktypes, dounconnected) {
  
  vnet = net - V(net)[!(V(net)$domein %in% perspectives)]
  vnet = vnet - E(vnet)[!(E(vnet)$linktype %in% linktypes)]
  
  if (!dounconnected) {
    vnet = znops.zonderOnverbondenNodes(vnet)
  }
  
  # cat("vnet\n")
  # print(V(vnet)[[]])
  # print(E(vnet)[[]])
  vnet
}



znops.focusLinks <- function(g, nodes, linktypes) {
  els = incident_edges(g, nodes)
  res = el
  z = E(z)[E(z)$linktype %in% linktypes]
  z
}

znops.maakViewgraaf <-  function(g, view, focusnodes, linktypes, blacklist) {
  # cat("mv ", focusnodes, "\n")
  newg = znops.voegNodesToeAanView(g, view, focusnodes)
  print(newg)
  newg
}



# znops.maakViewgraaf <-  function(g, view, focusnodes, linktypes, blacklist) {
#   friendnodes = znops.friendNodes(g, focusnodes, linktypes, blacklist)
#   totalnodes = c(focusnodes, friendnodes)
#   cat("totalnodes= ", totalnodes, "\n")
#   newg = g
#   newg = znops.voegNodesToeAanView(newg, view, totalnodes)
#   links = znops.focusLinks(newg, totalnodes, linktypes)
#   print(class(links))
#   newg = znops.voegLinksToeAanView(newg, view, E(links))
#   # 
#   # newg = g - V(g)[!(V(g)$name %in% totalnodes)]
#   # newg = newg - E(newg)[!(E(newg)$linktype %in% linktypes)]
#   newg
# }


# Voeg een view toe aan de graaf. includeall geeft aan of alle nodes/links erbij horen
znops.startViewOpGraaf <- function(graaf, viewnaam) {
  # bewaar de aangemaakte views 
  x = graph_attr(graaf, "hasviews")
  if (is.null(x))
    x = c()
  x = c(viewnaam, x)
  graph_attr(graaf, "hasviews") <- x
  graph_attr(graaf, viewnaam) <- FALSE
  edge_attr(graaf, viewnaam, E(graaf)) <- FALSE
  vertex_attr(graaf, viewnaam, V(graaf)) <- FALSE
  graaf
}


# Geef de namen van nodes die gelinkt zijn (volgens een relatie in linktypes) aan elementen uit nodes en geef ook de links
znops.friendNodes <-  function(g, nodes, linktypes, blacklist) {
  res = c()
  for (node in nodes) {
    es = incident(g, node)
    els = es[es$linktype %in% linktypes]
    ns = c(els$van, els$naar)
    res = append(res, ns)
  }
  res = unique(res)
  res = res[!(res %in% nodes)]
  res = res[!(res %in% blacklist)]
  res
}

znops.herstartViewOpNodes <- function(g, view, nodes) {
  # cat("herstart\n")
  # print(g)
  edge_attr(g, view, E(g)) <- FALSE
  vertex_attr(g, view, V(g)) <- FALSE
  g2 = znops.voegNodesToeAanView(g, view, nodes)
  g2
}

znops.voegVriendenToeAanView <- function(graaf, view, node, linktypes) {
  #browser()
  #cat("voeg vrienden ", node, " toe aan ", view, '\n')
  
  es = incident(graaf, node)
  els = es[es$linktype %in% linktypes]
  ns = unique(c(els$van, els$naar))
  
  # cat ('ns = ', ns, '\n')
  g = znops.voegNodesToeAanView(graaf, view, ns)
  g = znops.voegLinksToeAanView(g, view, els)
  g
}

znops.voegNodesToeAanView <-  function(graaf, view, nodes) {
  # cat("voeg toe ", nodes, " aan ", view, '\n')
  vertex_attr(graaf, view, V(graaf)[nodes]) <- TRUE
  # print(V(graaf)[[]])
  graaf
}

znops.verwijderNodesUitView <-  function(graaf, view, nodes) {
  vertex_attr(graaf, view, V(graaf)[nodes]) <- FALSE
  graaf
}

znops.voegLinksToeAanView <- function(graaf, view, edges) {
  edge_attr(graaf, view, E(graaf)[edges]) <- TRUE
  graaf
}

znops.verwijderLinksUitView <- function(graaf, view, edges) {
  edge_attr(graaf, view, E(graaf)[edges]) <- FALSE
  graaf
  
}

znops.nodesInView <- function(graaf, view) {
  g = graaf
  V(g)$naam[vertex_attr(g, view, V(g))]
}

znops.linksInView <- function(graaf, view) {
  E(graaf)[edge_attr(graaf, view, E(graaf))]
}

znops.toonAllesInView <- function(graaf, view) {
  vertex_attr(graaf, view, V(graaf)) <- TRUE
  edge_attr(graaf, view, E(graaf)) <- TRUE
  graaf
}

znops.copyViewInfo <- function(graaf, fromview, toview) {
  g = graaf
  # cat('copy view from ', fromview, ' to ', toview, '\n')
  ns = znops.nodesInView(g, fromview)
  es = znops.linksInView(g, fromview)
  g = znops.voegNodesToeAanView(g, toview, ns)
  g = znops.voegLinksToeAanView(g, toview, es)
  g
}



# # maak aparte groepen voor de nodes in de perspectieven
# # TODO!
# znops.maakGroepenVoor <- function(g, perspectieven) {
#   pnodes = V(g)$name[V(g)$domein %in% perspectieven]
#   pgroups = lapply(pnodes, function(i) {
#                      list(i, neighbors(g, i, mode="out"))
#                      })
# #  list(pnodes, neighbors(g, pnodes, mode="out"))
#   pgroups
# }


