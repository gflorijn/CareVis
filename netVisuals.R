#
# Visualisatie settings
#
require("igraph", quietly=T)
require("RColorBrewer", quietly=T)
require("plyr", quietly=T)
require("png", quietly=T)
require("RCurl", quietly=T)
require("visNetwork", quietly=T)
require("shiny", quietly=T)
require("stringr", quietly=T)

getNodeColorFor <- function(view, node) {
  map = tibble(type=getNodeTypes(view$net), color=brewer.pal(length(getNodeTypes(view$net)),"Set3"))
  return(map$color[map$type==node$nodetype])
}

getEdgeColorForLinktype <- function(net, linktype) {
  map = tibble(type=getLinkTypes(net), color=brewer.pal(length(getLinkTypes(net)),"Accent"))
  return(map$color[map$type==linktype])
}
getEdgeColorFor <- function(view, edge) {
  return(getEdgeColorForLinktype(view$net, edge$linktype))
}

getImagePath <- function(view) {
  return("")
}

getNodeShapeFor <- function(view, node) {
  return("dot")
}

# # Add visualisation settings to network structure (list)
# extendNetworkInfoForVisualisation <-  function(nstruct) {
#   ns = nstruct
#   
#   # See display.brewer.all() for examples
#   #
#   dcolors = brewer.pal(length(ns$nodetypes),"Set3")
#   ltcolors = brewer.pal(length(ns$linktypes), "Accent")
#   ltcolors[4] = ltcolors[8]
#   #ip <- "http://localhost:8001/www/"
#   ip <- ""
#   
#   ns[["nodetypecolors"]] = dcolors
#   ns[["linktypecolors"]] = ltcolors
#   ns[["imagepath"]] = ip
#   
#   ns
# }

getImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/", if_else( (node$icon!=""), node$icon, node$label), ".png"))
}

getBrokenImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/NotFound", ".png"))
}


# # define basic properties for visualisation of network
# #
# setupVisualDefinitionsForNetwork <- function(net) {
#   
#   nd = net$nodes
#   ne = net$edges
#   
#   nd$color = mapvalues(nd$nodetype, from=net$nodetypes, to=net$nodetypecolors, warn_missing = TRUE)
#   ne$color = mapvalues(ne$linktype, from=net$linktypes, to=net$linktypecolors, warn_missing = TRUE)
#   
#   nd$image = str_c(net$imagepath, "Images/", if_else( (nd$icon!=""), nd$icon, nd$label), ".png")
#   nd$brokenImage = str_c(net$imagepath, "Images/NotFound", ".png")
# 
#   # nd$groupnames = vapply(nd$groups, toString, character(1L))
#   net$nodes = nd
#   net$edges = ne
#   return(net)
# }
  

getVisualSettingsForNode <- function(view, node, doimage) {
  return(tibble(
      shape=if_else((!is.null(doimage) & doimage==TRUE), "image", "dot"),
      color=getNodeColorFor(view, node),
      image=getImageForNode(view, node),
      brokenImage=getBrokenImageForNode(view, node)
  ))
}

addVisualSettingsToNode <- function(view, node, doimage) {
  gv = getVisualSettingsForNode(view,node, doimage)
  node = bind_cols(node, gv)
  return(node)
}

getVisualSettingsForEdge <- function(view, edge, dolabel) {
  return(tibble(
    color=getEdgeColorFor(view, edge),
    vislabel=if_else((!is.null(dolabel) & dolabel==TRUE), edge$label, "")  # will be mapped to visnetwork label...
  ))
}

addVisualSettingsToEdge <- function(view, edge, dolabel) {
  gv = getVisualSettingsForEdge(view,edge, dolabel)
  edge = bind_cols(edge, gv)
  return(edge)
}


# addVisualSettingsForView <- function(view, doimages, dolinklabels) {
#     # cat('Add visuals\n')
#     
#     if (doimages) {
#       view$nodes$shape = "image"
#     }
#     else {
#       view$nodes$shape = "dot"
#     }
# 
# 
#     #view$nodes$widthConstraint = TRUE  #Applies to long labels inside shapes - not useful for images
# 
#     if (!dolinklabels) { # hiermee verdwijnen de labels, TODO
#       view$edges$label = ""
#     }
# 
#     return(view)
# }
