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
  map = tibble(type=getLinkTypes(net), color=brewer.pal(length(getLinkTypes(net)),"Set1"))
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

getImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/", if_else( (node$icon!=""), node$icon, node$label), ".png"))
}

getBrokenImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/NotFound", ".png"))
}

addActualShapeForNode <- function(view, node, doimage) {
  if ("shape" %in% colnames(node)) {
    if (!is.na(node$shape) & !is.null(node$shape) & node$shape!="") {
        return(node)
    }
    else {
      node$shape=if_else((!is.null(doimage) & doimage==TRUE), "image", getNodeShapeFor(view, node))
      return(node)
    }
  }
  newnode = add_column(node, shape=if_else((!is.null(doimage) & doimage==TRUE), "image", getNodeShapeFor(view, node)))
  return(newnode)
}

addActualColorForNode <- function(view, node) {
  if ("color" %in% colnames(node)) {
    if (!is.na(node$color) & !is.null(node$color) & node$color!="") {
      return(node)
    }
    else {
      node$color=getNodeColorFor(view, node)
      return(node)
    }
  }
  newnode = add_column(node, color=getNodeColorFor(view, node))
  return(newnode)
}

addActualImageForNode <- function(view, node) {
  if ("image" %in% colnames(node)) {
    if (!is.na(node$image) & !is.null(node$image) ) {
      return(node)
    }
    else {
      node$image=getImageForNode(view, node)
      return(node)
    }
  }
  newnode = add_column(node, image=getImageForNode(view, node))
  return(newnode)
}

addActualBrokenImageForNode <- function(view, node) {
  if ("brokenImage" %in% colnames(node)) {
    if (!is.na(node$brokenImage) & !is.null(node$brokenImage)) {
      return(node)
    }
    else {
      node$brokenImage=getBrokenImageForNode(view, node)
      return(node)
    }
  }
  newnode = add_column(node, brokenImage=getBrokenImageForNode(view, node))
  return(newnode)
}


# Make sure that basic control over visual controls shape, image and color can be
# set in the input. 
#

addVisualSettingsToNode <- function(view, node, doimage, dofreeze) {
#   browser()
  newnode = node
  
  # Experiment
  newnode$label = str_wrap(newnode$label, width=20) #TODO: add explicit control in label

  newnode = addActualShapeForNode(view, newnode, doimage)
  newnode = addActualImageForNode(view, newnode)
  newnode = addActualColorForNode(view, newnode)
  newnode = addActualBrokenImageForNode(view, newnode)
  # if (dofreeze) {
  #   newnode$physics = FALSE
  # }
  return(newnode)
}

addActualColorForEdge <- function(view, edge) {
  if ("color" %in% colnames(edge)) {
    if (!is.na(edge$color) & !is.null(edge$color) & edge$color!="") {
      return(edge)
    }
    edge$color=getEdgeColorFor(view, edge)
    return(edge)
  }
  newedge = add_column(edge, color=getEdgeColorFor(view, edge))
  return(newedge)
}

addActualArrowsForEdge <- function(view, edge, doarrows) {
  if ("arrows" %in% colnames(edge)) {
    if (!is.na(edge$arrows) & !is.null(edge$arrows)) {
      return(edge)
    }
    edge$arrows=if_else(doarrows, "to", "")
    return(edge)
  }
  newedge = add_column(edge, arrows=if_else(doarrows, "to", ""))
  return(newedge)
}

addVisualSettingsToEdge <- function(view, edge, dolabel, doarrows, dofreeze) {
  newedge = edge
  
  newedge = addActualColorForEdge(view, newedge)
  newedge = addActualArrowsForEdge(view, newedge, doarrows)
  if (!dolabel)
    newedge$label = ""
  # if (dofreeze)
  #   newedge$smooth = FALSE
  return(newedge)
}

