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

getImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/", if_else( (node$icon!=""), node$icon, node$label), ".png"))
}

getBrokenImageForNode <- function(view, node) {
  return(str_c(getImagePath(view), "Images/NotFound", ".png"))
}

getActualShapeForNode <- function(view, node, doimage) {
  if ("v_shape" %in% colnames(node)) {
    if (!is.na(node$v_shape) & !is.null(node$v_shape) & node$v_shape!="") {
        return(node$v_shape)
    }
  }
  return(if_else((!is.null(doimage) & doimage==TRUE), "image", getNodeShapeFor(view, node)))
}

getActualColorForNode <- function(view, node) {
  if ("v_color" %in% colnames(node)) {
    if (!is.na(node$v_color) & !is.null(node$v_color) & node$v_color!="") {
      return(node$v_color)
    }
  }
  return(getNodeColorFor(view, node))
}

getActualImageForNode <- function(view, node) {
  if ("v_image" %in% colnames(node)) {
    if (!is.na(node$v_image) & !is.null(node$v_image) & node$v_image!="") {
      return(node$v_image)
    }
  }
  return(getImageForNode(view, node))
}

# Allow visual control over image, color and shape setting by node-properties:
# v_image, v_color and v_shape
# 
# Todo: check whether there is a smart "merge" operation to handle this...
#
getVisualSettingsForNode <- function(view, node, doimage) {

  fshape = getActualShapeForNode(view, node, doimage)
  fcolor = getActualColorForNode(view, node)
  fimage = getActualImageForNode(view, node)
  
  return(tibble(
      shape=fshape,
      color=fcolor,
      image=fimage,
      brokenImage=getBrokenImageForNode(view, node)
  ))
}

addVisualSettingsToNode <- function(view, node, doimage) {
  gv = getVisualSettingsForNode(view,node, doimage)
  node = bind_cols(node, gv)
  return(node)
}

getVisualSettingsForEdge <- function(view, edge, dolabel, doarrows) {
  return(tibble(
    color=getEdgeColorFor(view, edge),
    arrows=if_else(doarrows, "to", NULL),
    vislabel=if_else((!is.null(dolabel) & dolabel==TRUE), edge$label, "")  # will be mapped to visnetwork label...
  ))
}

addVisualSettingsToEdge <- function(view, edge, dolabel, doarrows) {
  gv = getVisualSettingsForEdge(view,edge, dolabel, doarrows)
  edge = bind_cols(edge, gv)
  return(edge)
}

