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

# Add visualisation settings to network structure (list)
extendNetworkInfoForVisualisation <-  function(nstruct) {
  ns = nstruct
  
  # See display.brewer.all() for examples
  #
  dcolors = brewer.pal(length(ns$domains),"Set3")
  dshapes = rep("dot", length(ns$domains))
  ltcolors = brewer.pal(length(ns$linktypes), "Accent")
  ltcolors[4] = ltcolors[8]
  #ip <- "http://localhost:8001/www/"
  ip <- ""
  
  ns[["domaincolors"]] = dcolors
  ns[["domainshapes"]] = dshapes
  ns[["linktypecolors"]] = ltcolors
  ns[["imagepath"]] = ip
  
  ns
}

#
#Zet basis settings voor visualisatie van graaf, gegeven settubgs
setupVisualDefinitionsForGraph <- function(lnet, netinfo) {
  #cat('Defaultvisualisatiesettings voor graaf\n')
  llnet = lnet
  ns = netinfo
  V(llnet)$color = mapvalues(V(llnet)$domain, from=ns$domains, to=ns$domaincolors, warn_missing = FALSE)
  V(llnet)$shape = mapvalues(V(llnet)$domain, from=ns$domains, to=ns$domainshapes, warn_missing = FALSE)
  E(llnet)$color = mapvalues(E(llnet)$linktype, from=ns$linktypes, to=ns$linktypecolors, warn_missing = FALSE)

  # Default attempt for imagefile is the node name.
  # Can be overriden by the icon attribute
  V(llnet)$image = paste0(ns$imagepath, "Images/", V(llnet)$name, ".png")
  iconnodes = V(llnet)[V(llnet)$icon != ""]
  for (i in iconnodes) {
    V(llnet)[i]$image = paste0(ns$imagepath, "Images/", V(llnet)[i]$icon, ".png")
  }
  V(llnet)$brokenImage = paste0(ns$imagepath, "Images/NotFound", ".png")

  #  browser()
  #string representation for groups used in networdk browser widget
  for (n in V(llnet)[[]]) {
    V(llnet)[[n]]$groupnames = toString(V(llnet)[[n]]$groups)
  }
  llnet 
}



#Set the properties for visualisation
visNetworkVisualisationSettings <- function(lnet, netinfo, doimages, dolinks, dolinklabels) {
  
  vnet = lnet

  # see the visnetwork documentation for meaning. Size is node size, value a scale factor
  V(vnet)$size = 25
  V(vnet)$value = 20
  
  if (doimages) {
    V(vnet)$shape = "image" 
  }
  
  # Hide category nodes
  V(vnet)[V(vnet)$nodetype=="category"]$hidden=TRUE
  
  V(vnet)$widthConstraint = TRUE  #Applies to long labels inside shapes - not useful for images
 
  if (!dolinklabels) {
    E(vnet)$label = ""
  }
  E(vnet)$hidden = !dolinks
  E(vnet)$width = 2
  
  vnet
}

