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
  dcolors = brewer.pal(length(ns$nodetypes),"Set3")
  ltcolors = brewer.pal(length(ns$linktypes), "Accent")
  ltcolors[4] = ltcolors[8]
  #ip <- "http://localhost:8001/www/"
  ip <- ""
  
  ns[["nodetypecolors"]] = dcolors
  ns[["linktypecolors"]] = ltcolors
  ns[["imagepath"]] = ip
  
  ns
}

# define basic properties for visualisation of network
#
setupVisualDefinitionsForNetwork <- function(net) {
  
  nd = net$nodes
  ne = net$edges
  
  nd$color = mapvalues(nd$nodetype, from=net$nodetypes, to=net$nodetypecolors, warn_missing = TRUE)
  ne$color = mapvalues(ne$linktype, from=net$linktypes, to=net$linktypecolors, warn_missing = TRUE)
  
  nd$image = str_c(net$imagepath, "Images/", if_else( (nd$icon!=""), nd$icon, nd$label), ".png")
  nd$brokenImage = str_c(net$imagepath, "Images/NotFound", ".png")

  # nd$groupnames = vapply(nd$groups, toString, character(1L))
  net$nodes = nd
  net$edges = ne
  return(net)
}
  


addVisualSettingsForView <- function(view, doimages, dolinklabels) {
    # cat('Add visuals\n')
    
    if (doimages) {
      view$nodes$shape = "image"
    }
    else {
      view$nodes$shape = "dot"
    }


    # Hide category nodes
    view$nodes = subset(view$nodes, view$nodes$nodetype != "category")

    #view$nodes$widthConstraint = TRUE  #Applies to long labels inside shapes - not useful for images

    if (!dolinklabels) { # hiermee verdwijnen de labels, TODO
      view$edges$label = ""
    }

    return(view)
}
