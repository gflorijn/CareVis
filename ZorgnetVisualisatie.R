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

# Add visualisation settings to network structure (list)
extendNetworkInfoForVisualisation <-  function(nstruct) {
  ns = nstruct
  
  # See display.brewer.all() for examples
  #
  dcolors = brewer.pal(length(ns$domains),"Set3")
  dshapes = rep("circle", length(ns$domains))
  ltcolors = brewer.pal(length(ns$linktypes), "Dark2")
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
znvis.defaultVisualisatieSettingsVoorGraaf <- function(lnet, netinfo) {
  #cat('Defaultvisualisatiesettings voor graaf\n')
  llnet = lnet
  ns = netinfo

  V(llnet)$color = mapvalues(V(llnet)$domein, from=ns$domains, to=ns$domaincolors, warn_missing = FALSE)
  V(llnet)$shape = mapvalues(V(llnet)$domein, from=ns$domains, to=ns$domainshapes, warn_missing = FALSE)

  E(llnet)$color = mapvalues(E(llnet)$linktype, from=ns$linktypes, to=ns$linktypecolors, warn_missing = FALSE)

  # Default attempt for imagefile is the node name.
  # Can be overriden by the icon attribute
  V(llnet)$image = paste0(ns$imagepath, "Images/", V(llnet)$naam, ".png")
  iconnodes = V(llnet)[V(llnet)$icon != ""]
  
  for (i in iconnodes) {
    V(llnet)[i]$image = paste0(ns$imagepath, "Images/", V(llnet)[i]$icon, ".png")
  }

  V(llnet)$brokenImage = paste0(ns$imagepath, "Images/NotFound", ".png")
  llnet 
}

#Zet de settings voor visNetwork
znvis.visNetworkVisualisatieSettings <- function(lnet, netinfo, doimages, dolinks, dolinklabels) {
  
  vnet = znvis.defaultVisualisatieSettingsVoorGraaf(lnet, netinfo)  
  donodesize = 20  
  
  if (doimages) {
    V(vnet)$shape = "image" # voor visnetwork
    # V(vnet)[V(vnet)$nodetype == "hub"]$shape = "circularImage"
    # V(vnet)[V(vnet)$nodetype == "category"]$shape = "dot"
    # V(vnet)[V(vnet)$nodetype == "hub"]$color = "white"
  }
  else {
    V(vnet)$shape = "dot"
    #   V(vnet)[V(vnet)$nodetype == "hub"]$shape = "diamond"
  }
  
  V(vnet)$size = donodesize
  
  # Verstop category nodes
  V(vnet)[V(vnet)$nodetype=="category"]$hidden=TRUE
  
  if (!dolinklabels) {
    E(vnet)$label = ""
  }
  E(vnet)$hidden = !dolinks
  
  
  E(vnet)$width = 2
  E(vnet)$dashes[E(vnet)$linktype %in% c("_cat", "info", "is")] = TRUE  
  
  vnet
}


# 
# ======== OLD STUFF
# 
# 
# #
# # Maak een plot versie van een igraph
# # Instellingen voor visualisatie
# zorgvis.maakPlotGraaf <- function(g, doimages) {
#   V(g)$shape = "circle"
#   if (doimages) {
#     g = znvis.laadRastersVoorGraaf(g)
#   }
#   g
# }
# 
# 
# znvis.getnodesize <- function(name) { 20 }
# # Bepaal welke layouts gebruikt kunnen worden
# # Zie voor achtergrond: ?igraph::layout_
# znvis.layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# znvis.layouts <- znvis.layouts[!grepl("bipartite|merge|norm|sugiyama|tree", znvis.layouts)]
# 
# 
# #
# # Definities voor plots van rasters
# #
# 
# # Voor plots: laad raster file voor een node
# znvis.laadRasterVanURL <- function(imageurl) {
#   result = ""
#   if (url.exists(imageurl)) {
#     result = readPNG(getURLContent(imageurl))
#   }
#   result
# }
# 
# 
# # laad rasters van lokale schijf
# #
# znvis.localimagepath <- "./Data/Images/"
# 
# #laad rasterfile uit lokale file.
# #
# znvis.laadRasterLokaal <- function(name) {
#   fname = paste0(znvis.localimagepath, name, ".png")
#   readPNG(fname)
# }
# 
# 
# znvis.laadRastersVoorGraaf <- function(lnet) {
#   xnet = lnet
#   lnamen = V(xnet)$naam
#   limages = lapply(lnamen, znvis.laadRasterLokaal)
#   V(xnet)$raster = limages[match(V(xnet)$naam, lnamen)]
#   xnet
# }
# 
# # Oud spul
# 
# #plot(netsystemen, vertex.shape="raster", vertex.label.dist=2, vertex.label.degree=pi/2, #vertex.label=NA,
# #      vertex.size=12, vertex.size2=12, edge.width=2, edge.label=NA, edge.arrow.mode="")
# 
# # #Maak een graaf voor een bepaald systeem
# # 
# # sid="Zorgdomein"
# # basen = netsystemen
# # netZD = basen - V(basen)[V(basen)$naam != sid & V(basen)$domein=="Systeem"]
# # plot(netZD, vertex.label.dist=1, vertex.label.degree=pi/2, #vertex.shape="raster", #vertex.label=NA,
# #      vertex.size=12, vertex.size2=12, edge.width=2, edge.label=NA, edge.arrow.mode="")

