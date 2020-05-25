

#
# Visualisatie settings
#
# Kleurinstellingen: we gebruiken brewer, zie colorbrewer.org
#
require("igraph", quietly=T)
require("RColorBrewer", quietly=T)
require("plyr", quietly=T)
require("png", quietly=T)
require("RCurl", quietly=T)
require("visNetwork", quietly=T)
require("shiny", quietly=T)


# gebruik brewer om kleuren te kiezen
znvis.domeinen = zndef.domeinen
znvis.domeinen.kleuren = brewer.pal(length(znvis.domeinen),"Set3")

# de default shape is circle.
znvis.domeinen.shapes = rep("circle", length(znvis.domeinen))

#voor de eigenschappen van de links (de kleuren) gebruikebn we de linktypes
znvis.linksoorten = zndef.linksoorten
znvis.linksoorten.kleuren = brewer.pal(length(znvis.linksoorten), "Set1")
znvis.linkslegenda = data.frame(label=znvis.linksoorten, color=znvis.linksoorten.kleuren)


#We gaan ervanuit dat voor elke node een png bestand is voor visualisatie
#Hier moeten de urls van de images staan - vereist opstarten van een webserver bij lokaal gebruik

znvis.imagepath <- "http://localhost:8001/www/"

#
#Zet basis settings voor visualisatie van iGraph graphs 
znvis.defaultVisualisatieSettingsVoorGraaf <- function(lnet) {
  #cat('Defaultvisualisatiesettings voor graaf\n')
  llnet = lnet
  
  #Todo: dit geeft nu waarschuwingen als domeinen niet gebruikt worden.
  V(llnet)$color = mapvalues(V(llnet)$domein, from=znvis.domeinen, to=znvis.domeinen.kleuren, warn_missing = FALSE)
  V(llnet)$shape = mapvalues(V(llnet)$domein, from=znvis.domeinen, to=znvis.domeinen.shapes, warn_missing = FALSE)

  E(llnet)$color = mapvalues(E(llnet)$linktype, from=znvis.linksoorten, to=znvis.linksoorten.kleuren, warn_missing = FALSE)

  #Het image attribuut wordt gebruikt door visNetwork
  #
  V(llnet)$image = paste0(znvis.imagepath, "Images/", V(llnet)$naam, ".png")
  llnet 
}

#Zet de settings voor visNetwork
znvis.visNetworkVisualisatieSettings <- function(lnet, doimages, dolinks, dolinklabels) {
  
  vnet = znvis.defaultVisualisatieSettingsVoorGraaf(lnet)  
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
# Maak een plot versie van een igraph
# Instellingen voor visualisatie
zorgvis.maakPlotGraaf <- function(g, doimages) {
  V(g)$shape = "circle"
  if (doimages) {
    g = znvis.laadRastersVoorGraaf(g)
  }
  g
}


znvis.getnodesize <- function(name) { 20 }



# Bepaal welke layouts gebruikt kunnen worden
# Zie voor achtergrond: ?igraph::layout_
znvis.layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
znvis.layouts <- znvis.layouts[!grepl("bipartite|merge|norm|sugiyama|tree", znvis.layouts)]


#
# Definities voor plots van rasters
#

# Voor plots: laad raster file voor een node
znvis.laadRasterVanURL <- function(imageurl) {
  result = ""
  if (url.exists(imageurl)) {
    result = readPNG(getURLContent(imageurl))
  }
  result
}


# laad rasters van lokale schijf
#
znvis.localimagepath <- "./Data/Images/"

#laad rasterfile uit lokale file.
#
znvis.laadRasterLokaal <- function(name) {
  fname = paste0(znvis.localimagepath, name, ".png")
  readPNG(fname)
}


znvis.laadRastersVoorGraaf <- function(lnet) {
  xnet = lnet
  lnamen = V(xnet)$naam
  limages = lapply(lnamen, znvis.laadRasterLokaal)
  V(xnet)$raster = limages[match(V(xnet)$naam, lnamen)]
  xnet
}

# Oud spul

#plot(netsystemen, vertex.shape="raster", vertex.label.dist=2, vertex.label.degree=pi/2, #vertex.label=NA,
#      vertex.size=12, vertex.size2=12, edge.width=2, edge.label=NA, edge.arrow.mode="")

# #Maak een graaf voor een bepaald systeem
# 
# sid="Zorgdomein"
# basen = netsystemen
# netZD = basen - V(basen)[V(basen)$naam != sid & V(basen)$domein=="Systeem"]
# plot(netZD, vertex.label.dist=1, vertex.label.degree=pi/2, #vertex.shape="raster", #vertex.label=NA,
#      vertex.size=12, vertex.size2=12, edge.width=2, edge.label=NA, edge.arrow.mode="")

