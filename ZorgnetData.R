# laad data voor zorgnet visualisatie


require("igraph", quietly=T)

# laad de data  een bepaald perspectief. Elk perspectief omvat een nodes en een links file.
#de functie levert een list op van twee dataframes - de nodes en de links.
#als het argument nodeslinks niet null is worden de ingelezen nodes en links toegevoegd 
#aan de meegegeven argumenten zodat er uiteindelijk 1 graaf ontstaat
#
zndata.laadPerspectief <- function(nodeslinks=NULL, naam) {
  cat('laadPerspectief: ', naam, "\n")

  lnodes <- read.csv2(paste0("Data/", naam, "-Nodes.csv"), header=T, colClasses="character", sep=";")
  llinks <- read.csv2(paste0("Data/", naam, "-Links.csv"), header=T, colClasses="character", sep=";")

  #   #Voeg links toe voor de categorieen
  # categorie = lnodes$id[1]
  # targets = lnodes$id[2:nrow(lnodes)]
  # # cat("categorie = ", categorie, "\n")
  # # cat("targets = ", targets, "\n")
  # categorielinks = data.frame(from=categorie, to=targets, label="", linktype="_cat", weight=0)
  # llinks = rbind(llinks, categorielinks)
  # 
    # print(lnodes)
  # print(llinks) 
  llinks$van = llinks$from
  llinks$naar = llinks$to
  lnodes$naam = lnodes$id
  # lnodes$bron = naam
  # llinks$bron = naam

  nodenames = lnodes$naam
  if (!is.null(nodeslinks)) {
    oln = nodeslinks[[1]]
    nodenames = c(nodenames, oln$naam)
  }

  n = zndata.checkLinks(nodenames, llinks)
  
  if (length(n) > 0) {
    cat("Laag ", naam, " ontbrekende node definities: ", n, "\n")
  }
  
  if (is.null(nodeslinks)) {
    result = list(lnodes, llinks)
  }
  else {
    olinks = nodeslinks[[2]]
    onodes = nodeslinks[[1]]
    rnodes = rbind(onodes, lnodes)
    rlinks = rbind(olinks, llinks)
    result = list(rnodes, rlinks)
  }
  result
}

#check of links verwijzen naar bekende nodes
#todo: accepteer onbekende nodes, maak er forward references van.
# of doe de checklinks pas aan het eind.
#
zndata.checkLinks <- function(nodenames, links) {
  nofrom = !(links$from %in% nodenames)
  noto = !(links$to %in% nodenames)
  c(links$from[nofrom], links$to[noto])
}

# Lees alle aangegeven perspectieven. Produceer een lijst bestaande uit nodes en links voor de graaf.
zndata.leesPerspectieven <- function(namen) {
  #cat("n = ", namen)
  lnl = NULL
  for (i in namen) {
    lnl = zndata.laadPerspectief(lnl, naam=i)
  }
  lnl
}

#Voeg nodes en links voor de categorieen (dus de domeinen) aan een graaf
zndata.voegCategorienToe <- function(g) {
  doms = unique(V(g)$domein)
  g2 = add_vertices(g, length(doms), doms, nodetype = "category", lijn)
  # 
  # for (d %in% doms) {
  #   g2 = add_vertices
  # }
  g2
} 

#===
# lees de data
#
zndata.perspectiefnamen = c("Patienten", "Zorgaanbieders", "Administratie", "Gegevens",  
                             "Interactie", "Systemen","Platformen",  "Standaarden", 
                            "Leveranciers")

nls = zndata.leesPerspectieven(zndata.perspectiefnamen)


#definieer de basis graafstructuren voor de rest van de applicatie:
zndata.netall.nodes = nls[[1]]
zndata.netall.links = nls[[2]]
zndata.geladengraaf = graph_from_data_frame(d=zndata.netall.links, vertices=zndata.netall.nodes, directed=T)
#zndata.verrijktegraaf = zndata.voegCategorienToe(zndata.geladengraaf)

zndata.netall = zndata.geladengraaf
zndata.netUD = as.undirected(zndata.netall, mode=c("each"))

#De graaf zonder nodes die geen verbinding hebben met de graaf.
#netall.connected = netall - V(netall)[degree(netall, V(netall)) == 0]
#todo: gebruik "ZonderOndervbondn

