library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)

source("ZorgnetData.R")
source("ZorgnetOps.R")
source("ZorgnetVisualisatie.R")


znapp.mogelijkestartpunten = c("Patient", V(zndef.netUD)$name[V(zndef.netUD)$nodetype == "category"])
znapp.defaultnavrootnode = "Patient"

# lever de graaf op conform de nodes die geselecteerd  zijn of verborgen, 
# En haal nodes eruit die niet in de geselecteerde perspectieven zitten
#
znapp.maakgraafvoorfocusnodes <- function(g, focusnodes, blacklist, persps, docategories, doalles) {
  cat("focus = ", focusnodes, " blacklist = ", blacklist, "persps = ", persps, "\n")

  if (!doalles) {
    x = incident_edges(g, focusnodes)
    z = rle(unlist(x))$values
    y = subgraph.edges(g, z)
    y = y - V(y)[V(y)$name %in% blacklist]
    y = y - V(y)[!(V(y)$domein %in% persps)]
    if (!docategories)
      y = y - V(y)[V(y)$nodetype == "category"]
    # print(y[[]])
    y
  }
  else { #return de hele graaf
    g
  }
}

#
# De shiny app voor visnetworks
#
ui <- fluidPage(theme=shinytheme("flatly"),
                
                #  shinythemes::themeSelector(),
                useShinyjs(),
                
                titlePanel("Visualisatie van Zorgcommunicatie"),
                sidebarLayout(
                  sidebarPanel(width=2,
                         fluidRow(
                           column(12, 
                                selectInput("startingpoint", label="Startpunten", 
                                             choices=znapp.mogelijkestartpunten,
                                             selected="Patient"),
                                checkboxGroupInput("perspsel", "Perspectieven", zndef.perspectieven, selected= zndef.perspectieven), 
                                checkboxGroupInput("linksel", "Link types", zndef.linksoorten, selected = zndef.linksoorten),
                                tags$hr(),
                                selectInput("layout", "Layout opties", znvis.layouts, "layout_nicely"),
                                tags$hr(),
                                actionButton(inputId="quit", "Stoppen"),
                                
                           ),
                         )
                  ),
                  
                  mainPanel(
                    # fluidRow(
                    #   checkboxGroupInput("perspsel", "Perspectieven", zndef.perspectieven, selected="Basis", inline = TRUE), 
                    # ),
                    fluidRow(
                        column(2, checkboxInput("onverbonden", "Onverbonden", value=TRUE)),
                        column(2, checkboxInput("ongericht", "Ongericht", value=TRUE)),
                        column(2, checkboxInput("includecats", "Categories", TRUE)),
                        column(2, checkboxInput("images", "Iconen", TRUE)),
                        column(2, checkboxInput("showall", "Toon alles", FALSE))
   #                     column(2, sliderInput("nodegrootte", "Grootte", min=10, max=40, value=18)),
                    ),
                    fluidRow(
                      column(2, checkboxInput("showlinks", "Links", TRUE)),
                      column(2, checkboxInput("linklabels", "Link namen", FALSE)),
                      column(2, checkboxInput("smooth", "Smooth")),
                      column(2, checkboxInput("navigatie", "Navigatie")),
                    ),
                    fluidRow(
                      column(3, verbatimTextOutput("nodemessage")),
                      column(9,
                        actionButton("expandview", "Expand"),
                        actionButton("hidefromview", "Hide"),
                        actionButton("switchfocus", "Focus"),
                        actionButton("switchtoview", "View"),
                        actionButton(inputId="show", "Update"),
                        
                      )
                    ),
                    tabsetPanel(id="visTabSet",
                              tabPanel("Graaf", 
                                       visNetworkOutput("graph_panel", height="500px", width="100%")
                              ) 
                              #,
                              # tabPanel("Info",
                              #       "gogo"
                              # )
                              
                    ),
                    tags$hr(),
                    fluidRow(
                      downloadButton("export", "Export graaf"),
                      verbatimTextOutput("generalmessage"),
                    )
                  ),
                    
              )
          )





server <- function(input, output, session) {
  
  # 
  # De "State" variabelen van de applicatie
 
  rv <- reactiveValues(
    theigraph=NULL, 
    thevisgraph = NULL, 
    thegraphproxy = NULL,
    thenodeselected = NULL, 
    themessage = NULL,
    thecurrentfocus = NULL, 
    theblacklist = NULL
  )
  
  #
  # Initialiseer de data die de view bepaalt.
  
  isolate ({
    rv$theblacklist = c()
    rv$thecurrentfocus = znapp.defaultnavrootnode
    rv$thenodeselected = ""
  })
  
  
  # Quit button
  observeEvent(input$quit, {
    stopApp()
  })
  
  # Once the graph-panel is stabilized for drawing, grab the proxy for manipulations
  observeEvent(input$graph_panel_initialized, {
      p = visNetworkProxy(input$graph_panel)
      #cat('Initialized proxy \n')
      rv$thegraphproxy = p
  })

   
  # ==========
  # Selectie van een node
  observeEvent(input$graph_panel_selected, {
    #cat("Node selected ", input$graph_panel_selected, "\n")
    rv$thenodeselected = input$graph_panel_selected
  })
  
  # Selectie van een groep, (nog) niet gebruikt
  #  observeEvent(input$graph_panel_selectedBy, {
  #    cat("Observe-graph_panel_selectedBy ", input$graph_panel_selectedBy, "\n")
  #  })
  
  
  # == UI for node selectie afhandeling
   observeEvent(rv$thenodeselected, {
     #cat("Selected ", rv$thenodeselected, "\n")
    rv$themessage = " "
    toggleState("expandview", rv$thenodeselected != "")
    toggleState("hidefromview", rv$thenodeselected != "")
    toggleState("switchfocus", rv$thenodeselected != "")
    toggleState("switchtoview", rv$thenodeselected != "")
  })
   
  observeEvent(input$startingpoint, {
    rv$thecurrentfocus = c(input$startingpoint)
    rv$theblacklist = c()
  })
  
  # Expand node 
  observeEvent(input$expandview, {
    cat('expand ', rv$thenodeselected, '\n')
    rv$thecurrentfocus = append(rv$thecurrentfocus, rv$thenodeselected)
    # updateTabsetPanel(session, "visTabSet",
    #                   selected = "Info")
    # #    url = V(rv$theigraph)[input$thenodeselected]$url
    # #    cat('URL = ', url, "\n")
  })
  
  observeEvent(input$hidefromview, {
    cat('hide', rv$thenodeselected, '\n')
    rv$theblacklist = append(rv$theblacklist, rv$thenodeselected)
    # updateTabsetPanel(session, "visTabSet",
    #                   selected = "Info")
    # #    url = V(rv$theigraph)[input$thenodeselected]$url
    # #    cat('URL = ', url, "\n")
  })

  observeEvent(input$switchfocus, {
    rv$thecurrentfocus = c(rv$thenodeselected)
    rv$theblacklist = c()
  }) 
  
  # Update info panel - nu niet gebruikt
  # deinfotekst <- eventReactive(input$shownodeinfo, {
  #   n =  V(rv$theigraph)[rv$thenodeselected]
  #   n$url
  # })
  
  # output$node_info <- renderText({
  #   # deinfotekst()
  # })
  
  #======= Zoom on hub
  # Spawn hub - module
  
  observeEvent(input$switchtoview, {
    node = rv$thenodeselected
    hubid = paste0("view", node)
    tpid = paste0("View ", node)
    tabp = tabPanel(tpid, 
                    {
                      tagList(
                        hubZoomUI(hubid),
                      )
                    }
    )
    # 
    # Het zou mooi zijn om de input-settings voor de visualisaties
    # door te kunnen geven. Nu geven we de sessie door en moet in de sub-module
    # gerefereerd worden naar de parentsessie...
    # Todo: De netvisrendering moet uniform worden voor de graaf en de view panels
    #
    gr <- callModule(hubZoom, 
                     id=paste0("view", node),
                     session,  # nodig om visualisatiesettings te kunnen gebruiken
                     rv$thenodeselected
    )
    
    appendTab("visTabSet", tabp,
              select=TRUE)  
    
  })
  
  
  hubZoomUI <- function(id) {
    ns <- NS(id)
    { 
      visNetworkOutput(ns("hubdrawnet"), height="600px", width="100%") 
    }
  }
  
  hubZoom <- function(input, output, session, parentsession, node) {
    # 
    cat('in hubzoom node ', node)
    
    zoomHubGraaf <- reactive({
      cat('in zoomhubgraaf')
      znops.maakGraafVoorHub(zndef.basisgraaf(parentsession$input$ongericht), node)
    })
    
    output$hubdrawnet <- renderVisNetwork({
      cat('Tekenen')
      g = zoomHubGraaf()
      
      g2 = znvis.visNetworkVisualisatieSettings(g, parentsession$input$images,
                                                parentsession$input$showlinks, parentsession$input$linklabels)
      g3 = visIgraph(g2, layout=parentsession$input$layout, smooth=parentsession$input$smooth)
      # if (parentsession$input$navigatie)
      #   g3 = visInteraction(g3, navigationButtons = TRUE)
      
      g3
    })
  }
  
  #==========  
  #
  # Export the graph
  #
  output$export <- downloadHandler(
    filename = function() {
      f = paste('network-', Sys.Date(), '.html', sep='')
      rv$themessage = paste0("Exporteer naar ", f)
      f
    },
    content = function(con) {
      visSave(rv$thevisgraph, file=con)      
    }
  )
  
  output$nodemessage <- renderText({
    paste0(rv$thenodeselected," ")
  })
  
  output$generalmessage <- renderText({
    rv$themessage
  })
  # setPhysicsExperiment <- function(net) {
  #   E(net)[E(net)$linktype=="is"]$weight = input$islinkweight
  #   E(net)[E(net)$linktype=="has"]$weight = input$haslinkweight
  #   E(net)[E(net)$linktype=="link"]$weight = input$linklinkweight
  #   E(net)[E(net)$linktype=="info"]$weight = input$infolinkweight
  #   net
  # }
  
  # ====
  # Het hoofdpaneel voor de graphics
  # repaint als op de show knop wordtg gedrukt of als de focus/blacklist veranderen.
   degraaf <- eventReactive(
    eventExpr=c(input$show,rv$thecurrentfocus, rv$theblacklist, input$includecats),
    valueExpr={
      g = zndef.basisgraaf(input$ongericht)
      cat("doalles ", input$showall)
      g = znapp.maakgraafvoorfocusnodes(g, rv$thecurrentfocus, rv$theblacklist, input$perspsel, input$includecats, input$showall)
  cat("linksel ", input$linksel, "\n")
        E(g)[!(E(g)$linktype %in% input$linksel)]$hidden = TRUE
      g
    })
  
  # Dit ook nog eens uitproberen. Graaf niet opnieuw tekenen...
  # visNetwork(nodes, edges) %>%
  #   visEvents(type = "once", startStabilizing = "function() {
  #           this.moveTo({scale:0.1})}") %>%
  #   visPhysics(stabilization = FALSE)
  # 
  
  # # Event experimenten
  # observeEvent(input$click, {    
  #   cat("click "); 
  #   print(input$click$nodes)   
  # })
  # 
  
  # observeEvent(input$click, {    cat("click "); print(input$click$nodes)   })
  # observeEvent(input$oncontext, {    cat("oncontext "); print(input$oncontext)   })
  # observeEvent(input$selectNode, {    cat("selectNode "); print(input$selectNode)   })
  # observeEvent(input$selectEdge, {    cat("selectEdge "); print(input$selectEdge)   })
  #  observeEvent(input$deselectNode, {    cat("deselectNode "); print(input$deselectNode)   })
  #  observeEvent(input$deselectEdge, {    cat("deselectEdge "); print(inputdeselectEdge)   })
  
  output$graph_panel <- renderVisNetwork({
    visual = degraaf() 
    cat('Paint\n')
    visual2 = znvis.visNetworkVisualisatieSettings(visual, input$images, 
                                                   input$showlinks, input$linklabels)
    rv$theigraph = visual2
    
    vnt = visIgraph(visual2, layout=input$layout, smooth=input$smooth) 
    vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE)
#    vnt = visPhysics(vnt, solver="barnesHut")
    if (input$navigatie)
      vnt = visInteraction(vnt, navigationButtons = TRUE)
    #    vnt = visEdges(arrows="from") # directed network
    rv$thevisgraph = vnt
    vnt
  })
}

shinyApp(ui = ui, server = server)
