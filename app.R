library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)

source("ZorgnetData.R")
source("ZorgnetOps.R")
source("ZorgnetVisualisatie.R")
source("frozenview.R")
source("uploaddata.R")

# Nodig om het browser window te sluiten
#
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
#
# De shiny app voor visualisaties
#
ui <- fluidPage(
  theme=shinytheme("simplex"),
  #shinythemes::themeSelector(),
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  titlePanel("Visualisatie van Zorgcommunicatie"),
  sidebarLayout(
    sidebarPanel(width=2,
                 fluidRow(
                   column(12, 
                          actionButton("showabout", "About"),
                          actionButton("showhelp", "Help"),
                          tags$hr(),
                          actionButton("showgraph", "Redraw"),
                          actionButton("restart", "Restart"),
                          tags$hr(),
                          actionButton("startupload", "Upload data"),
                          tags$hr(),
                          actionButton(inputId="quit", "Quit")
                   )
                 )
    ),
    
    mainPanel(
      tabsetPanel(id="visTabSet",
                  tabPanel("Main", 
                           fluidRow(
                             column(2, checkboxInput("navigatie", "Navigation", TRUE)),
                             column(2, checkboxInput("ongericht", "Undirected", value=TRUE)),
                             column(2, checkboxInput("images", "Icons", TRUE)),
                             column(2, checkboxInput("showlinks", "Links", TRUE)),
                             column(2, checkboxInput("linklabels", "Link names", FALSE)),
                             column(2, checkboxInput("smooth", "Smooth"))
                           ),
                           tags$hr(),
                           fluidRow(
                             column(3, uiOutput("startpointsmenu")),
                             column(2, verbatimTextOutput("nodemessage")),
                             column(3, uiOutput("nodeselectmenu")),
                             column(4,
                                    actionButton("growfocusall", "Grow"),
                                    actionButton("showall", "All"),
                                    actionButton("switchtoview", "> View"),
                                    downloadButton("export", "Export")
                             )
                           ), 
                           tags$hr(),
                           visNetworkOutput("graph_panel", height="600px", width="100%"),
                           tags$hr(),
                           fluidRow(
                             column(9, verbatimTextOutput("generalmessage")),
                             column(3, actionButton("launchbrowser", "Launch browser"))
                           )
                  )
      )
    )
    
  )
  
  )


server <- function(input, output, session) {
  
    # 
    # De "state" variabelen van de applicatie
   
    rv <- reactiveValues(
      thenetworkinfo = NULL,
      theigraph=NULL, 
      thevisgraph = NULL, 
      thegraphproxy = NULL,
      thenodeselected = NULL, 
      theurl = "",
      themessage = NULL,
      theviewcounter = 0,
      focusshowsall = FALSE, # True als de huidige weergave de hele database is.
      forcerepaint = FALSE,
      thecurrentview = NULL,
      haveuploadpane = FALSE,
      thenodesread = NULL,
      thelinksread = NULL,
      theuploadeddata = NULL
    )
    
    #layerstoload = c("Patienten")
    layerstoload = c("Patienten", "Zorgaanbieders", "Administratie", "Gegevens",
                     "Interactie", "Systemen","Platformen",  "Standaarden",
                     "Leveranciers")
    
    loadNetworkInfo <- function(netinfo, additionaldata) {
      if (is.null(additionaldata)) {
        n =  readNetworkData(layerstoload)
      }
      else { # in case of extending with uploaded data
        n = addAdditionalData(netinfo, additionaldata)
      }
      n = addDerivedNetworkData(n)
      n = extendNetworkInfoForVisualisation(n)
#      browser()
      n
    }
    
    restartAll <- function(additionaldata) {
      ni = loadNetworkInfo(rv$thenetworkinfo, additionaldata)
      rv$thenetworkinfo = ni
      rv$thenodeselected = ""
      rv$thecurrentview = "Main"
      rv$theigraph = ni$network
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, rv$thecurrentview)
      #  cat('initin\n')
      # print(rv$theigraph)
      
      nodes = c("Patient")
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
      updateTabsetPanel(session, "visTabSet", selected = "Main")
      
    }
    
    #
    # Initialiseer de data die de view bepaalt.
    isolate ({
      restartAll(NULL)
      })
    
    # Quit button
    observeEvent(input$quit, {
      js$closeWindow()
      stopApp()
    })
    
    #restart - load everything from the start
    # Todo - hoeft niet de hele graaf opnieuw te lezen
    observeEvent(input$restart, {
      restartAll(NULL)
    })

    # handle tabpanel selection event
    #
    observeEvent(input$visTabSet, {
 #     rv$thecurrentview = input$visTabSet
    })

    
 
    observeEvent(input$showabout, {
      showModal(modalDialog(
        title = "About",
        tags$p("An experimental browser for network graphs, in this case communication in the care sector in the Netherlands"),
        tags$p("Gert Florijn, 2020")
      ))
    })

    observeEvent(input$showhelp, {
      showModal(modalDialog(
        title = "Help",
        tagList(
          tags$p("Via de browser kun je navigeren door een netwerk van 
                  gegevens over informatie-uitwisseling in de zorg in Nederland.
                  De iconen (nodes) representeren personen, systemen, objecten, partijen, et cetera. 
                  De lijnen (links) geven verbanden aan. 
                 "),
          tags$p("Browsen is simpel. Als je de nodes van een bepaalde soort wilt zien, selecteer die soort dan uit de lijst.
                  Klik vervolgens op een node en kies welke verbanden (soorten links) je 
                  wilt toevoegen aan de view. De kleuren/letters van de knopjes geven de mogelijke 
                 verbanden aan:"),
          
          tags$ul(
            tags$li("a(ctor) links"), 
            tags$li("u(se) links"), 
            tags$li("s(ystem) links"), 
            tags$li("o(bject) links"), 
            tags$li("* alle soorten links")
          ),
          tags$p("De betekenis van andere knoppen:"),
          tags$ul(
            tags$li("H(ide) verwijdert de node uit de view"),
            tags$li("F(ocus) focusseert de view op deze node (dubbel-klik op de node doet dit ook)"),
            tags$li("Grow - voeg alle verbanden toe voor alle nodes in de view"),
            tags$li("All- toon alle nodes en links in het onderliggende netwerk"),
            tags$li(">View - maak een apart (read-only) viewpanel voor de huidige weergave"),
            tags$li("Export - exporteer de view naar een HTML bestand"),
            tags$li("Redraw - teken de view opnieuw (leidt tot andere layout)"),
            tags$li("Restart - breng de view terug naar de begintoestand")
          )
        )
      ))
    })
    
#     # 
#     # Dit is de event handler als de grafiek getekend is.
#     # Goede plek om the bevriezen? Dat kan via de proxy
#     # 
#     observeEvent(input$graph_panel_initialized, {
#         p = visNetworkProxy(input$graph_panel)
# #        cat('Initialized proxy \n')
#         rv$thegraphproxy = p
#     })
# 
   
    # ==========
    # Selectie van een node - twee methodes. dubbelop?
    
    observeEvent(input$current_node_id,  {
      #cat("input_current_node ", input$current_node_id, "\n")
    })

    observeEvent(input$graph_panel_selected, {
      #cat("Node selected ", input$graph_panel_selected, "\n")
      rv$thenodeselected = input$graph_panel_selected
    })
    
    
    # Selectie van een groep, (nog) niet gebruikt
    #  observeEvent(input$graph_panel_selectedBy, {
    #    cat("Observe-graph_panel_selectedBy ", input$graph_panel_selectedBy, "\n")
    #  })
    
    # # Rechter muis knop. Nu nog geen actie aan verbonden
    # observeEvent(input$oncontext, {
    # }) 
    
    # Double click ==> focus op de betreffende node
    observeEvent(input$doubleClick, {
      #browser()
      rv$thenodeselected = input$doubleClick$nodes[[1]]
      #cat('switch focus', rv$thenodeselected, '\n')
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    }) 
    
    
    # ===
    #
    
    # == UI for node selectie afhandeling
    observeEvent(rv$thenodeselected, {
      #cat("Selected ", rv$thenodeselected, "\n")
      rv$themessage = " "
      toggleState("hidefromview", rv$thenodeselected != "")
      toggleState("switchfocus", rv$thenodeselected != "")
      rv$theurl = ""
      haveurl = rv$thenodeselected != "" && V(rv$theigraph)[rv$thenodeselected]$url != ""
      
      if (haveurl) {
        rv$theurl = V(rv$theigraph)[rv$thenodeselected]$url
        rv$themessage = paste0("Zie voor meer informatie ", rv$theurl)
      }
      toggleState("launchbrowser", haveurl) #does not work on deployed apps
    })
    

    observeEvent(input$nodemenuclick, {
      if (rv$thenodeselected != "")
        handleSelectedNodeMenuClick(input$nodemenuclick)
    } )
    
    
    handleSelectedNodeMenuClick <- function(label) {
#browser()
      # cat('Grow current selection with links of type <', label, '>\n')
      linknodes = c(label)
      if (label == "all")
        linknodes = rv$thenetworkinfo$linktypes
      rv$theigraph = znops.voegVriendenToeAanView(rv$theigraph,rv$thecurrentview, rv$thenodeselected, linknodes)
    }
    
    
     

    observeEvent(input$focusopnodetype, {
      nodes = V(rv$theigraph)[V(rv$theigraph)$nodetype == input$focusopnodetype]$name
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
    })
    
    # Creeer en teken de graaf opnieuw
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    # # Expand node 
    # observeEvent(input$expandnodeall, {
    #   cat('expandall ', rv$thenodeselected, '\n')
    #   rv$theigraph = znops.voegVriendenToeAanView(rv$theigraph, rv$thecurrentview, rv$thenodeselected, 
    #                                                 zndef.linksoorten)
    # })

    # Verstop node
    observeEvent(input$hidefromview, {
      #cat('hide', rv$thenodeselected, '\n')
      rv$theigraph = znops.verwijderNodesUitView(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    })
  
    # Focus de view op een node
    observeEvent(input$switchfocus, {
      # cat('switch focus', rv$thenodeselected, '\n')
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
      # cat('new graph\n')
      # print(rv$theigraph)
    }) 
    
    # Breid de view uit met 1 niveau extra nodes verbonden met links van bepaalde types
    #
    observeEvent(input$growfocusall, {
      #cat('grow focus all', '\n')
      cns = znops.nodesInView(rv$theigraph, rv$thecurrentview)
      for (n in cns) {
        rv$theigraph = znops.voegVriendenToeAanView(rv$theigraph, rv$thecurrentview, n, 
                                                    rv$thenetworkinfo$linktypes)
        
      }
      rv$forcerepaint = TRUE
    })
    

     # Toon de hele graaf
    observeEvent(input$showall, {
      rv$theigraph = znops.toonAllesInView(rv$theigraph, rv$thecurrentview)
    }) 
    
    observeEvent(input$launchbrowser, {
      rv$themessage("not available yet")
      #browseURL(rv$theurl)
    })
    
    observeEvent(input$startupload, {
      #Todo check first whether pane is already there
      if (!rv$haveuploadpane) {
        tabpid = "upload"
        tabp = tabPanel("Upload data", value=tabpid, {
          tagList( 
            uploadDataUI(tabpid),
            tags$hr(),
            actionButton("adduploadeddata", "Add data to network"))
        })
        # Voeg de tab toeg
        appendTab("visTabSet", tabp, select=TRUE)  
        rv$haveuploadpane = TRUE
        
        # the is the reactive variable from the module that will produce the data to load
        # a list of nodes and links
        rv$theuploadeddata = callModule(uploadData, tabpid)
      }
    })

    # Try to add the loaded nodes and links to the network and restart all
    #
    observeEvent(input$adduploadeddata, {
      #get the data from the module
      thedata = rv$theuploadeddata() 
                        
      restartAll(thedata)
    })
      
    
    # Spawn a frozen viewpane from the main view
    observeEvent(input$switchtoview, {
      node = rv$thecurrentnode
      
      rv$theviewcounter = rv$theviewcounter + 1
      viewid = paste0("view", rv$theviewcounter)
      tabplabel = paste0("View ", rv$theviewcounter)

      tabp = tabPanel(tabplabel, value=viewid, {
                      tagList( frozenViewUI(viewid)
                      )}
      )
      
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, viewid)
      rv$theigraph = znops.copyViewInfo(rv$theigraph, rv$thecurrentview, viewid)
      
      gr <- callModule(frozenView, 
                       viewid,   # module id
                       viewid,
                       rv$thevisgraph
      )

      # Voeg de tab toeg
      appendTab("visTabSet", tabp, select=TRUE)  
      
    })
    
    
  #==========  
  #
  # Export the graph
  #
  output$export <- downloadHandler(
    filename = function() {
      f = paste('network-', Sys.Date(), '.html', sep='')
      rv$themessage = paste0("Export to ", f)
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

  #Starting point menu for showing nodes from different domains.
  output$startpointsmenu <- renderUI({
    tagList(
      selectInput("startingpoint", label=NULL,
                   choices=rv$thenetworkinfo$domains)
      
    )
  })
  
  # Focus de graaf op een set van nodes uit een domein
  observeEvent(input$startingpoint, {
    #browser()
    nodes = V(rv$theigraph)[V(rv$theigraph)$domein == input$startingpoint]$name
    rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
  })
  

# Visual menu settings for node manipulation ------------------------------
  
  getLinkColor <- function(l) {
    rv$thenetworkinfo$linktypecolors[rv$thenetworkinfo$linktypes == l]
  }
  
  getNodeMenuEntryScriptFor <- function(linkname, actionlabel) {
    color = getLinkColor(linkname)
    getNodeMenuEntryScriptForColor(linkname, actionlabel, color, "nodemenuclick")
  }
  
  getNodeMenuEntryScriptForColor <- function(linkname, actionlabel, color, inputevent) {
    buttext = 
      paste0("<button id='",linkname,"' style='border: none;display: inline-block; color: white; background-color:", color, "' type='button'
            onclick ='Shiny.setInputValue(\"", inputevent, "\",\"",linkname, "\", {priority: \"event\"}
            );'>", actionlabel, "</button>")
    buttext
  }
  
  
  defaultNodeSelectMenu <- function() {
    res =  c(getNodeMenuEntryScriptFor("actor", "a"), getNodeMenuEntryScriptFor("use", "u"), 
             getNodeMenuEntryScriptFor("system", "s"),getNodeMenuEntryScriptFor("object", "o"),
             getNodeMenuEntryScriptForColor("all", "*", "black", "nodemenuclick"),
             getNodeMenuEntryScriptForColor("all", "H", "grey", "hidefromview"),
             getNodeMenuEntryScriptForColor("all", "F", "grey", "switchfocus")
    )
  }
  
  output$nodeselectmenu <- renderUI({
    tagList(
      HTML(
        defaultNodeSelectMenu()
      )
    )
    
  })
  
     
  # Dit ook nog eens uitproberen. Graaf niet opnieuw tekenen...
  # visNetwork(nodes, edges) %>%
  #   visEvents(type = "once", startStabilizing = "function() {
  #           this.moveTo({scale:0.1})}") %>%
  #   visPhysics(stabilization = FALSE)
  # 
  
  
  output$graph_panel <- renderVisNetwork({
       if (rv$forcerepaint)
         rv$forcerepaint = FALSE
     
      visual3 = rv$theigraph

      #Focus op de elementen in de view
      visual3 = visual3 - V(visual3)[!vertex_attr(visual3, rv$thecurrentview, V(visual3))]
      visual3 = visual3 - E(visual3)[!edge_attr(visual3, rv$thecurrentview, E(visual3))]
 
      if (input$ongericht) 
        visual3 = as.undirected(visual3, mode="each")

      visual3 = znvis.visNetworkVisualisatieSettings(visual3, rv$thenetworkinfo, input$images, 
                                                     input$showlinks, input$linklabels)

      # see https://www.w3schools.com/howto/howto_css_text_buttons.asp
      
      #cat(znapp.defaultNodeSelectMenu())
      
      V(visual3)$title = HTML( 
        defaultNodeSelectMenu()
      )
      # V(visual3)$title = HTML(
      #   "<button id='l1' style='border: none;display: inline-block;' type='button'
      #       onclick ='Shiny.setInputValue(\"nodemenuclick\",\"actor\",
      #       Math.random());'>A</button>
      #    <button id='l2' style='border: none;display: inline-block;' type='button'
      #       onclick ='Shiny.setInputValue(\"nodemenuclick\",\"use\",
      #       Math.random());'>U</button>")
      data3 <- toVisNetworkData(visual3)

      vnt = visNetwork(nodes=data3$nodes, edges=data3$edges)
      vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=FALSE)

      if (input$navigatie)
        vnt = visInteraction(vnt, navigationButtons = TRUE)
      if (!input$ongericht)
        vnt = visEdges(vnt, arrows="to")
      
      vnt = visEvents(vnt, 
          doubleClick="function (event) {  Shiny.setInputValue(\"doubleClick\", event); }",
          oncontext="function (event) {  Shiny.setInputValue(\"oncontext\", event); }"              
      )
      
      #visPhysics(vnt, stabilization = FALSE)
      rv$thevisgraph = vnt

      #
      # Gebruik van Igraph voor layout, 
      # vnt = visIgraph(visual3, layout=input$layout, smooth=input$smooth) 
      # 
      # vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE)
      # if (input$navigatie)
      #   vnt = visInteraction(vnt, navigationButtons = TRUE)
      # rv$thevisgraph = vnt
      
      vnt
      })
}

shinyApp(ui = ui, server = server)
