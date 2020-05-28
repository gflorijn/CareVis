library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)

source("ZorgnetData.R")
source("ZorgnetOps.R")
source("ZorgnetVisualisatie.R")
source("frozenview.R")

# Nodig om het browser window te sluiten
#
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

znapp.mogelijkestartpunten = zndef.domeinen
znapp.defaultnavrootnode = "Patienten"
znapp.mainPaneName = "Main"

znapp.basenetall = zndef.netall

getLinkColor <- function(l) {
  znvis.linksoorten.kleuren[znvis.linksoorten == l]
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


znapp.defaultNodeSelectMenu <- function() {
  res =  c(getNodeMenuEntryScriptFor("actor", "a"), getNodeMenuEntryScriptFor("use", "u"), 
                           getNodeMenuEntryScriptFor("system", "s"),getNodeMenuEntryScriptFor("object", "o"),
                           getNodeMenuEntryScriptForColor("all", "*", "black", "nodemenuclick"),
                           getNodeMenuEntryScriptForColor("all", "H", "grey", "hidefromview"),
                           getNodeMenuEntryScriptForColor("all", "F", "grey", "switchfocus")
           )
  
}



#
# De shiny app voor visualisaties
#
ui <- fluidPage(theme=shinytheme("simplex"),
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
                      selectInput("startingpoint", label="View...", 
                                   choices=znapp.mogelijkestartpunten, selected=znapp.defaultnavrootnode),
                      tags$hr(),
                      actionButton("showgraph", "Redraw"),
                      actionButton("restart", "Restart"),
                      tags$hr(),
                      actionButton(inputId="quit", "Quit")
                   )
                 ),
        ),
        
        mainPanel(
          fluidRow(
            column(2, checkboxInput("navigatie", "Navigation", TRUE)),
            column(2, checkboxInput("ongericht", "Undirected", value=TRUE)),
            column(2, checkboxInput("images", "Icons", TRUE)),
            column(2, checkboxInput("showlinks", "Links", TRUE)),
            column(2, checkboxInput("linklabels", "Link names", FALSE)),
            column(2, checkboxInput("smooth", "Smooth")),
          ),
          fluidRow(
            column(3, verbatimTextOutput("nodemessage")),
            column(9, uiOutput("graphbrowsemenu")) #only visible when the main view is active
          ),          
          tabsetPanel(id="visTabSet",
                tabPanel(znapp.mainPaneName, 
                         visNetworkOutput("graph_panel", height="600px", width="100%"),
                         tags$hr(),
                         fluidRow(
                           column(9, verbatimTextOutput("generalmessage")),
                           column(3, actionButton("launchbrowser", "Launch browser"))
                         )
                         
                ),
                tabPanel("loadpanel",
                     fluidRow(
                       fileInput("uploadnodes", "Load Nodes", multiple=TRUE),
                       tags$hr(),
                       tableOutput("nodecontents"),
                       tags$hr(),
                       fileInput("uploadlinks", "Load Links", multiple=TRUE),
                       tags$hr(),
                       tableOutput("linkcontents"),
                       tags$hr(),
                       actionButton("adduploadednodes", "Add nodes"),
                       actionButton("adduploadedlinks", "Add links")
                     )
                )
          ),
       ),
          
    )
)


server <- function(input, output, session) {
  
    # 
    # De "state" variabelen van de applicatie
   
    rv <- reactiveValues(
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
      thenodesread = NULL,
      thelinksread = NULL
    )
    
    restartAll <- function() {
      rv$thenodeselected = ""
      rv$thecurrentview = znapp.mainPaneName
      rv$theigraph = znapp.basenetall
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, rv$thecurrentview)
      #  cat('initin\n')
      # print(rv$theigraph)
      
      nodes = V(rv$theigraph)[V(rv$theigraph)$domein == znapp.defaultnavrootnode]$name
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
    }
    
    #
    # Initialiseer de data die de view bepaalt.
    isolate ({
      restartAll()
      })
    
    # Quit button
    observeEvent(input$quit, {
      js$closeWindow()
      stopApp()
    })
    
    #restart - load everything from the start
    #
    observeEvent(input$restart, {
      restartAll()
    })
    
    output$nodecontents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$uploadnodes
      if (is.null(inFile))
        return(NULL)
      
      rv$thenodesread = read.csv2(inFile$datapath, header=T, colClasses="character", sep=";")
      rv$thenodesread
    })

    output$linkcontents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$uploadlinks
      if (is.null(inFile))
        return(NULL)
      
      rv$thelinksread = read.csv2(inFile$datapath, header=T, colClasses="character", sep=";")
      rv$thelinksread
    })
    
    observeEvent(input$adduploadednodes, {
      r = rv$thenodesread
      r$naam = r$id
#browser()
      g = rv$theigraph
      for (i in 1:nrow(r)) {
        n=r[i,]
        cat('add ', n$id, '\n')
browser()
      g = add_vertices(g, 1, id=n$id, domein=n$domein, nodetype=n$nodetype, lijn=n$lijn, belang=n$belang, url=n$url, naam=n$naam)
      }
      browser()
      rv$theigraph = g
    })

    observeEvent(input$adduploadedlinks, {
      
    })
    
    
    # handle tabpanel selection event
    #
    observeEvent(input$visTabSet, {
      rv$thecurrentview = input$visTabSet
    })
    
    output$graphbrowsemenu <- renderUI({
      if (rv$thecurrentview == znapp.mainPaneName) {
         tagList(
           column(9,
                 HTML(
                   znapp.defaultNodeSelectMenu()
                 ),
                 # actionButton("hidefromview", "Hide"),
                 # actionButton("switchfocus", "Focus"),
                 HTML("&nbsp;&nbsp;   Network: "),
                 actionButton("growfocusall", "Grow"),
                 actionButton("showall", "All"),
                 actionButton("switchtoview", "> View"),
                 downloadButton("export", "Export")
            )
         )
      }
      else {
        ""
      }
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
          tags$p("Browsen is simpel: selecteer een node en kies welke verbanden (soorten links) je 
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
            tags$li("F(ocus) focusseert de view op deze node"),
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
    
    # 
    # Dit is de event handler als de grafiek getekend is.
    # Goede plek om the bevriezen? Dat kan via de proxy
    # 
    observeEvent(input$graph_panel_initialized, {
        p = visNetworkProxy(input$graph_panel)
#        cat('Initialized proxy \n')
        rv$thegraphproxy = p
    })

   
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
    
    
    
    # Rechter muis knop. Nu nog geen actie aan verbonden
    observeEvent(input$oncontext, {
    }) 
    
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
      nodes = c(label)
      if (label == "all")
        nodes = znvis.linksoorten
      rv$theigraph = znops.voegVriendenToeAanView(rv$theigraph,rv$thecurrentview, rv$thenodeselected, nodes)
    }
    
    
     
    # Focus de graaf op een set van nodes uit een domein
    observeEvent(input$startingpoint, {
      #browser()
      nodes = V(rv$theigraph)[V(rv$theigraph)$domein == input$startingpoint]$name
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
    })

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
                                                    zndef.linksoorten)
        
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
    

    observeEvent(input$switchtoview, {
      node = rv$thecurrentnode
      
      rv$theviewcounter = rv$theviewcounter + 1
      viewid = paste0("view", rv$theviewcounter)
      tabpid = paste0("View ", rv$theviewcounter)

      tabp = tabPanel(tabpid, {
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

      visual3 = znvis.visNetworkVisualisatieSettings(visual3, input$images, 
                                                     input$showlinks, input$linklabels)

      # see https://www.w3schools.com/howto/howto_css_text_buttons.asp
      
      #cat(znapp.defaultNodeSelectMenu())
      
      V(visual3)$title = HTML( 
        znapp.defaultNodeSelectMenu()
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
