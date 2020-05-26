library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)

source("ZorgnetData.R")
source("ZorgnetOps.R")
source("ZorgnetVisualisatie.R")

# Nodig om het browser window te sluiten
#
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

znapp.mogelijkestartpunten = zndef.domeinen
znapp.defaultnavrootnode = "Patienten"

znapp.basenetall = zndef.netall

getNodeMenuEntryScriptFor <- function(linkname, actionlabel) {
  color = znvis.linksoorten.kleuren[znvis.linksoorten == linkname]
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
                      # checkboxGroupInput("linksel", "Link types", zndef.linksoorten, selected = znapp.defaultlinks),
                      # checkboxGroupInput("nodesel", "Node types", zndef.nodetypes),
                      # tags$hr(),
                      # selectInput("layout", "Layout opties", znvis.layouts, "layout_nicely"),
                      tags$hr(),
                      actionButton("showgraph", "Redraw"),
                      actionButton("restart", "Restart"),
                      tags$hr(),
                      downloadButton("export", "Export"),
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
            )
          ),
          tabsetPanel(id="visTabSet",
                    tabPanel("Main", 
                             visNetworkOutput("graph_panel", height="600px", width="100%")
                    ) 
          ),
          tags$hr(),
          fluidRow(
            column(9, verbatimTextOutput("generalmessage")),
            column(3, actionButton("launchbrowser", "Launch browser"))
          )
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
      thecurrentview = NULL
    )
    
    restartAll <- function() {
      rv$thenodeselected = ""
      rv$thecurrentview = "main"
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
      # # rv$thecurrentfocus = znapp.defaultnavrootnode
      # rv$thenodeselected = ""
      # rv$thecurrentview = "main"
      # rv$theigraph = znapp.basenetall
      # rv$theigraph = znops.startViewOpGraaf(rv$theigraph, rv$thecurrentview)
      # #  cat('initin\n')
      # # print(rv$theigraph)
      # 
      # nodes = V(rv$theigraph)[V(rv$theigraph)$domein == znapp.defaultnavrootnode]$name
      # rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
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
  

    
    observeEvent(input$showabout, {
      showModal(modalDialog(
        title = "About",
        tags$p("Een experimentele browser van informatieuitwisseling in de zorg"),
        tags$p("Gert Florijn, 2020")
      ))
    })

    observeEvent(input$showhelp, {
      showModal(modalDialog(
        title = "Help",
        "Todo"
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
      toggleState("launchbrowser", haveurl)
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
      browseURL(rv$theurl)
    })
    

    observeEvent(input$switchtoview, {
      node = rv$thecurrentnode
      
      rv$theviewcounter = rv$theviewcounter + 1
      viewid = paste0("view", rv$theviewcounter)
      tabpid = paste0("View ", rv$theviewcounter)

      tabp = tabPanel(tabpid, {
                      tagList( aparteViewUI(viewid)
                      )}
      )
      
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, viewid)
      rv$theigraph = znops.copyViewInfo(rv$theigraph, rv$thecurrentview, viewid)
      
      gr <- callModule(aparteView, 
                       viewid,   # module id
                       "",
                       session,  # nodig om visualisatiesettings te kunnen gebruiken in de viewmodule
                       viewid,
                       rv$thevisgraph
      )

      # Voeg de tab toeg
      appendTab("visTabSet", tabp, select=TRUE)  
      
    })
    
    
    aparteViewUI <- function(id) {
      ns <- NS(id)
      { 
        visNetworkOutput(ns("viewdrawnet"), height="500px", width="100%") 
      }
    }
    
    aparteView <- function(input, output, session, parentsession, graaf, viewid, visnet) {
      
      # browser()
      # cat("in aparte view viewid = ", viewid, " graaf =\n")
      # print(graaf)
      
       output$viewdrawnet <- renderVisNetwork({
          g3 = visnet
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
