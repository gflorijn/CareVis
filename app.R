library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)

source("netData.R")
source("netOps.R")
source("netVisuals.R")
source("frozenview.R")
source("uploaddata.R")
source("helppage.R")

# Nodig om het browser window te sluiten
#
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
#
# De shiny app voor visualisaties
#
ui <- 
tagList(
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  useShinyjs(),

  navbarPage("NetVis",
             
  #shinythemes::themeSelector(),
  theme=shinytheme("simplex"),
  id = "theAppPage",
  inverse = TRUE,

  footer = tagList(
    fluidRow(
      tags$hr(),
      column(8, offset=2, textOutput("generalmessage"))
      #column(3, actionButton("launchbrowser", "Launch browser"))
    )
  ),

  tabPanel("Main",
           sidebarLayout(
             sidebarPanel(width=1,
                          fluidRow(
                            actionButton("showabout", "About"),
                            tags$hr(),
                            actionButton("showgraph", "Redraw"),
                            tags$hr(),
                            actionButton("restart", "Restart"),
                            tags$hr(),
                            downloadButton("export", "Export"),
                            tags$hr(),
                            actionButton(inputId="quit", "Quit")
                            # tags$hr(),
                            # actionButton(inputId="interrupt", "Interrupt"),
                          )
             ),
             mainPanel(width = 11,
                fluidRow(
                 column(2, checkboxInput("navigation", "Navigation", TRUE)),
                 column(2, checkboxInput("undirected", "Undirected", value=TRUE)),
                 column(2, checkboxInput("images", "Icons", TRUE)),
                 column(2, checkboxInput("showlinks", "Links", TRUE)),
                 column(2, checkboxInput("linklabels", "Link names", FALSE)),
                 column(2, checkboxInput("igraphlayout", "iGraph layout"))
                # column(2, checkboxInput("smooth", "Smooth"))
               ),
               fluidRow(
                 column(2, uiOutput("startpointsmenu")),
                 column(3, uiOutput("searchnodemenu")),
                 column(2, textInput("nodefield", label=NULL)),
                 column(2, uiOutput("singlenodeselectmenu")),
                 column(3, uiOutput("viewnodeselectmenu")),
                 # column(1, uiOutput("viewselectmenu"))
               ),
               tags$hr(),
               visNetworkOutput("graph_panel", height="700px", width="100%")
             )
           )
  ),
  tabPanel("Upload",
           sidebarLayout(
             sidebarPanel(width = 2,
                          tagList(
                            actionButton("adduploadeddata", "Add data to network")
                          )
             ),
             mainPanel(
               uploadDataUI("upload"),
             )
           )
  ),
  tabPanel("Data view - Nodes", 
           tagList(
              tags$h2("Nodes"),
              tags$br(),
              tableOutput("dataviewnodes")
           )),
  
  tabPanel("Data view - Links", 
           tagList(
             tags$h2("Links"),
             tags$br(),
             tableOutput("dataviewlinks")
           )),
  
  tabPanel("Help",
           tagList(
             helpPageText()
           ))
 
)
)

server <- function(input, output, session) {
  

# App wide state variables -------------------------------------------------

    # De "state" variabelen van de applicatie
   
    rv <- reactiveValues(
      thenetworkinfo = NULL,
      theigraph=NULL, 
      thevisgraph = NULL, 
      # thegraphproxy = NULL,
      thenodeselected = NULL, 
      theurl = "",
      themessage = NULL,
      theviewcounter = 0,
      forcerepaint = FALSE,
      thecurrentview = NULL,
      thedatauploader = NULL
    )


# Initialisation ----------------------------------------------------------

        
    layerstoload = c("Patienten", "Zorgaanbieders", "Administratie", "Gegevens",
                     "Interactie", "Systemen","Platformen",  "Standaarden",
                     "Leveranciers", "PGO")
    
    loadNetworkInfo <- function(netinfo, additionaldata) {
      if (is.null(additionaldata)) {
        n = readNetworkData(layerstoload)
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

      rv$thevisgraph = NULL
#      rv$thevispositions = NULL
      rv$thenetworkinfo = ni
      rv$thenodeselected = ""
      rv$themessage = ""
      rv$thecurrentview = "Main"
#      browser()

      # the is the reactive variable from the module that will produce the data to load
      # a list of nodes and links
      rv$thedatauploader = callModule(uploadData, "upload", "upload", rv$thenetworkinfo)

      nodes = c("Patient")
      rv$theigraph = ni$network
      rv$theigraph = initializeViewOnGraph(rv$theigraph, rv$thecurrentview)
      rv$theigraph = restartViewOnNodes(rv$theigraph, rv$thecurrentview, nodes)
      rv$theigraph = setupVisualDefinitionsForGraph(rv$theigraph, rv$thenetworkinfo)
      
      updateTabsetPanel(session, "theAppPage", selected = "Main")
    }
    
    #
    # Initialiseer de data die de view bepaalt.
    isolate ({
      restartAll(NULL)
      })
    
    # Quit button
    observeEvent(input$quit, {
#      js$closeWindow()
      stopApp()
    })
    
    #restart - load everything from the start
    # Todo - hoeft niet de hele graaf opnieuw te lezen
    observeEvent(input$restart, {
      restartAll(NULL)
    })

 # observeEvent(input$interrupt, {
 #    browser()
 #     })
    
    # handle tabpanel selection event
    #
    observeEvent(input$theAppPage, {
 #     rv$thecurrentview = input$theAppPage
    })


# About ----------------------------------------------------------

 
    observeEvent(input$showabout, {
      showModal(modalDialog(
        title = "About NetVis",
        tags$p("An experimental browser for network graphs, in this case communication in the care sector in the Netherlands"),
        tags$p("Gert Florijn, 2020")
      ))
    })



# Node selection and menu handling ----------------------------------------


    # the graph panel has been initialized - set up inputs to get from them
    # 
    observeEvent(input$graph_panel_initialized, {
      # rv$thegraphproxy = visNetworkProxy("graph_panel")
      # rv$thegraphproxy = visGetPositions(rv$thegraphproxy, nodes=NULL, input="graph_panel_positions")
      #rv$thegraphproxy = visGetNodes(rv$thegraphproxy,input="graph_panel_nodes")
    })
# 

    # observeEvent(list(input$graph_panel_positions), {
    #   rv$thevispositions = input$graph_panel_positions
    # })   
     # ==========
    # Selectie van een node - twee methodes. dubbelop?
    
    observeEvent(input$current_node_id,  {
      #cat("input_current_node ", input$current_node_id, "\n")
    })

    observeEvent(input$graph_panel_selected, {
      #cat("Node selected ", input$graph_panel_selected, "\n")
      rv$thenodeselected = input$graph_panel_selected
      updateTextInput(session, "nodefield", value = rv$thenodeselected)
    })
    
    
    # Selectie van een groep, (nog) niet gebruikt
    #  observeEvent(input$graph_panel_selectedBy, {
    #    cat("Observe-graph_panel_selectedBy ", input$graph_panel_selectedBy, "\n")
    #  })
    
    # # Rechter muis knop. Nu nog geen actie aan verbonden
    # observeEvent(input$oncontext, {
    # }) 
    
    # Double click ==> focus on the node selected. Usage set in event-setting 
    observeEvent(input$doubleClick, {
      #browser()
      rv$thenodeselected = input$doubleClick$nodes[[1]]
      rv$theigraph = restartViewOnNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    }) 
    
    
    # == UI for node selectie afhandeling
    observeEvent(rv$thenodeselected, {
      rv$themessage = " "
      rv$theurl = ""
      haveurl = rv$thenodeselected != "" && V(rv$theigraph)[rv$thenodeselected]$url != ""
      
      if (haveurl) {
        rv$theurl = V(rv$theigraph)[rv$thenodeselected]$url
        rv$themessage = paste0("Zie voor meer informatie ", rv$theurl)
      }
      toggleState("launchbrowser", haveurl) #does not work on deployed apps
    })
    
    #react to click on linkmenu for selected node - the event has the link type to follow
    observeEvent(input$nodemenuclick, {
      if (rv$thenodeselected != "")
        growViewByLinks(c(rv$thenodeselected), input$nodemenuclick)
    } )
    
    #click on linkmenu for all nodes in view
    observeEvent(input$viewmenuclick, {
        # browser()
        growViewByLinks(znops.nodesInView(rv$theigraph, rv$thecurrentview), input$viewmenuclick)
    } )
    
    #add nodes by following links of type from nodes
    growViewByLinks <- function(nodes, l) {
      linktypes = c(l)
      if (l == "all")
        linktypes = rv$thenetworkinfo$linktypes
      for (n in nodes) {
        rv$theigraph = addFriendsToView(rv$theigraph, rv$thecurrentview, n, linktypes)
      }
    }
   
    # Force redraw of the graph
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    # hide event
    observeEvent(input$hidefromview, {
      rv$theigraph = znops.verwijderNodesUitView(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    })
  
    # Focus view on a node
    observeEvent(input$switchfocus, {
      rv$theigraph = restartViewOnNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    }) 
    
     # View the whole underlying network
    observeEvent(input$showall, {
      rv$theigraph = znops.toonAllesInView(rv$theigraph, rv$thecurrentview)
    }) 
    
    # Should launch a browser for nodes with an URL.
    observeEvent(input$launchbrowser, {
      rv$themessage("not available yet")
      #browseURL(rv$theurl)
    })
    
 

# Data view output --------------------------------------------------------

    output$dataviewnodes <- renderTable({
      flattenedDataFrameForTable(rv$thenetworkinfo$rawnodes)
    })    
    
    output$dataviewlinks <- renderTable({
#      browser()
      flattenedDataFrameForTable(rv$thenetworkinfo$rawlinks)
    })    
    
    
    
# Handle uploading data ----------------------------------------------------

    # Try to add the loaded nodes and links to the network and restart all
    #
    observeEvent(input$adduploadeddata, {
      #get the data from the module
      thedata = rv$thedatauploader()

      if (thedata$errors) {
        rv$themessage = "Issues in data - update cancelled."
      } else {
        rv$themessage = "Updating network."
        restartAll(thedata)
      }
    })
      


# Spawn current view into separate viewpane -------------------------------

        
    # Spawn a frozen viewpane from the main view
    observeEvent(input$switchtoview, {
      rv$theviewcounter = rv$theviewcounter + 1
      viewid = paste0("view", rv$theviewcounter)
      tabplabel = paste0("View ", rv$theviewcounter)

      tabp = tabPanel(tabplabel, value=viewid, {
                      tagList( frozenViewUI(viewid)
                      )}
      )
      
      rv$theigraph = initializeViewOnGraph(rv$theigraph, viewid)
      rv$theigraph = znops.copyViewInfo(rv$theigraph, rv$thecurrentview, viewid)
      
      gr <- callModule(frozenView, viewid, viewid, rv$thevisgraph)

      # Voeg de tab toeg
      appendTab("theAppPage", tabp, select=TRUE)  
      
    })
    


# Export the graph --------------------------------------------------------

        
  #
  # Export the graph
  #
  output$export <- downloadHandler(
    filename = function() {
      f = paste('network-', Sys.Date(), '.html', sep='')
      rv$themessage = paste0("Export to ", f, ". Warning: icons are not exported.")
      f
    },
    content = function(con) {
      visSave(rv$thevisgraph, file=con, selfcontained=TRUE)  
    }
  )
  


# Output rendering and reaction -------------------------------------------

     
  output$generalmessage <- renderText({
    rv$themessage
  })


# Starting points selector + handling -------------------------------------

    
  #Starting point menu for showing nodes from different domains.
  output$startpointsmenu <- renderUI({
    domains= rv$thenetworkinfo$domains
    selectInput("startingpoint", label=NULL, domains)
    fixedRow(
       selectizeInput("startingpoint", label=NULL, c("Select domain"="", domains))
    )
  })

  # Focus de graaf op een set van nodes uit een domain
  observeEvent(input$startingpoint, {
    #    browser()
    nd = input$startingpoint
    if (!is.null(nd) & nd!= "") {
      nodes = V(rv$theigraph)[V(rv$theigraph)$domain == input$startingpoint]$name
      rv$theigraph = restartViewOnNodes(rv$theigraph, rv$thecurrentview, nodes)
    }
  })
  

# Search node box + action handling ---------------------------------------

  #Starting point menu for showing nodes from different domains.
  output$searchnodemenu <- renderUI({
    #    browser()
    names= V(rv$thenetworkinfo$network)$name
    fixedRow(
        column(9, selectizeInput("addsearchnodes", NULL, c("Search node"="", names), multiple = TRUE)),
        column(3, tagList(
          HTML(
          searchfieldAddMenu()
          )
        ))
      )
  })
  
  observeEvent(input$addsearchnodes, {
    #Don't do anything, add the selected nodes after the addbutton is pressed
  }) 
  
  observeEvent(input$addnodefromsearch, {
    nodes = input$addsearchnodes
    if (!is.null(nodes) & length(nodes) > 0) {
      rv$theigraph = addNodesToView(rv$theigraph, rv$thecurrentview, nodes)
#      rv$forcerepaint = TRUE 
    }
  }) 

# Visual menu settings for node manipulation ------------------------------
  
  getLinkColor <- function(l) {
    rv$thenetworkinfo$linktypecolors[rv$thenetworkinfo$linktypes == l]
  }
  
  getMenuEntryScriptForColor <- function(linkname, actionlabel, color, inputevent) {
    buttext = 
      paste0("<button id='",linkname,"' style='border: none;display: inline-block; color: white; background-color:", color, "' type='button'
            onclick ='Shiny.setInputValue(\"", inputevent, "\",\"",linkname, "\", {priority: \"event\"}
            );'>", actionlabel, "</button>")
    buttext
  }
  
  getNodeMenuEntryScriptFor <- function(linkname, actionlabel) {
    color = getLinkColor(linkname)
    getMenuEntryScriptForColor(linkname, actionlabel, color, "nodemenuclick")
  }
  
  getViewMenuEntryScriptFor <- function(linkname, actionlabel) {
    color = getLinkColor(linkname)
    getMenuEntryScriptForColor(linkname, actionlabel, color, "viewmenuclick")
  }
  
  singleNodeSelectMenu <- function() {
    res =  c(getNodeMenuEntryScriptFor("actor", "a"), 
             getNodeMenuEntryScriptFor("system", "s"),
             getNodeMenuEntryScriptFor("use", "u"), 
             getNodeMenuEntryScriptFor("object", "o"),
             getNodeMenuEntryScriptFor("part", "p"), 
             getNodeMenuEntryScriptFor("refer", "r"),
             getMenuEntryScriptForColor("all", "*", "black", "nodemenuclick"),
             getMenuEntryScriptForColor("all", "H", "grey", "hidefromview"),
             getMenuEntryScriptForColor("all", "F", "grey", "switchfocus")
    )
  }
  
  viewNodesSelectMenu <- function() {
    res =  c(getViewMenuEntryScriptFor("actor", "a"), 
             getViewMenuEntryScriptFor("system", "s"),
             getViewMenuEntryScriptFor("use", "u"), 
             getViewMenuEntryScriptFor("object", "o"),
             getViewMenuEntryScriptFor("part", "p"), 
             getViewMenuEntryScriptFor("refer", "r"),
             getMenuEntryScriptForColor("all", "*", "black", "viewmenuclick"),
#             tags$b(" -- "),
             viewSelectMenu()
    )
  }
  
  searchfieldAddMenu  <- function() {
      res = c(
        getMenuEntryScriptForColor("all", "Add", "grey", "addnodefromsearch")
      )  
  }
  
  viewSelectMenu  <- function() {
      res = c(
        getMenuEntryScriptForColor("all", "All", "grey", "showall"),
        getMenuEntryScriptForColor("all", ">View", "grey", "switchtoview")
      )
  }
  
  
  output$singlenodeselectmenu <- renderUI({
    tagList(
      HTML(
           singleNodeSelectMenu()
        ),      
      tags$br(),
      tags$small("Selected node")
    )
  })
  
  output$viewnodeselectmenu <- renderUI({
    tagList(
      HTML(
        viewNodesSelectMenu()
      ),
      tags$br(),
      tags$small("Nodes in view")
    )
  })
  

  # observeEvent(input$beforedrawing, {
  # })
  # 
  # observeEvent(input$afterdrawing, {
  # })

# Output rendering for the graph panel ------------------------------------

  
   
  output$graph_panel <- renderVisNetwork({
     
      if (rv$forcerepaint) { #called when the redraw action has been activated
        rv$forcerepaint = FALSE
      } 
      

      #Reduce the graph to the elements in the current view
    
      visual3 = rv$theigraph
    
      visual3 = visual3 - V(visual3)[!vertex_attr(visual3, rv$thecurrentview, V(visual3))]
      visual3 = visual3 - E(visual3)[!edge_attr(visual3, rv$thecurrentview, E(visual3))]
      
 
      if (input$undirected) 
        visual3 = as.undirected(visual3, mode="each")

      # Prepare the graph for visualisation
      #
      visual3 = visNetworkVisualisationSettings(visual3, rv$thenetworkinfo, input$images, 
                                                     input$showlinks, input$linklabels)
      
      # #activate this to show the node-action menu when hovering over the node
      # V(visual3)$title = HTML( 
      #   singleNodeSelectMenu()
      # )
      
      if (input$igraphlayout) {
        # Use Igraph for  layout, 
        vnt = visIgraph(visual3)    # layout=input$layout, smooth=input$smooth) 
      } else {
        data3 <- toVisNetworkData(visual3)
        vnt = visNetwork(nodes=data3$nodes, edges=data3$edges)
      }
      
      # Allow interaction - note: nodes can be in multiple groups
      vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE
                       , selectedBy=list(variable = "groupnames", multiple = TRUE))

      if (input$navigation)
        vnt = visInteraction(vnt, navigationButtons = TRUE)
      
      if (!input$undirected)
        vnt = visEdges(vnt, arrows="to")

 #      vnt = visEvents(vnt, 
 #         doubleClick="function (event) {  Shiny.setInputValue(\"doubleClick\", event); }",
 #         oncontext="function (event) {  Shiny.setInputValue(\"oncontext\", event); }",
 #          # beforeDrawing="function (ctx) {  Shiny.setInputValue(\"beforedrawing\", ctx); }",
 #      )
      
      #Older experiments
      # groups = unique(V(visual3)$group)
      #      vnt = visClusteringByGroup(vnt, groups)
      #visPhysics(vnt, stabilization = FALSE)
 
      rv$thevisgraph = vnt

      vnt
      })
}

shinyApp(ui = ui, server = server)
