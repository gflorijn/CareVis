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
                            actionButton("showdemos", "Examples"),
                            tags$hr(),
                            downloadButton("export", "Export"),
                            tags$hr(),
                            downloadButton("downloadviewasjson", "JSON"),
                            tags$hr(),
                             actionButton(inputId="interrupt", "Interrupt"),
                             tags$hr(),
                            actionButton(inputId="quit", "Quit")
                          )
             ),
             mainPanel(width = 11,
                fluidRow(
                 column(2, checkboxInput("navigation", "Navigation", TRUE)),
                 column(2, checkboxInput("undirected", "Undirected", value=TRUE)),
                 column(2, checkboxInput("images", "Icons", TRUE)),
                 column(2, checkboxInput("linklabels", "Link names", TRUE)),
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
             mainPanel(width=10,
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
  
  tabPanel("Data view - Edges", 
           tagList(
             tags$h2("Edges"),
             tags$br(),
             tableOutput("dataviewedges")
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
      thecurrentviewname = NULL,
      thedatauploader = NULL,
      
      activeview = NULL
      
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
        n = combineNetworks(netinfo, additionaldata)
      }
      n = updateDerivedNetworkInfo(n)
      
#      browser()
      return(n)
    }
    
    # Add stuff like domain lists and colors
    updateDerivedNetworkInfo <- function(net) {
      n = addDerivedNetworkData(net)
      n = extendNetworkInfoForVisualisation(n)
      n = setupVisualDefinitionsForNetwork(n)
      return(n)
    } 
    
    # 
    # `graph_panel_data` is a list of two data frames: one of nodes, one of edges.
    # Deze moeten we initialiseren bij het restart verhaal.
    graph_panel_data = reactiveValues(
      nodes = NULL,
      edges = NULL
    )
    # we initialiseren ze van daaruit met  lege dataframe:
    setGraphPanelData <- function(nds, eds) {
      graph_panel_data$nodes = nds
      graph_panel_data$edges = eds
    }
    
    
    restartAll <- function(additionaldata) {
      #browser()
      ni = loadNetworkInfo(rv$thenetworkinfo, additionaldata)

      rv$thevisgraph = NULL
#      rv$thevispositions = NULL
      rv$thenetworkinfo = ni
      rv$thenodeselected = ""
      rv$themessage = ""
      rv$thecurrentviewname = "Main"
#      browser()
        
      # the is the reactive variable from the module that will produce the data to load
      # a list of nodes and links
      rv$thedatauploader = callModule(uploadData, "upload", "upload", rv$thenetworkinfo)

      nodes = c("Patient")
        
      rv$activeview = newViewOnNetwork(ni, "Main")    
      rv$activeview = addNodesToViewByName(rv$activeview, nodes)

      setGraphPanelData(rv$activeview$nodes, rv$activeview$edges)
      
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

 observeEvent(input$interrupt, {
    browser()
     })

    # handle tabpanel selection event
    #
    observeEvent(input$theAppPage, {
 #     rv$thecurrentviewname = input$theAppPage
    })


# About ----------------------------------------------------------

 
    observeEvent(input$showabout, {
      showModal(modalDialog(
        easyClose = TRUE,
        title = "About NetVis",
        tags$p("An experimental browser for network graphs, in this case communication in the care sector in the Netherlands"),
        tags$p("Gert Florijn, 2020")
      ))
    })



# Demo files --------------------------------------------------------------

        
    observeEvent(input$showdemos, {
      showModal(modalDialog(
        easyClose = TRUE,
        title = "Some Demos",
        tags$p("Tutorial - simple illustration of different kinds of nodes"),
        downloadLink("demotutorialjson", "Download json file"),
        tags$br(),
        actionLink("demotutorialload", "Add to network"),
        tags$p(),
        tags$p("PGO - Illustration of components involved in a PGO (already loaded)"),
        downloadLink("demoPGOjson", "Download json file"),
        tags$br(),
      ))
    })
    
    output$demotutorialjson <- downloadHandler(
        filename <- function() {
          "demo-tutorial-v1.json"
        },
        content <- function(file) {
          file.copy("Demos/tutorial-v1.json", file)
        }
      )
    
    observeEvent(input$demotutorialload, {
      thedata = fromJSON("Demos/tutorial-v1.json")
      restartAll(thedata)
    })

    output$demoPGOjson <- downloadHandler(
      # filename <- function() {
      #   "demo-PGO.json"
      # },
      # content <- function(file) {
      #   file.copy("Data/PGO.json", file)
      # }
    )
    

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
      rv$activeview = restartViewOnNodeNames(rv$activeview, rv$thenodeselected)
    }) 
    
    
    # == UI for node selectie afhandeling
    observeEvent(rv$thenodeselected, {
      rv$themessage = " "
      rv$theurl = ""
      haveurl = rv$thenodeselected != "" && getNodeByName(rv$activeview, rv$thenodeselected)$url != ""
      
      if (haveurl) {
        rv$theurl = getNodeByName(rv$activeview, rv$thenodeselected)$url
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
        growViewByLinks(getNodeNamesInView(rv$activeview), input$viewmenuclick)
    } )
    
    #add nodes by following links of type from nodes
    growViewByLinks <- function(nodenames, lt) {
      linktypes = c(lt)
      if (lt == "all")
        linktypes = rv$thenetworkinfo$linktypes
      for (n in nodenames) {
        rv$activeview = addFriendsOfNodeToView(rv$activeview, n, linktypes)
      }
      #rv$forcerepaint = TRUE
    }
   
    # Force redraw of the graph
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    # hide event
    observeEvent(input$hidefromview, {
      rv$activeview = removeNodesFromViewByName(rv$activeview, c(rv$thenodeselected))
    })
  
    # Focus view on a node
    observeEvent(input$switchfocus, {
      rv$activeview = restartViewOnNodeNames(rv$activeview, c(rv$thenodeselected))
    }) 
    
     # View the whole underlying network
    observeEvent(input$showall, {
      rv$activeview = switchViewToNetwork(rv$activeview)
    }) 
    
    # Should launch a browser for nodes with an URL.
    observeEvent(input$launchbrowser, {
      rv$themessage("not available yet")
      #browseURL(rv$theurl)
    })
    
 

# Data view output --------------------------------------------------------

    output$dataviewnodes <- renderTable({
      rv$activeview$net$nodes
      #flattenedDataFrameForTable(rv$activeview$nodes$name)
    })    
    
    output$dataviewedges <- renderTable({
#      browser()
      rv$activeview$net$edges
      #flattenedDataFrameForTable(rv$activeview$edges$eid)
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
      
 #     rv$theigraph = initializeViewOnGraph(rv$theigraph, viewid)
 #     rv$theigraph = znops.copyViewInfo(rv$theigraph, rv$thecurrentviewname, viewid)
      
      gr <- callModule(frozenView, viewid, viewid, rv$activeview)

      # Voeg de tab toeg
      appendTab("theAppPage", tabp, select=TRUE)  
      
    })
    


# Export the graph --------------------------------------------------------

    #Todo: Remove this
    removeInternalColums <- function(nodeslinks) {
      nd = nodeslinks$nodes
      nl = nodeslinks$edges

      nd$brokenImage = NULL
      nd$image = NULL
      return(list(nodes=nd, links=nl))
    }
    
  output$downloadviewasjson = downloadHandler(
      filename <- function() {
        "currentview.json"
      },
      content <- function(file) {
         d = rv$activeview()
        writeLines(
          toJSON(
           r , pretty=T, rownames = FALSE), file)
      }
      
    )
    
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
      rv$activeview = restartViewOnNodesFromDomain(rv$activeview, nd) 
    }
  })
  

# Search node box + action handling ---------------------------------------

  #Starting point menu for showing nodes from different domains.
  output$searchnodemenu <- renderUI({
    #    browser()
    names= rv$activeview$net$nodes$name
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
      rv$activeview = addNodesToViewByName(rv$activeview, nodes)
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
    res =  c(
             getNodeMenuEntryScriptFor("actor", "a"), 
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


# Event Experiments -------------------------------------------------------


  # observeEvent(input$beforedrawing, {
  # })
  # 
  # observeEvent(input$afterdrawing, {
  # })
  

# Output rendering for the graph panel ------------------------------------
  
  # Splits de output naar 2 niveaus
  # 1) afhankelijkheid van igraph
  # 2) afhankelijkheid van de nodes en edge set die het Visnetwork gebruikt
  # Zie https://stackoverflow.com/questions/54846529/how-to-extract-and-save-visnetwork-manipulation-changes-in-shiny
  

  
  # Dit is observer die kijkt naar de igraph en veranderingen vertaalt in scherm updates
  #
  observeEvent( {
          rv$activeview
          rv$forcerepaint},  {

    if (rv$forcerepaint) { #called when the redraw action has been activated
      cat('Force repaint\n')
      rv$forcerepaint = FALSE
    }
#browser()
    # Prepare the graph for visualisation
    #
    rv$activeview= addVisualSettingsForView(rv$activeview, input$images, input$linklabels)

    # #activate this to show the node-action menu when hovering over the node
    # V(viewg)$title = HTML(
    #   singleNodeSelectMenu()
    # )

      # Save the graph for future use
      # rv$theviewigraph = viewg  #n this is different from the visgraph. Decide what to use in spawning views.


      # allows users to give a specific name to a node. Moet weg - geen igraph meer

      graph_panel_data$nodes = rv$activeview$nodes
      graph_panel_data$edges = rv$activeview$edges
   })
  
  #todo
  #- save changes in a list and carry them through after a save button on a special menu
  #- add a clone node function based on add new Nodes
  
  
  
  #Make a new node give the id
  createNewNodeForIdWithDefaults <- function(nid) {
    return(tibble(id=nid, name=nid, icon="", url="", groups="", domain="UI", nodetype="undefined"))
  }
 
  createNewEdgeWithDefaults <- function(from,to) {
    return(tibble(from=from, to=to,  label="", linktype="refer", eid=getEidForEdge(from,to,"") ))
  }

  # If the user edits the graph, this shows up in
  # `input$[name_of_the_graph_output]_graphChange`.  This is a list whose
  # members depend on whether the user added a node or an edge.  The "cmd"
  # element tells us what the user did.
  observeEvent(
    input$graph_panel_graphChange,
    {
      cmd = input$graph_panel_graphChange$cmd
      #cat("cmd = ", cmd, "\n")
      if (cmd == "addNode") {
        nid = input$graph_panel_graphChange$id # this is the visnetwork internal id
        nlabel = input$graph_panel_graphChange$label # this is the name we will use as id
        newnode = createNewNodeForIdWithDefaults(nlabel)
        rv$activeview = addNewNodesToNetworkAndView(rv$activeview, newnode)
        rv$activeview$net = updateDerivedNetworkInfo(rv$activeview$net)
        rv$thenetworkinfo = rv$activeview$net
      }
      else if (cmd == "addEdge") {
        nfrom = input$graph_panel_graphChange$from
        nto = input$graph_panel_graphChange$to
        newedge = createNewEdgeWithDefaults(nfrom, nto)
        rv$activeview = addNewEdgesToNetworkAndView(rv$activeview, newedge)
        rv$activeview$net = updateDerivedNetworkInfo(rv$activeview$net)
        rv$thenetworkinfo = rv$activeview$net
      }
    })
  
  
 
  # Render the graph.
  output$graph_panel <- renderVisNetwork({

      vnt = visNetwork(nodes=graph_panel_data$nodes, edges=graph_panel_data$edges)
  
      # Allow interaction - note: nodes can be in multiple groups
      # Allow maniuplation - should be switch?
      vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE, manipulation = TRUE,
                       selectedBy=list(variable = "groups", multiple = TRUE))
  
      if (input$igraphlayout) {
        vnt = visIgraphLayout(vnt, type="full")
      }
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
