library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)

library(DT)

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
           div(
             class = "outer",
             tags$head(includeCSS("styles.css")),
             
             sidebarLayout(
               sidebarPanel(
                 width = 1,
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
                   actionButton(inputId = "interrupt", "Interrupt"),
                   tags$hr(),
                   actionButton(inputId = "quit", "Quit")
                 )
               ),
               mainPanel(
                 width = 11,
                 fluidRow(
                   column(2, checkboxInput("navigation", "Navigation", TRUE)),
                   column(2, checkboxInput("undirected", "Undirected", value =
                                             TRUE)),
                   column(2, checkboxInput("images", "Icons", TRUE)),
                   column(2, checkboxInput("linklabels", "Link names", TRUE)),
                   column(2, checkboxInput("igraphlayout", "iGraph layout"))
                   # column(2, checkboxInput("smooth", "Smooth"))
                 ),
                 fluidRow(
                   column(2, uiOutput("startpointsmenu")),
                   column(3, uiOutput("searchnodemenu")),
                   column(2, textInput("nodefield", label = NULL)),
                   column(2, uiOutput("singlenodeselectmenu")),
                   column(3, uiOutput("viewnodeselectmenu")),
                   # column(1, uiOutput("viewselectmenu"))
                 ),
                 visNetworkOutput("graph_panel", height = "700px", width =
                                    "100%"),
                 absolutePanel(
                   id = "editcontrols",
                   class = "panel panel-default",
                   top = 145,
                   left = 320,
                   width = 420,
                   fixed = TRUE,
                   draggable = TRUE,
                   height = "auto",
                   tagList(
                     uiOutput("editmodemenu")
                  )
                 )
               )
           ))), 
  tabPanel("Upload",
           sidebarLayout(
             sidebarPanel(width = 2,
                          tagList(
                            actionButton("adduploadeddata", "Add data to network")
                          )),
             mainPanel(width = 10,
                       uploadDataUI("upload"),)
           )),
  tabPanel("Data view - Nodes",
           tagList(tags$h2("Nodes"),
                   tags$br(),
                   DT::dataTableOutput("dataviewnodes"))),
  
  tabPanel("Data view - Edges",
           tagList(tags$h2("Edges"),
                   tags$br(),
                   DT::dataTableOutput("dataviewedges"))),
  
  tabPanel("Help",
           tagList(helpPageText()))
  
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
      ni = loadNetworkInfo(rv$thenetworkinfo, additionaldata)

      rv$thevisgraph = NULL
      rv$thenetworkinfo = ni
      rv$thenodeselected = ""
      rv$themessage = ""
      rv$thecurrentviewname = "Main"

            # the is the reactive variable from the module that will produce the data to load
      # a list of nodes and links
      rv$thedatauploader = callModule(uploadData, "upload", "upload", rv$thenetworkinfo)

      nodes = c("Patient")
        
      rv$activeview = newViewOnNetwork(ni, "Main")    
      rv$activeview = addNodesToViewById(rv$activeview, nodes)

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


    # the graph panel has been initialized - can set up inputs to get from them
    # 
    observeEvent(input$graph_panel_initialized, {
    })
# 
    
    # Node selection - see visEvents
    observeEvent(input$select_current_nodes,  {
      #cat("select_current_nodes ", input$select_current_nodes, "\n")
      rv$thenodeselected = input$select_current_nodes
      updateTextInput(session, "nodefield", value = rv$thenodeselected)
    })
    
    #Edge selection - see visEvents
    observeEvent(input$select_current_edges,  {
      #cat("select_current_edges ", input$select_current_edges, "\n")
      rv$theedgeselected = input$input$select_current_edges
      updateTextInput(session, "nodefield", value = rv$thenodeselected)
    })
    
    
    # Selection of a group - not used
    #  observeEvent(input$graph_panel_selectedBy, {
    #    cat("Observe-graph_panel_selectedBy ", input$graph_panel_selectedBy, "\n")
    #  })
    
    # Right mouse click - not used
    # observeEvent(input$oncontext, {
    # }) 
    
    # Double click not used
    # observeEvent(input$doubleClick, {
    #   #browser()
    #   rv$thenodeselected = input$doubleClick$nodes[[1]]
    #   rv$activeview = restartViewOnNodeIds(rv$activeview, rv$thenodeselected)
    # }) 
    
    
    # == UI for node selection handling
    observeEvent(rv$thenodeselected, {
      rv$themessage = " "
      rv$theurl = ""
      if (is.null(rv$thenodeselected) | rv$thenodeselected == "")
          return()
      rv$theurl = getNodeById(rv$activeview, rv$thenodeselected)$url
      haveurl = FALSE
      if (rv$theurl != "") {
        haveurl = TRUE
        rv$themessage = paste0("Zie voor meer informatie ", rv$theurl)
      }
      toggleState("launchbrowser", haveurl) #does not work on deployed apps
    })
    
    #react to click on linkmenu for selected node - the event has the link type to follow
    observeEvent(
      input$nodemenuclick,
      {
        if (is.null(rv$thenodeselected) | rv$thenodeselected == "") {
          return
        }
      linktypes = c(input$nodemenuclick)
      if (linktypes == "all")
        linktypes = rv$activeview$net$linktypes
      rv$activeview = addFriendsOfNodeToView(rv$activeview, c(rv$thenodeselected), linktypes)
} )
    
    #click on linkmenu for all nodes in view
    observeEvent(input$viewmenuclick, {
      linktypes = c(input$viewmenuclick)
      if (linktypes == "internal") {
        rv$activeview = addEdgesBetweenNodesInView(rv$activeview)
      }
      else {
        if (linktypes == "all") #todo: simplify this
          linktypes=rv$activeview$net$linktypes
        rv$activeview = addFriendsAndEdgesOfNodesInView(rv$activeview, linktypes)
      }
    } )
    
    # Force redraw of the graph
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    # hide event
    observeEvent(input$hidefromview, {
      rv$activeview = removeNodesFromViewById(rv$activeview, c(rv$thenodeselected))
    })
  
    # Focus view on a node
    observeEvent(input$switchfocus, {
      rv$activeview = restartViewOnNodeIds(rv$activeview, c(rv$thenodeselected))
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
    
#

# Data view output --------------------------------------------------------
 
    output$dataviewnodes <-  DT::renderDataTable( 
      select(rv$activeview$net$nodes, nid, label, nodetype, domain, groups, icon, url),
              style="Bootstrap", rownames=F, 
              server=T, selection="single", options=list(pageLength=20)
    )
    
    output$dataviewedges <- DT::renderDataTable(
#      browser()
      select(rv$activeview$net$edges,from, to, label, linktype, eid),
        style="Bootstrap", rownames=F, 
        server=T, selection="single", options=list(pageLength=20)
    )
    

# Editbox for node(s) or edges -----------------------------------------------------
    
    editablenodetable = reactiveValues(
      nodes = NULL
    )
    
    observeEvent(input$startnodeeditor, {
      editablenodetable$nodes = select(rv$activeview$nodes, nid, label, nodetype, domain, groups, icon, url)
      showModal(
        modalDialog(
          title = "Edit node(s)",
          size="l",
          tagList(
            DT::dataTableOutput("editnodetable", width="90%"),
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("saveeditednodes", "Save")
          )
        )
      )
    })
    
    # Save the changes to the network - should watch out for changes to id's.
    # Should keep the original nodes, removebyId, and then add the changes
    # in addition: these steps occur on multiple places (clone, etc) , should be handled generically.
    observeEvent(input$saveeditednodes, {
      browser()
      net = view$net
      net = addNodesToNetwork(net, editablenodetable$nodes)
      net = updateDerivedNetworkInfo(net) # add presentation stuff
      view$net = net
      rv$thenetwerkinfo = net  # Should not be here
      view = addNodesToViewById(view, newnode$nid)
      return(view)
    })

    observeEvent(input$editnodetable_cell_edit, {
      info = input$editnodetable_cell_edit
      cat('caught table change\n')
      str(info)  # check what info looks like (a data frame of 3 columns)
      editablenodetable$nodes <<- editData(editablenodetable$nodes, info)
      # replaceData(proxy5, d5, resetPaging = FALSE)  # important
      # the above steps can be merged into a single editData() call; see examples below
    })

    # The table for the modal node editor
    #
    output$editnodetable <- DT::renderDataTable(      
      editablenodetable$nodes,
                style="Bootstrap", rownames=F,  editable= "cell", #list(target="cell", disable = list(columns=c(1))), 
                server=T, selection="single", options=list(pageLength=10))
    
    
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
  output$searchnodemenu <- renderUI({  # probably should be a list of labels...
    #    browser()
    names= rv$activeview$net$nodes$nid
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
      rv$activeview = addNodesToViewById(rv$activeview, nodes)
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
             getMenuEntryScriptForColor("all", "F", "grey", "switchfocus"),
             getMenuEntryScriptForColor("all", "C", "grey", "editclonenode")
    )
  }
  
  viewNodesSelectMenu <- function() {
    res =  c(getViewMenuEntryScriptFor("actor", "a"), 
             getViewMenuEntryScriptFor("system", "s"),
             getViewMenuEntryScriptFor("use", "u"), 
             getViewMenuEntryScriptFor("object", "o"),
             getViewMenuEntryScriptFor("part", "p"), 
             getViewMenuEntryScriptFor("refer", "r"),
             getMenuEntryScriptForColor("internal", "+", "black", "viewmenuclick"),
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
  
  editModeMenu  <- function() {
    res = c(
      getMenuEntryScriptForColor("all", "Save changes", "grey", "editmodesavechanges"),
      getMenuEntryScriptForColor("all", "Cancel", "grey", "editmodecancel"),
      getMenuEntryScriptForColor("all", "Launch editor", "grey", "startnodeeditor")
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

  output$editmodemenu <- renderUI({
    tagList(
      fixedRow(
        column(2, offset=1, checkboxInput("manipulationmode", "Draw mode", FALSE)),
        column(5, tagList(
          tags$small("Changes"),
          tags$br(),
          HTML(
            c(      getMenuEntryScriptForColor("all", "Save changes", "grey", "editmodesavechanges"),
                    getMenuEntryScriptForColor("all", "Cancel", "grey", "editmodecancel")
            )
          ))),
        column(4, tagList(
          tags$small("Editing"),
          tags$br(),
          HTML(
            getMenuEntryScriptForColor("all", "Launch editor", "grey", "startnodeeditor")
          )
        ))
      ))
  })
  
# Event Experiments -------------------------------------------------------


  # observeEvent(input$beforedrawing, {
  # })
  # 
  # observeEvent(input$afterdrawing, {
  # })
  

# Node Editor -------------------------------------------------------------


  

# Edit Mode actions -------------------------------------------------------

  observeEvent(input$editclonenode, {
    if (is.null(rv$thenodeselected) | rv$thenodeselected == "")
      return()
    rv$activeview = addCloneOfNodeToView(rv$activeview, rv$thenodeselected)
  })
  
  observeEvent(input$editmodesavechanges, {
    saveEditModeChangelog()
    flushEditModeChangelog()
  })
  
  observeEvent(input$editmodecancel, {
    flushEditModeChangelog()
    rv$forcerepaint = TRUE
  })

  

# Output rendering for the graph panel ------------------------------------

  

  addCloneOfNodeToView <- function (view, name) {
    #browser()
    if (is.null(name) | name == "") {
      return(view)
    }
    n = getNodeById(view, name)
    newnode = createCloneOfNode(view, n)
    newedge = tibble(from=newnode$nid, to=name,  label="", linktype="refer", eid=getEidForEdge(newnode$nid,name,"") )
    
    net = view$net
    net = addNodesToNetwork(net, newnode)
    net = addEdgesToNetwork(net, newedge)
    net = updateDerivedNetworkInfo(net) # add presentation stuff
    view$net = net
    rv$thenetwerkinfo = net  # Should not be here
    view = addNodesToViewById(view, newnode$nid)
    view = addEdgesToViewByEid(view, newedge$eid)
    return(view)
  }
  
  
  #Make a new node give the id
  genNewNodeForIdWithDefaults <- function(view, nid) {
    return(tibble(nid=nid, label=nid, icon="", url="", groups="", image="", domain="UI", nodetype="undefined"))
  }
  
  # create a new edge and handle id translation
  genNewEdgeWithDefaults <- function(view, from,to) {
    fname = if (existsNodeInView(view, from)) from else subset(editmodelog$idmap, id==from)$label
    tname = if (existsNodeInView(view, to)) to else subset(editmodelog$idmap, id==to)$label
    return(tibble(from=fname, to=tname,  label="", linktype="refer", eid=getEidForEdge(fname,tname,"") ))
  }
  
  
  #Capture the changes during an edit session
  #
  editmodelog <- reactiveValues(
      nodes = tibble(),
      edges = tibble(),
      idmap = tibble() # map visNetwork ids to our ids
  )
  
  flushEditModeChangelog <- function() {
    editmodelog$nodes = tibble()
    editmodelog$edges = tibble()
    editmodelog$idmap = tibble()
  }
 
  # mapping from visnetwork id to our label id.
  editModeLogIdMap <- function(id, label) { 
    editmodelog$idmap = bind_rows(editmodelog$idmap, tibble(id = id, label = label))
  }
    
  editModeLogAddNodes <- function(nodes) {
    onodes = editmodelog$nodes
    onodes = bind_rows(onodes, nodes)
    editmodelog$nodes = onodes
  }
  
  editModeLogAddEdges <- function(edges) {
    #browser()
    oedges = editmodelog$edges
    oedges = bind_rows(oedges, edges)
    editmodelog$edges = oedges
  }
  
  saveEditModeChangelog <- function() {
#    browser()
    net = rv$activeview$net
    net = addNodesToNetwork(net, editmodelog$nodes)
    net = addEdgesToNetwork(net, editmodelog$edges)
    net = updateDerivedNetworkInfo(net)
    rv$activeview$net = net
    rv$thenetworkinfo = net
    
    view = rv$activeview
    
    view = addNodesToViewById(view, editmodelog$nodes$nid)
    view = addEdgesToViewByEid(view, editmodelog$edges$eid)
    rv$activeview = view
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
        newnode = genNewNodeForIdWithDefaults(rv$activeview, nlabel)
        editModeLogIdMap(nid, nlabel)
        editModeLogAddNodes(newnode)
      }
      else if (cmd == "addEdge") { 
        
        
        nfrom = input$graph_panel_graphChange$from
        nto = input$graph_panel_graphChange$to
        newedge = genNewEdgeWithDefaults(rv$activeview, nfrom, nto)
        editModeLogAddEdges(newedge)
      }
    })


# Rendering the view -----------------------------------------------------

  
  # The observer watching the current view
  observeEvent( {
    rv$activeview
    rv$forcerepaint},  {
      
      if (rv$forcerepaint) { #called when the redraw action has been activated
        #cat('Force repaint\n')
        rv$forcerepaint = FALSE
      }
      
      flushEditModeChangelog()
      
      #browser()
      # Prepare the graph for visualisation
      #
      rv$activeview= addVisualSettingsForView(rv$activeview, input$images, input$linklabels)
      
      # #activate this to show the node-action menu when hovering over the node
      # V(viewg)$title = HTML(
      #   singleNodeSelectMenu()
      # )
      
      graph_panel_data$nodes = rv$activeview$nodes
      graph_panel_data$edges = rv$activeview$edges
    })
  
  
 
  # Render the graph.
  output$graph_panel <- renderVisNetwork({

      # set "id" to node id/edge id
      nds = graph_panel_data$nodes
      nds$id = nds$nid
      eds = graph_panel_data$edges
      eds$id = eds$eid
      vnt = visNetwork(nodes=nds, edges=eds)
  
      # Allow interaction - note: nodes can be in multiple groups
      # Allow maniuplation - should be switch?
      vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE, manipulation = input$manipulationmode,
                       selectedBy=list(variable = "groups", multiple = TRUE))
  
      if (input$igraphlayout) {
        vnt = visIgraphLayout(vnt, type="full")
      }
      if (input$navigation)
        vnt = visInteraction(vnt, navigationButtons = TRUE)
  
      if (!input$undirected)
        vnt = visEdges(vnt, arrows="to")
    
      vnt = visEvents(vnt,
                      select = "function(data) {
                          Shiny.onInputChange('select_current_nodes', data.nodes);
                          Shiny.onInputChange('select_current_edges', data.edges);
                  ;}")
      
      
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
