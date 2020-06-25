library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(editData)

library(DT)

source("netData.R")
source("netOps.R")
source("netVisuals.R")
source("uploadView.R")
source("helppage.R")


smallHTMLUIButton <- function(label, eventid, eventdata, color) {
  buttext = 
    HTML(
      paste0("<button id='",eventdata,"' style='border: none;display: inline-block; color: white; background-color:", 
              color, "' type='button'
                onclick ='Shiny.setInputValue(\"", eventid, "\",\"", eventdata, "\", {priority: \"event\"}
                );'>", label, "</button>")
    )
    buttext
}



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

  navbarPage(
    smallHTMLUIButton("NetVis", "showabout", "", "darkblue"),
             
  #shinythemes::themeSelector(),
  theme=shinytheme("spacelab"),
  id = "theAppPage",
  inverse = TRUE,

  tabPanel("Browser",
           div(
             class = "outer",
             tags$head(includeCSS("Styles.css")),
             
             sidebarLayout(
               sidebarPanel(
                 width = 1,
                 verticalLayout(
                   # actionButton("showabout", "About", width="90%"),
                   # tags$hr(),
                   # actionButton("uploadview", "Open"),
                   # tags$hr(),
                   # downloadButton("downloadviewasjson", "Save"),
                   # tags$hr(),
                   actionButton("showgraph", "Redraw"),
                   tags$hr(),
                   # actionButton("restart", "Restart"),
                   # tags$hr(),
                   actionButton(inputId = "interrupt", "Interrupt"),
                   tags$hr(),
                   actionButton(inputId = "quit", "Quit")
                 )
               ),
               mainPanel(
                 width = 11,
                 fixedRow(
                   column(11, uiOutput("statusbar"))
                 ),
                 fixedRow(
                   column(3, uiOutput("slicesmenu")),
                   column(3, uiOutput("searchnodemenu")),
                   column(3, uiOutput("singlenodeselectmenu")),
                   column(3, uiOutput("viewnodeselectmenu"))
                 ),
                 visNetworkOutput("graph_panel", height = "750px", width = "100%"),
                 absolutePanel(id = "visualeditcontrols",
                   class = "panel panel-default",
                   top = 155, left = 370,  width = 1200, fixed = TRUE,
                   draggable = TRUE,
                   height = "auto",
                   fixedRow(
                      column(12, uiOutput("visualeditmenu"))
                   )
                 )
              )
            )
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
           tagList(
             helpPageText(),
             tags$hr(),
             actionButton("showdemos", "Some examples"),
           ))
)
)


# Server part -------------------------------------------------------------



server <- function(input, output, session) {
  

# App wide state variables -------------------------------------------------

   
    rv <- reactiveValues(
      thenetworkinfo = NULL,
      thenodeselected = NULL, 
      theedgeselected = NULL,
      thevisnet = NULL,
      theurl = "",
      themessage = NULL,
      theviewcounter = 0,
      forcerepaint = FALSE,
      thedatauploader = NULL,
      activeview = NULL
    )

    

# Undo handling -----------------------------------------------------------
    
    undolog = reactiveValues(
      previouspoint = NULL
    )
    
    setUndoPoint <- function() {
      undolog$previouspoint = rv$activeview
    }
    
    doUndo <- function() {
      x = rv$activeview
      rv$activeview = undolog$previouspoint
      undolog$previouspoint = x
    }
    
# Initialisation ----------------------------------------------------------

        
    slicestoload = c("Patienten", "Zorgaanbieders", "Administratie", "Gegevens",
                     "Interactie", "Systemen","Platformen",  "Standaarden",
                     "Leveranciers", "PGO")
    
    
    initializeApplication <- function(networkdata) {
      rv$thenetworkinfo = networkdata
      rv$thenodeselected = ""
      rv$themessage = ""
      # the is the reactive variable from the module that will produce the data to load
      rv$thedatauploader = callModule(uploadView, "upload", "upload", rv$thenetworkinfo)
      
      nodes = c("Patient")
      rv$activeview = newViewOnNetwork(rv$thenetworkinfo, "aView")    
      rv$activeview = addNodesToViewById(rv$activeview, nodes)
      
      setUndoPoint()
      
      updateTabsetPanel(session, "theAppPage", selected = "Browser")
      return
    }
     
    restartBrowserOnViewData <- function(viewdata) {
      # laad de data uit newview in het netwerk
      
      setUndoPoint()
      
      rv$thenetworkinfo = combineNetworks(rv$thenetworkinfo, viewdata)
      newview = newViewOnNetwork(rv$thenetworkinfo, viewdata$info$name)
      newview = addNodesToViewById(newview, viewdata$nodes$nid)
      newview = addEdgesToViewByEid(newview, viewdata$edges$eid)
      newview$info = viewdata$info

      rv$activeview = newview
      rv$forcerepaint = TRUE
      
      updateTabsetPanel(session, "theAppPage", selected = "Browser")
      
      return(rv$activeview)
    }
    
    #
    # Initialise the application
    isolate ({
      n = readNetworkData(slicestoload)
      initializeApplication(n)
    })
    


# Graph-panel setup  --------------------------------------------------
    
    # 
    # `graph_panel_data holds the data to interact with the drawing in the graph_panel
    #  Proxy is set when the graph_panel has been initialized
    #  Initialized is set when the visnetwork/graph-panel-data has been initially filled with data
    #
    graph_panel_data = reactiveValues(
      nodes = NULL,
      edges = NULL,
      proxy = NULL,
      initialized = FALSE
    )
    
    
    # Controls that govern the presentation of the graph
    #
    visualcontrols = reactiveValues(
      arrows = FALSE,
      images = TRUE,
      linklabels = TRUE
    )
    

    # Create the visnetwork visualiser
    #
    output$graph_panel <- renderVisNetwork({
      
      nds = tribble(~id, ~label, ~nodetype, ~domain, ~groups, ~icon, ~url)
      eds = tribble(~id, ~from, ~to, ~label, ~linktype)

      vnt = visNetwork(nodes = nds, edges = eds)
      
      vnt = visOptions(vnt,
        nodesIdSelection = TRUE,
        collapse = FALSE,
        manipulation = TRUE,
        selectedBy = list(variable = "groups", multiple = TRUE)
      )
      vnt = visInteraction(vnt, 
        navigationButtons = TRUE)
      vnt = visEvents(vnt,
        select = "function(data) {
                          Shiny.onInputChange('select_current_nodes', data.nodes);
                          Shiny.onInputChange('select_current_edges', data.edges);
                  ;}"
      )
      rv$thevisnet = vnt
      
      vnt
    })
    
    
    # the graph panel has been initialized, we can set the proxy and setup info to retrieve
    #
    observeEvent(input$graph_panel_initialized, {
      graph_panel_data$proxy = visNetworkProxy("graph_panel")
      visGetNodes(graph_panel_data$proxy)
      visGetEdges(graph_panel_data$proxy)
      visGetPositions(graph_panel_data$proxy)
      visGetViewPosition(graph_panel_data$proxy)
    })
    
    # Setup the visNetwork settings based on visualcontrols 
    # Currently unused
    #
    observeEvent({
      graph_panel_data$proxy
      visualcontrols
    }, {
      if (is.null(graph_panel_data$proxy)) {
        return(NULL)
      }
      
      #nothing to do...
    })


# Drawing the view data to the visnetwork view  ---------------------------

    
    # Whenever the active view changes, this will push new graph_panel nodes/edges to the visNetwork
    # the graph_panel nodes have the additional visual attributes needed for visNetwork
    #         View      Graph _panel/vis network
    # nodes   nid       id
    # edges   eid       id
    # 
    observeEvent( {
      visualcontrols
      visualcontrols$arrows
      visualcontrols$images
      visualcontrols$linklabels
      graph_panel_data$proxy
      rv$activeview
      rv$forcerepaint},  {
        
      #cat('Draw active view...\n')
        if (rv$forcerepaint) { #called when the redraw action has been activated
          #cat('Force repaint\n')
          rv$forcerepaint = FALSE
        }
        
        if (is.null(graph_panel_data$proxy)) {
          #cat('Graph panel drawing, proxy not ready yet\n')
          return(NULL)
        }
        
        xn = map_dfr(rv$activeview$nodes$nid, makeGraphPanelNodeForNid)
        xe = map_dfr(rv$activeview$edges$eid, makeGraphPanelEdgeForEid)
        
        if (!graph_panel_data$initialized) {
          #cat('initial draw\n')
          graph_panel_data$initialized = TRUE
          # visSetData(graph_panel_data$proxy, xn, xe)
          visUpdateNodes(graph_panel_data$proxy, xn)
          visUpdateEdges(graph_panel_data$proxy, xe)
          graph_panel_data$nodes = xn
          graph_panel_data$edges = xe
          return
        }
        
        if(isTRUE(all.equal(graph_panel_data$nodes, xn)) & isTRUE(all.equal(graph_panel_data$edges, xe))) {
          #cat("The two data frames are the same! don't do anything.\n")
          return
        }
        
        if (nrow(xn) == 0) {
            xn = defaultNodeTibble()
            visRemoveNodes(graph_panel_data$proxy, graph_panel_data$nodes$nid)
        } 
        else {
          if (nrow(graph_panel_data$nodes) == 0) {
            visUpdateNodes(graph_panel_data$proxy, xn)
          }
          else {
            if (!isTRUE(all.equal(graph_panel_data$nodes, xn))) {
              # res = compareDF::compare_df(xn, graph_panel_data$nodes, group_col="nid", stop_on_error=FALSE)
              # browser()
              oldns = graph_panel_data$nodes$nid
              newns = xn$nid
              rms = oldns[!(oldns %in% newns)]
              visRemoveNodes(graph_panel_data$proxy, rms)
              nws =  newns[!(newns %in% oldns)]
              visUpdateNodes(graph_panel_data$proxy, subset(xn, nid %in% nws))
              pchgs = newns[!(newns %in% nws)]
              olds = subset(graph_panel_data$nodes, nid %in% pchgs)
              news = subset(xn, nid %in% pchgs)
              if (!isTRUE(all.equal(olds, news))) {
                visUpdateNodes(graph_panel_data$proxy, news)
              }
            }
          }
        }
             
        if (nrow(xe) == 0) {
          visRemoveEdges(graph_panel_data$proxy, graph_panel_data$edges)
          xe = defaultEdgeTibble()
        }
        else {
          if (nrow(graph_panel_data$edges) == 0) {
            visUpdateEdges(graph_panel_data$proxy, xe)
          }
          else {
#            browser()
            if (!isTRUE(all.equal(graph_panel_data$edges, xe))) {
              # res = compareDF::compare_df(xe, graph_panel_data$edges, group_col="eid", stop_on_error=FALSE)
              # browser()
              oldes = graph_panel_data$edges$eid
              newes = xe$eid
              rme = oldes[!(oldes %in% newes)]
              visRemoveEdges(graph_panel_data$proxy, rme)
              nwe = newes[!(newes %in% oldes)]
              visUpdateEdges(graph_panel_data$proxy, subset(xe, eid %in% nwe))
              pchgs = newes[!(newes %in% nwe)]
              olds = subset(graph_panel_data$edges, eid %in% pchgs)
              news = subset(xe, eid %in% pchgs)
              if (!isTRUE(all.equal(olds, news))) {
                visUpdateEdges(graph_panel_data$proxy, news)
              }
            }
          }
        }
      visStabilize(graph_panel_data$proxy, NULL)
      visFit(graph_panel_data$proxy)

        # browser()
        
        graph_panel_data$nodes = xn
        graph_panel_data$edges = xe
        
      })
 
    makeGraphPanelNodeForNid <- function(nid) {
      # browser()
      row =  addVisualSettingsToNode(rv$activeview, getNodeById(rv$activeview, nid), visualcontrols$images)
      row$id = row$nid
      return(row)
    }
    
    makeGraphPanelEdgeForEid <- function(eid) {
      row = addVisualSettingsToEdge(rv$activeview, getEdgeByEid(rv$activeview, eid), visualcontrols$linklabels, visualcontrols$arrows,)
      row$id = row$eid
      # not needed, we're working on copy
      # row$orglabel = row$label #hack attempt to support switch show label
      # row$label = row$vislabel
      return(row)
    }
    
    addGraphPanelNodeForNid <- function(nid) {
      graph_panel_data$nodes = bind_rows(graph_panel_data$nodes,makeGraphPanelNodeForNid(nid))
    }
    
    addGraphPanelEdgeForEid <- function(eid) {
      graph_panel_data$edges = bind_rows(graph_panel_data$edges, makeGraphPanelEdgeForEid(eid))
    }
    
    
    
# Main menu options -------------------------------------------------------

    
    # Quit button
    observeEvent(input$quit, {
#      js$closeWindow()
      stopApp()
    })
    
    #restart - reinitialize the browser without loading the files
    observeEvent(input$restart, {
      initializeApplication(rv$thenetworkinfo)
    })

    # Only for debugging
    observeEvent(input$interrupt, {
      browser()
    })

        # Force redraw of the graph
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    
    # handle tabpanel selection event
    #
    observeEvent(input$theAppPage, {
 #     rv$thecurrentviewname = input$theAppPage
    })

    # handle actions related to "about" and "demos"
    
    observeEvent(input$showabout, {
      showModal(modalDialog(
        easyClose = TRUE,
        title = "About NetVis",
        tags$p("An experimental browser for network graphs, in this case communication in the care sector in the Netherlands"),
        tags$p("Gert Florijn, 2020")
      ))
    })
    
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
        # downloadLink("demoPGOjson", "Download json file"),
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
      restartBrowserOnViewData(thedata)
    })
    
    # output$demoPGOjson <- downloadHandler(
    #   # filename <- function() {
    #   #   "demo-PGO.json"
    #   # },
    #   # content <- function(file) {
    #   #   file.copy("Data/PGO.json", file)
    #   # }
    # )
    

# Node selection and menu handling ----------------------------------------

    
    # Node selection - see visEvents
    observeEvent(input$select_current_nodes,  {
      #cat("select_current_nodes ", input$select_current_nodes, "\n")
      rv$thenodeselected = input$select_current_nodes
    })
    
    #Edge selection - see visEvents
    observeEvent(input$select_current_edges,  {
#      cat("select_current_edges ", input$select_current_edges, "\n")
      rv$theedgeselected = input$select_current_edges
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
     })

# Data view output --------------------------------------------------------
 
    output$dataviewnodes <-  DT::renderDataTable(
      #select(rv$activeview$nodes, nid, label, nodetype, domain, groups, icon, url),
      rv$activeview$net$nodes,
              style="Bootstrap", rownames=F, 
              server=T, selection="single", options=list(pageLength=20)
    )
    
    output$dataviewedges <- DT::renderDataTable(
#      browser()
      # select(rv$activeview$edges,from, to, label, linktype, eid),
      rv$activeview$net$edges,
        style="Bootstrap", rownames=F, 
        server=T, selection="single", options=list(pageLength=20)
    )
    

# Handle uploading data ----------------------------------------------------

    observeEvent(input$uploadview, {
      showModal(modalDialog(
        easyClose = TRUE,
        title = "Load view",
        tagList(
          uploadViewUI("upload"), #Call module done during initialization
          actionButton("adduploadeddata", "Done"),
        )
      ))
    })
    
    # Try to add a view
    #
    observeEvent(input$adduploadeddata, {
      #get the data from the module

      removeModal()
      thedata = rv$thedatauploader()
      
      # browser()

      rv$themessage = thedata$errors
      if (is.null(thedata$view))
          return()
      
      if (!is.null(thedata$missing)) {
        #add default nodes for missing
        rv$themessage = "Issues in data - fixing and updating."
        for (i in thedata$missing) {
          thedata$view$nodes = bind_rows(thedata$view$nodes, createNewUndefinedNode(i))
        }
      } else {
        rv$themessage = "No issues - updating network."
      }
      restartBrowserOnViewData(thedata$view)
      
    })


# Settings and handling for status bar menu -------------------------------


  output$statusbar <- renderUI({
    tagList(
      fixedRow(
        column(3,htmlOutput("activeviewtext")),
        column(3,htmlOutput("activeviewmenu")),
        column(5,htmlOutput("generalmessagetext"))
      ),
        
    )
  })
  
  output$activeviewtext <- renderUI({
    m = paste0("Active view: <b>", rv$activeview$info$name, "</b")    
    HTML(m)
  })  
  
  output$activeviewmenu <- renderUI({
    tagList(
      fixedRow(
        div(
            actionBttn("saveview", label="Save view (as)", size="xs", icon=icon("save",lib="font-awesome"), color="default"),
            HTML("&nbsp;"), HTML("&nbsp;"),
            actionBttn("uploadview", label="Open view", size="xs", icon=icon("folder-open-o",lib="font-awesome"), color="default"),
            HTML("&nbsp;"),HTML("&nbsp;"),
            actionBttn("restart", label="New view", size="xs", icon=icon("plus-square-o",lib="font-awesome"), color="default")
       )
      ),
      fixedRow(
        HTML("&nbsp;")
      )
    )
  })

  output$generalmessagetext <- renderUI({
    m = paste0("Messages: ", rv$themessage, "")
    HTML(m)
  })

  
  observeEvent(input$saveview, {
    showModal(modalDialog(
      easyClose = TRUE,
      title = "Save view",
      tagList(
        textInput("newfilename", label="Save view as ", value=rv$activeview$info$name),
        downloadButton("downloadviewasjson", "Save")
      )
    ))
  })
  
  
  output$downloadviewasjson = downloadHandler(
    filename <- function() {
      removeModal()
      if (!is.null(input$newfilename) & input$newfilename != "")
        rv$activeview$info$name = input$newfilename
      return(paste0(rv$activeview$info$name, ".json"))
    },
    content <- function(file) {
      d = rv$activeview
      # if (nrow(d$nodes) > 0)
      #   d$nodes = select(d$nodes, nid, label, nodetype, domain, groups, icon, url)
      # if (nrow(d$edges) > 0)
      #   d$edges = select(d$edges,from, to, label, linktype, eid)
      d$net = NULL
      writeLines(
        toJSON(
          d , pretty=T, rownames = FALSE), file)
    }
    
  )


# Node/Edge change handling -----------------------------------------------

  
  addCloneOfNodeToView <- function (view, name) {
    #browser()
    if (is.null(name) | name == "") {
      return(view)
    }
    n = getNodeById(view, name)
    newnode = createCloneOfNode(view, n)
    # Could add reference to the "original"
    # newedge = tibble(from=newnode$nid, to=name,  label="", linktype="refer", eid=getEidForEdge(newnode$nid,name,"") )
    return(addNewNodeToView(view, newnode))
  }
  
  
  addNewNodeToView <- function(view, newnode) {   
    net = view$net
    net = addNodesToNetwork(net, newnode)
    #    net = addEdgesToNetwork(net, newedge)
    view$net = net
    rv$thenetwerkinfo = net  # Should not be here
    view = addNodesToViewById(view, newnode$nid)
    #    view = addEdgesToViewByEid(view, newedge$eid)
    return(view)
  }
  
  addNewEdgeToView <- function(view, newedge) {   
    net = view$net
    net = addEdgesToNetwork(net, newedge)
    view$net = net
    rv$thenetwerkinfo = net  # Should not be here
    view = addEdgesToViewByEid(view, newedge$eid)
    return(view)
  }
  
  replaceNodeInView <- function(view, oldnode, newnode) {
    # should handle the case where nid has changed and update edges...
    if (oldnode$nid != newnode$nid)
      cat("Warning: replace node needs to change relations\n")
    #for now:
    return(addNewNodeToView(view, newnode))   
  }
  
  replaceEdgeInView <- function(view, oldedge, newedge) {
    # should handle the case where nid has changed and update edges...
    #for now:
    if (oldedge$eid != newedge$eid)
      cat("Warning: replace edge needs to check relations\n")
    return(addNewEdgeToView(view, newedge))   
  }

# Support for simple edit-modals for nodes and edges ----------------------

  nep_editor_state = reactiveValues(
    original = NULL
  )
  
  # Launch editor on existing node
  observeEvent(input$exist_node_editor, {
   if (!is.null(rv$thenodeselected) & rv$thenodeselected != "") {
     nodeChangeModal(getNodeById(rv$activeview, rv$thenodeselected), "nep_edit_node_done")
   }
  })
  
  # Launch editor on existing edge
  observeEvent(input$exist_edge_editor, {
    if (!is.null(rv$theedgeselected) & rv$theedgeselected != "") {
      # browser()
       edgeChangeModal(getEdgeByEid(rv$activeview, rv$theedgeselected), "nep_edit_edge_done")
    }
   })


  newNodeEditor <- function(shape) {
    node = createNewUndefinedNode("new")
    if (!is.null(shape) && shape != "")
      node$shape = shape
    nodeChangeModal(node, "nep_new_node_done")
  }
  
  # Launch editor on new node. The value of the event is the shape of the node.
  observeEvent(input$new_node_editor, { newNodeEditor("") })
  observeEvent(input$new_node_editor_box, { newNodeEditor("box") })
  observeEvent(input$new_node_editor_dot, { newNodeEditor("dot") })
  observeEvent(input$new_node_editor_text, { newNodeEditor("text") })
  
  # Launch editor on new edge
  observeEvent(input$nep_edit_edge_done, {
    removeModal()
    newedge = nep_editor_state$original
    newedge$from=input$nep_from
    newedge$to=input$nep_to
    newedge$label=input$nep_label
    newedge$linktype=input$nep_linktype
    newedge$eid=getEidForEdge(input$nep_from, input$nep_to, input$nep_label)
    rv$activeview = replaceEdgeInView(rv$activeview, nep_editor_state$original, newedge)
  })  
  
  
  observeEvent(input$nep_edit_node_done, {
    removeModal()
    newnode = saveEditChangesToNode(nep_editor_state$original)
    rv$activeview = replaceNodeInView(rv$activeview, nep_editor_state$original, newnode)
  })  

  observeEvent(input$nep_new_node_done, {
    removeModal()
    newnode = saveEditChangesToNode(nep_editor_state$original)
    rv$activeview = addNewNodeToView(rv$activeview, newnode)
  })
  
  saveEditChangesToNode <- function(n) {
    newnode = n
    newnode = nep_editor_state$original
    newnode$nid=input$nep_nid
    newnode$label=input$nep_label
    newnode$nodetype=input$nep_nodetype 
    newnode$icon=input$nep_icon
    newnode$domain=input$nep_domain
    newnode$groups=input$nep_groups
    newnode$url=input$nep_url
    return(newnode)
  }
  
  edgeChangeModal <- function(edge, actionlabel) {
    nep_editor_state$original = edge
    showModal(modalDialog(
      title = "Edge editor",
      easyClose = TRUE,
      size="m",
      footer = fixedRow(actionButton(actionlabel, "Done"), modalButton("Cancel")),
      
      fixedRow(column(3, HTML("from:")), column(7, textInput("nep_from", value = edge$from , label=NULL))),
      fixedRow(column(3, HTML("to:")), column(7, textInput("nep_to", value = edge$to , label=NULL))),
      fixedRow(column(3, HTML("label:")), column(7, textInput("nep_label", value = edge$label , label=NULL))),
      fixedRow(column(3, HTML("linktype:")), column(7, textInput("nep_linktype", value = edge$linktype , label=NULL))),
      # fixedRow(column(3, HTML("domain:")), column(7, textInput("nep_domain", value = node$domain , label=NULL))),
      # fixedRow(column(3, HTML("groups:")), column(7, textInput("nep_groups", value = node$groups , label=NULL))),
      # fixedRow(column(3, HTML("url:")), column(7, textInput("nep_url", value = node$url , label=NULL))),
    )
    )
  }
  
  nodeChangeModal <- function(node, actionlabel) {
    nep_editor_state$original = node
    showModal(modalDialog(
      title = "Node editor",
      easyClose = TRUE,
      size="m",
      footer = fixedRow(actionButton(actionlabel, "Done"), modalButton("Cancel")),

      fixedRow(column(3, HTML("nid:")), column(7, textInput("nep_nid", value = node$nid , label=NULL))),
      fixedRow(column(3, HTML("label:")), column(7, textInput("nep_label", value = node$label , label=NULL))),
      fixedRow(column(3, HTML("nodetype:")), column(7, textInput("nep_nodetype", value = node$nodetype , label=NULL))),
      fixedRow(column(3, HTML("icon:")), column(7, textInput("nep_icon", value = node$icon , label=NULL))),
      fixedRow(column(3, HTML("domain:")), column(7, textInput("nep_domain", value = node$domain , label=NULL))),
      fixedRow(column(3, HTML("groups:")), column(7, textInput("nep_groups", value = node$groups , label=NULL))),
      fixedRow(column(3, HTML("url:")), column(7, textInput("nep_url", value = node$url , label=NULL))),
     )
    )
  }
  

# Starting points selector + handling -------------------------------------

    
  #Starting point menu for showing nodes from different domains.
  output$slicesmenu <- renderUI({
    slices= getDomains(rv$thenetworkinfo)
#    selectInput("startingpoint", label=NULL, domains)
    fixedRow(
      column(8, 
        tags$small(selectizeInput("startingpoint", label=NULL, c("Select slice"="", slices)))),
      column(4,
       actionBttn("addslicetoview", label="+", size="xs", color="default"), 
       actionBttn("focusviewonslice", label="F", size="xs", color="default")) 
    )
  })

 observeEvent(input$startingpoint, { #don't do anything, action done in command events below.
  })

  observeEvent(input$focusviewonslice, {
    nd = input$startingpoint
    if (!is.null(nd) & nd!= "") {
      setUndoPoint()
      rv$activeview = restartViewOnNodesFromDomain(rv$activeview, nd) 
    }
  }) 
 
  observeEvent(input$addslicetoview, {
    nd = input$startingpoint
    if (!is.null(nd) & nd!= "") {
      setUndoPoint()
      rv$activeview = addNodesFromDomainToView(rv$activeview, nd) 
    }
  }) 
  

# Search node box + action handling ---------------------------------------

  #Search node(s) to add to view
  output$searchnodemenu <- renderUI({  # probably should be a list of labels...
    #    browser()
    names= rv$activeview$net$nodes$nid
    fixedRow(
         column(8,
            tags$small(selectizeInput("addsearchnodes", NULL, c("Search node"="", names), multiple = TRUE))),
         column(4,
                actionBttn("addnodefromsearch", label="+", size="xs", color="default"), 
                actionBttn("focusnodefromsearch", label="F", size="xs", color="default")) 
    )
  })
  
  observeEvent(input$addsearchnodes, {
    #Don't do anything, add the selected nodes after the addbutton is pressed
  }) 
  
  observeEvent(input$addnodefromsearch, {
    nodes = input$addsearchnodes
    if (!is.null(nodes) & length(nodes) > 0) {
      setUndoPoint()
      rv$activeview = addNodesToViewById(rv$activeview, nodes)
        #      rv$forcerepaint = TRUE 
    }
  }) 
  observeEvent(input$focusnodefromsearch, {
    nodes = input$addsearchnodes
    if (!is.null(nodes) & length(nodes) > 0) {
      setUndoPoint()
      rv$activeview = restartViewOnNodeIds(rv$activeview, nodes)
      #      rv$forcerepaint = TRUE 
    }
  }) 
  
  
# Settings and action handling for selected node menu ---------------------

  
  getLinkColor <- function(l) {
    return(getEdgeColorForLinktype(rv$thenetworkinfo, l))
  }
  
              
  output$singlenodeselectmenu <- renderUI({
    tagList(
      HTML(
        c (
            smallHTMLUIButton("a", "nodemenuclick", "actor", getLinkColor("actor")),
            smallHTMLUIButton("u", "nodemenuclick", "use", getLinkColor("use")),
            smallHTMLUIButton("s", "nodemenuclick", "system", getLinkColor("system")),
            smallHTMLUIButton("o", "nodemenuclick", "object", getLinkColor("object")),
            smallHTMLUIButton("r", "nodemenuclick", "refer", getLinkColor("refer")),
            smallHTMLUIButton("*", "nodemenuclick", "all", "black")
        )
      ),
      actionBttn("switchfocus", label="F", size="xs", color="default"),
      tags$br(),
      tags$small(htmlOutput("selectionfield"))
    )
  })
  
  # Onder node select menu
  output$selectionfield <- renderUI({
    m = paste0("Selected node: <b>", ifelse(!is.null(rv$thenodeselected), rv$thenodeselected, "(none)"), "</b>")
    HTML(m)
  })
  
  #react to click on linkmenu for selected node - the event has the link type to follow
  observeEvent(input$nodemenuclick, {
    if (is.null(rv$thenodeselected) | rv$thenodeselected == "") {
      return
    }
    linktypes = c(input$nodemenuclick)
    if (linktypes == "all")
      linktypes = getLinkTypes(rv$activeview$net)
    setUndoPoint()
    rv$activeview = addFriendsOfNodeToView(rv$activeview, c(rv$thenodeselected), linktypes)
  } )
  
  # hide event
  observeEvent(input$removenode, {
    if (is.null(rv$thenodeselected) | rv$thenodeselected == "") {
      return
    }
    setUndoPoint()
    rv$activeview = removeNodesFromViewById(rv$activeview, c(rv$thenodeselected))
  })

  # hide event
  observeEvent(input$removeedge, {
    if (is.null(rv$theedgeselected) | rv$theedgeselected == "") {
      return
    }
#    browser()
    setUndoPoint()
    rv$activeview = removeEdgesFromViewById(rv$activeview, c(rv$theedgeselected))
  })
  
  # Focus view on a node
  observeEvent(input$switchfocus, {
    setUndoPoint()
    rv$activeview = restartViewOnNodeIds(rv$activeview, c(rv$thenodeselected))
  }) 
  
  # View the whole underlying network
  observeEvent(input$showall, {
    setUndoPoint()
    rv$activeview = switchViewToNetwork(rv$activeview)
  }) 
  
  # Should launch a browser for nodes with an URL.
  observeEvent(input$launchbrowser, {
    rv$themessage("not available yet")
    #browseURL(rv$theurl)
  })
  
  
  observeEvent(input$editclonenode, {
    if (is.null(rv$thenodeselected) | rv$thenodeselected == "")
      return()
    rv$activeview = addCloneOfNodeToView(rv$activeview, rv$thenodeselected)
  })
  
# Settings and action handling for view node menu -------------------------

  
  output$viewnodeselectmenu <- renderUI({
    tagList(
      HTML(
        c (
          smallHTMLUIButton("a", "viewmenuclick", "actor", getLinkColor("actor")),
          smallHTMLUIButton("u", "viewmenuclick", "use", getLinkColor("use")),
          smallHTMLUIButton("s", "viewmenuclick", "system", getLinkColor("system")),
          smallHTMLUIButton("o", "viewmenuclick", "object", getLinkColor("object")),
          smallHTMLUIButton("r", "viewmenuclick", "refer", getLinkColor("refer")),
          smallHTMLUIButton("#", "viewmenuclick", "internal", "black"),
          smallHTMLUIButton("*", "viewmenuclick", "all", "black")
        )
      ),
      actionBttn("showall", label="All", size="xs", color="default"), 
      tags$br(),
      tags$small("Nodes in view")
    )
  })


  
  #click on linkmenu for all nodes in view
  observeEvent(input$viewmenuclick, {
    #cat('input menu click: ', input$viewmenuclick, '\n')
    linktypes = c(input$viewmenuclick)
    if (linktypes == "internal") {
      setUndoPoint()
      rv$activeview = addEdgesBetweenNodesInView(rv$activeview)
    }
    else {
      if (linktypes == "all") #todo: simplify this
        linktypes=getLinkTypes(rv$thenetworkinfo)
      setUndoPoint()
      rv$activeview = addFriendsAndEdgesOfNodesInView(rv$activeview, linktypes)
    }
  } )
  
# Settings and action handling for draw mode ---------------------

  # If the user edits the graph (in manipulation mode), this shows up in
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
        visRemoveNodes(graph_panel_data$proxy, nid)
        newnode = genNewNodeForIdWithDefaults(rv$activeview, nlabel)
        rv$activeview = addNewNodeToView(rv$activeview, newnode)
        return
      }
      else if (cmd == "addEdge") { 
        nfrom = input$graph_panel_graphChange$from
        nto = input$graph_panel_graphChange$to
        visRemoveEdges(graph_panel_data$proxy, input$graph_panel_graphChange$id)
        newedge = genNewEdgeWithDefaults(rv$activeview, nfrom, nto)
        rv$activeview = addNewEdgeToView(rv$activeview, newedge)
        return
      }
    })

# Settings and action handling for visual options  ---------------------

   observeEvent({
    input$vo_arrows
    input$vo_images
    input$vo_linklabels
  }, {
    visualcontrols$images = input$vo_images
    visualcontrols$arrows = input$vo_arrows
    visualcontrols$linklabels = input$vo_linklabels
  })


# Visual properties editing -----------------------------------------------

output$visualeditmenu <- renderUI ({
  tagList(
    HTML("&nbsp;"), 
    HTML("Edit"),
    HTML("&nbsp;"), 
    actionBttn("undo", label=NULL, size="xs", icon=icon("undo",lib="font-awesome"), color="danger"),
    HTML("-"),
    actionBttn("removenode", label=NULL, size="xs", icon=icon("cut",lib="font-awesome"), color="danger"),
    actionBttn("removeedge", label=NULL, size="xs", icon=icon("chain-broken",lib="font-awesome"), color="danger"),
    HTML("-"),
    actionBttn("exist_node_editor", label=NULL, size="xs", icon=icon("edit",lib="font-awesome"), color="primary"),
    actionBttn("exist_edge_editor", label=NULL, size="xs", icon=icon("link",lib="font-awesome"), color="primary"),
    HTML("-"),
    actionBttn("editclonenode", label=NULL, size="xs", icon=icon("clone",lib="font-awesome"), color="default"),
    actionBttn("new_node_editor", label=NULL, size="xs", icon=icon("plus-square-o",lib="font-awesome"), color="default"),
    actionBttn("new_node_editor_box", label=NULL, size="xs", icon=icon("square-o",lib="font-awesome"), color="default"),
    actionBttn("new_node_editor_dot", label=NULL, size="xs", icon=icon("circle-o",lib="font-awesome"), color="default"),
    actionBttn("new_node_editor_text", label=NULL, size="xs", icon=icon("text-width",lib="font-awesome"), color="default"),
    HTML("---"),
    actionBttn("ve_grow_size", label=NULL, size="xs", icon=icon("chevron-up",lib="font-awesome"), color="success"),
    actionBttn("ve_shrink_size", label="node", size="xs", icon=icon("chevron-down",lib="font-awesome"), color="success"),
    HTML("-"),
    actionBttn("ve_grow_font", label=NULL, size="xs", icon=icon("chevron-up",lib="font-awesome"), color="success"),
    actionBttn("ve_shrink_font", label="text", size="xs", icon=icon("chevron-down",lib="font-awesome"), color="success"),
    HTML("-"),
    actionBttn("ve_widen_edge", label=NULL, size="xs", icon=icon("chevron-up",lib="font-awesome"), color="success"),
    actionBttn("ve_narrow_edge", label="edge", size="xs", icon=icon("chevron-down",lib="font-awesome"), color="success"),
    HTML("&nbsp;"), 
    HTML("Visual "),
    HTML("&nbsp;"), 
    tags$small(checkboxInput3("vo_images", "Icons", TRUE, width=80)),
    tags$small(checkboxInput3("vo_arrows", "Arrows", FALSE, width=80)),
    tags$small(checkboxInput3("vo_linklabels", "Link labels", TRUE, width=80))
  )
})
  
  # 
   observeEvent(input$undo, {
     doUndo()
   })
   
   observeEvent(input$ve_grow_size, {
     id = rv$thenodeselected
     if (is.null(id) | id == "") 
       return
     node = getNodeById(rv$activeview, id)
     if (!("size" %in% colnames(node)))
       node = add_column(node, size=25)
     newnode = node
     if(is.na(node$size)) {
       newnode$size = 25
     }
     newnode$size = newnode$size+1
     rv$activeview = replaceNodeInView(rv$activeview, node, newnode)
   })
 
   observeEvent(input$ve_shrink_size, {
     id = rv$thenodeselected
     if (is.null(id) | id == "") 
       return
     node = getNodeById(rv$activeview, id)
     if (!("size" %in% colnames(node)))
       node = add_column(node, size=25)
     newnode = node
     if(is.na(node$size)) {
       newnode$size = 25
     }
     newnode$size = newnode$size-1
     rv$activeview = replaceNodeInView(rv$activeview, node, newnode)
   })
   
   
   observeEvent(input$ve_grow_font, {
     id = rv$thenodeselected
     if (is.null(id) | id == "") 
       return
     node = getNodeById(rv$activeview, id)
     if (!("font.size" %in% colnames(node)))
       node = add_column(node, font.size=14)
     newnode = node
     if(is.na(node$font.size)) {
       newnode$font.size = 14
     }
     newnode$font.size = newnode$font.size+1
     rv$activeview = replaceNodeInView(rv$activeview, node, newnode)
   })

   observeEvent(input$ve_shrink_font, {
     id = rv$thenodeselected
     if (is.null(id) | id == "") 
       return
     node = getNodeById(rv$activeview, id)
     if (!("font.size" %in% colnames(node)))
       node = add_column(node, font.size=14)
     newnode = node
     if(is.na(node$font.size)) {
       newnode$font.size = 14
     }
     newnode$font.size = newnode$font.size-1
     rv$activeview = replaceNodeInView(rv$activeview, node, newnode)
   })
 
    observeEvent(input$ve_widen_edge, {
     id = rv$theedgeselected
     if (is.null(id) | id == "") 
       return
     edge = getEdgeByEid(rv$activeview, id)
     if (!("width" %in% colnames(edge)))
       edge = add_column(edge, width=1)
     newedge = edge
     if(is.na(edge$width)) {
       newedge$width = 1
     }
     newedge$width = newedge$width+1
     rv$activeview = replaceEdgeInView(rv$activeview, edge, newedge)
   })

   observeEvent(input$ve_narrow_edge, {
     id = rv$theedgeselected
     if (is.null(id) | id == "") 
       return
     if (!("width" %in% colnames(edge)))
       edge = add_column(edge, width=1)
     edge = getEdgeByEid(rv$activeview, id)
     newedge = edge
     if(is.na(edge$width)) {
       newedge$width = 2
     }
     newedge$width = newedge$width-1
     rv$activeview = replaceEdgeInView(rv$activeview, edge, newedge)
   })
}

shinyApp(ui = ui, server = server)
