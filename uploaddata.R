#
# Module for frozen view panes
#


uploadDataUI <- function(id, label="Upload Data") {
  #browser()
  ns <- NS(id)
  { 
    tagList(
      fluidRow(
        column(12, verbatimTextOutput(ns("loadmsg"))),
      ),
      tags$hr(),
      fluidRow(
        column(6, fileInput(ns("uploadjsonfile"), "Load JSON file", multiple=FALSE)),
      ),
      tags$hr(),
      fluidRow(
        column(6, tableOutput(ns("nodecontents"))),
        column(6, tableOutput(ns("linkcontents")))
      ),
    ) 
  }
}

uploadData <- function(input, output, session, viewid, netinfo) {
  
  jsonContents <- reactive({
    jfile = input$uploadjsonfile
    if (is.null(jfile)) 
      return(NULL)
    
    fromJSON(jfile$datapath)
  })

      
  output$nodecontents <- renderTable({
    if (!is.null(jsonContents()))
      flattenedDataFrameForTable(jsonContents()$nodes)
    else
      return(NULL)
  })
  
  output$linkcontents <- renderTable({
    if (!is.null(jsonContents()))
      flattenedDataFrameForTable(jsonContents()$links)
    else
      return(NULL)
  })
  

  checkdata <- reactive({
    newnds = jsonContents()$nodes$id
    existnds = netinfo$rawnodes$id
    allnodes = c(newnds, existnds)
    newlks = jsonContents()$links
    
    haveerrors = FALSE
    msg = ""
    
    dupn = existnds[existnds %in% newnds]
    if (length(dupn) > 0) {
      msg1 = paste0("Duplicate nodes: ")
      msg2 = unique(dupn)
      msg = c(msg1, msg2, ". ")
      haveerrors = TRUE
    }
    mis = checkNodesInLinks(allnodes, newlks)
    if (length(mis) > 0) {
      msg1 = c(msg, " Missing node definitions for ")
      msg2 = unique(mis)
      msg = c(msg1, msg2, ".")
      haveerrors = TRUE
    }
    if (!haveerrors) {
      msg = "No issues found."
    }
    list(haveerrors=haveerrors, loadmsg = msg)
  })
  
  output$loadmsg <- renderText({
    checkdata()$loadmsg
  })
  
  loadedData <- reactive({
    list(nodes=jsonContents()$nodes, links=jsonContents()$links, errors=checkdata()$haveerrors)
  })
  
  return(loadedData)
  
}

