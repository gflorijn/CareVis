#
# Module for frozen view panes
#


uploadDataUI <- function(id, label="Upload Data") {
  #browser()
  ns <- NS(id)
  { 
    tagList(
      fluidRow(
        column(6, fileInput(ns("uploadnodesfile"), "Load Nodes", multiple=FALSE)),
        column(6, fileInput(ns("uploadlinksfile"), "Load Links", multiple=FALSE))
      ),
      tags$hr(),
      fluidRow(
        column(6, tableOutput(ns("nodecontents"))),
        column(6, tableOutput(ns("linkcontents")))
      ),
      fluidRow(
        column(12, tableOutput(ns("loadmsg"))),
      ),
    ) 
  }
}

uploadData <- function(input, output, session, viewid, netinfo) {
  nodesFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$uploadnodesfile, message = FALSE))
    input$uploadnodesfile
  })
  
  linksFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$uploadlinksfile, message = FALSE))
    input$uploadlinksfile
  })
  
  nodesData <- reactive({
    read.csv2(nodesFile()$datapath, header=T, colClasses="character", sep=";")
  })

  linksData <- reactive({
    read.csv2(linksFile()$datapath, header=T, colClasses="character", sep=";")
  })  
  
  output$nodecontents <- renderTable({
    nodesData()
  })
  
  output$linkcontents <- renderTable({
    linksData()
  })
  

  checkdata <- reactive({
    newnds = nodesData()$id
    existnds = netinfo$rawnodes$id
    allnodes = c(newnds, existnds)
    newlks = linksData()
#    browser()
    
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
    list(nodes=nodesData(), links=linksData(), errors=checkdata()$haveerrors)
  })
  
  return(loadedData)
  
}

