#
# Module for frozen view panes
#


uploadDataUI <- function(id, label="Upload Data") {
  ns <- NS(id)
  { 
    tagList(
      fluidRow(
        column(6, fileInput(ns("uploadnodesfile"), "Load Nodes", multiple=TRUE)),
        column(6, fileInput(ns("uploadlinksfile"), "Load Links", multiple=TRUE))
      ),
      tags$hr(),
      fluidRow(
        column(6, tableOutput(ns("nodecontents"))),
        column(6, tableOutput(ns("linkcontents")))
      ),
      tags$hr(),
      fluidRow(
        column(3, actionButton(ns("chlinks"), "Check links")),
        column(9, textOutput(ns("checklinkmessage")))
      )
    ) 
  }
}

uploadData <- function(input, output, session, viewid, networkinfo) {
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
  
  checkresult <- reactiveVal()
  
  observeEvent(input$chlinks, {
    nfl = unique(c(linksData()[["from"]], linksData()[["to"]]))
    nodenames = c(nodesData()[["id"]], networkinfo$nodes$naam)
    missing = !(nfl %in% nodenames)
    result = nfl[missing]
    if (result == "") {
      checkresult("No issues found")
    }
    else {
      checkresult(paste0("Unknown nodes: ", result))
    }
  })
  
  output$checklinkmessage <- renderText ({
    checkresult()
  })
  
  loadedData <- reactive({
    list(nodes=nodesData(), links=linksData())
  })
  
  
  
  return(loadedData)
  
}

