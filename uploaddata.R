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
    ) 
  }
}

uploadData <- function(input, output, session, viewid) {
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
  
  loadedData <- reactive({
    list(nodes=nodesData(), links=linksData())
  })
  
  return(loadedData)
  
}

