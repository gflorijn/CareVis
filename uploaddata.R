#
# Module for uploading
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
        column(6, div(tableOutput(ns("nodecontents")), style="font-size:80%")),
        column(6, div(tableOutput(ns("edgecontents")), style="font-size:80%"))
      ),
    ) 
  }
}
uploadData <- function(input, output, session, viewid, netinfo) {
  
  jsonContents <- reactive({
    jfile = input$uploadjsonfile
    if (is.null(jfile)) 
      return(NULL)
 #   browser()
    newnet = tryCatch({
      readNetworkDataFromJSON(jfile$datapath)
    }, warning = function(w) {
      w
    }, error = function(e) {
      e
    })
    return(newnet)
  })

      
  output$nodecontents <- renderTable({
    if (!is.null(jsonContents()))
      jsonContents()$nodes
      #flattenedDataFrameForTable(jsonContents()$nodes)
    else
      return(NULL)
  })
  
  output$edgecontents <- renderTable({
    if (!is.null(jsonContents()))
      jsonContents()$edges
      #flattenedDataFrameForTable(jsonContents()$edges)
    else
      return(NULL)
  })
  

  checkdata <- reactive({
    if (is.null(jsonContents()))
        return(NULL)
    newnds = jsonContents()$nodes
    existnds = netinfo$nodes
    allnodes = c(newnds$nid, existnds$nid)
    newlks = jsonContents()$edges
#browser()    
    haveerrors = FALSE
    msg = ""
    
    # dupn = existnds[existnds %in% newnds]
    # if (length(dupn) > 0) {
    #   msg1 = paste0("Duplicate nodes: ")
    #   msg2 = unique(dupn)
    #   msg = c(msg1, msg2, ". ")
    #   haveerrors = TRUE
    # }
#    browser()
    mis = checkNodesInedges(allnodes, newlks)
    if (length(mis) > 0) {
      msg1 = c(msg, "Warning: missing node definitions for ")
      msg2 = unique(mis)
      msg = c(msg1, msg2, ". Adding them")
    }
    list(haveerrors=haveerrors, loadmsg = msg, missing=unique(mis))
  })
  
  output$loadmsg <- renderText({
    checkdata()$loadmsg
  })
  
  loadedData <- reactive({
    list(nodes=jsonContents()$nodes, edges=jsonContents()$edges, missing=checkdata()$missing)
  })
  
  return(loadedData)
  
}

