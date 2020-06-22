#
# Module for uploading a View
#


uploadViewUI <- function(id, label="Upload View") {
  #browser()
  ns <- NS(id)
  { 
    fileInput(ns("uploadjsonfile"), "Load JSON file", multiple=FALSE)
  }
}

uploadView <- function(input, output, session, viewid, netinfo) {
  
  jsonContents <- reactive({
    jfile = input$uploadjsonfile
    loadresult = list(view = NULL, error="")
    
    if (is.null(jfile)) 
      return(loadresult)
    
    #   browser()
    data = tryCatch({
      readViewFromJSON(jfile$datapath)
    }, warning = function(w) {
      loadresult$error = w
    }, error = function(e) {
      loadresult$error = e
    })
    if (!is.null(data$message) && data$message != "")
      loadresult$error = data$message
    else 
      loadresult$view = data
    
    return(loadresult)
  })
  
  
  
  checkdata <- reactive({
    if (is.null(jsonContents()$view))
      return(NULL)
    newnds = jsonContents()$view$nodes
    existnds = netinfo$nodes
    allnodes = c(newnds$nid, existnds$nid)
    newlks = jsonContents()$edges
    mis = checkNodesInedges(allnodes, newlks)
    list(missing=unique(mis))
  })
  
  output$loadmsg <- renderText({
    checkdata()$loadmsg
  })
  
  viewloader <- reactive({
    list(view=jsonContents()$view, errors=jsonContents()$error, missing=checkdata()$missing)
  })
  
  return(viewloader)
  
}

