#
# Module for frozen view panes
#

frozenViewUI <- function(id) {
  ns <- NS(id)
  { 
    tagList(
      visNetworkOutput(ns("viewdrawnet"), height="500px", width="100%"),
      downloadButton(ns("viewexport"), "Export this view")
    )
  }
}

frozenView <- function(input, output, session, viewid, visnet) {
  
  browser()
  # cat("in aparte view viewid = ", viewid, " graaf =\n")
  # print(graaf)
  
  output$viewexport <- downloadHandler(
    filename = function() {
      f = paste('view-', Sys.Date(), '.html', sep='')
      f
    },
    content = function(con) {
      visSave(visnet, file=con)      
    }
  )
  
  output$viewdrawnet <- renderVisNetwork({
    g3 = visnet
    g3
  })
}

