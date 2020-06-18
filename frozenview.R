#
# Module for frozen view panes
#

frozenViewUI <- function(id) {
  ns <- NS(id)
  { 
    tagList(
      visNetworkOutput(ns("viewdrawnet"), height="700px", width="100%"),
      downloadButton(ns("viewexport"), "Export this view")
    )
  }
}

frozenView <- function(input, output, session, viewid, theview) {
  
  # browser()
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
    dnodes = theview$nodes
    dnodes$id = dnodes$nid
    dedges = theview$edges
    dedges$id = dedges$eid
    vnt = visNetwork(nodes=dnodes, edges=dedges)
    
    # Allow interaction - note: nodes can be in multiple groups
    # Allow maniuplation
    vnt = visOptions(vnt, nodesIdSelection = TRUE, 
                     selectedBy=list(variable = "groups", multiple = TRUE))
    
    # if (input$navigation)
    #   vnt = visInteraction(vnt, navigationButtons = TRUE)
    # 
    # if (!input$undirected)
    #   vnt = visEdges(vnt, arrows="to")
    vnt
  })
}

