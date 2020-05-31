library(shiny)
library(igraph)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)

source("ZorgnetData.R")
source("ZorgnetOps.R")
source("ZorgnetVisualisatie.R")
source("frozenview.R")
source("uploaddata.R")
source("helppage.R")

# Nodig om het browser window te sluiten
#
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
#
# De shiny app voor visualisaties
#
ui <- navbarPage("NetVis",
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  useShinyjs(),
  
  #shinythemes::themeSelector(),
  theme=shinytheme("simplex"),
  id = "theAppPage",
  inverse = TRUE,
  # position = "static-top",
  # # 
  # header = tagList(
  #   tags$b("Header")
  # ),
  # 
  # footer = tagList(
  #   tags$b("Footer")
  # ),
  
  tabPanel("Main",
           sidebarLayout(
             sidebarPanel(width=1,
                          fluidRow(
                            actionButton("showabout", "About"),
                            tags$hr(),
                            actionButton("showgraph", "Redraw"),
                            tags$hr(),
                            actionButton("restart", "Restart"),
                            tags$hr(),
                            actionButton(inputId="quit", "Quit")
                            # tags$hr(),
                            # actionButton(inputId="interrupt", "Interrupt"),
                          )
             ),
             mainPanel(
               fluidRow(
                 column(2, checkboxInput("navigatie", "Navigation", TRUE)),
                 column(2, checkboxInput("ongericht", "Undirected", value=TRUE)),
                 column(2, checkboxInput("images", "Icons", TRUE)),
                 column(2, checkboxInput("showlinks", "Links", TRUE)),
                 column(2, checkboxInput("linklabels", "Link names", FALSE)),
                 column(2, checkboxInput("smooth", "Smooth"))
               ),
               fluidRow(
                 column(2, uiOutput("startpointsmenu")),
                 column(2, textInput("nodefield", label=NULL)),
                 column(1, uiOutput("nodefieldops")),
                 column(3, uiOutput("singlenodeselectmenu")),
                 column(2, uiOutput("viewnodeselectmenu")),
                 column(2,
                        uiOutput("viewselectmenu"),

                 )
               ),
               tags$hr(),
               visNetworkOutput("graph_panel", height="600px", width="100%"),
               tags$hr(),
               fluidRow(
                 column(9, verbatimTextOutput("generalmessage")),
                 column(3, actionButton("launchbrowser", "Launch browser"))
               )
             )
           )
  ),
  tabPanel("Upload",
           sidebarLayout(
             sidebarPanel(width = 2,
                          tagList(
                            actionButton("prepareupload", "Prepare upload"),
                            tags$hr(),
                            actionButton("checkuploadeddata", "Verify data"),
                            verbatimTextOutput("uploadcheckmessage"),
                            tags$hr(),
                            actionButton("adduploadeddata", "Add data to network")
                          )
             ),
             mainPanel(
               uploadDataUI("upload"),
             )
           )
  ),
  tabPanel("Data view - Nodes", 
           tagList(
              tags$h2("Nodes"),
              tags$br(),
              tableOutput("dataviewnodes")
           )),
  
  tabPanel("Data view - Links", 
           tagList(
             tags$h2("Links"),
             tags$br(),
             tableOutput("dataviewlinks")
           )),
  
  tabPanel("Help",
           tagList(
             helpPageText()
           ))
 
)

server <- function(input, output, session) {
  
    # 
    # De "state" variabelen van de applicatie
   
    rv <- reactiveValues(
      thenetworkinfo = NULL,
      theigraph=NULL, 
      thevisgraph = NULL, 
      thegraphproxy = NULL,
      thenodeselected = NULL, 
      theurl = "",
      themessage = NULL,
      theviewcounter = 0,
      focusshowsall = FALSE, # True als de huidige weergave de hele database is.
      forcerepaint = FALSE,
      thecurrentview = NULL,
      haveuploadpane = FALSE,
      thenodesread = NULL,
      thelinksread = NULL,
      thedatauploader = NULL,
      theuploadeddata = NULL
    )
    
    #layerstoload = c("Patienten")
    layerstoload = c("Patienten", "Zorgaanbieders", "Administratie", "Gegevens",
                     "Interactie", "Systemen","Platformen",  "Standaarden",
                     "Leveranciers")
    
    loadNetworkInfo <- function(netinfo, additionaldata) {
      if (is.null(additionaldata)) {
        n =  readNetworkData(layerstoload)
      }
      else { # in case of extending with uploaded data
        n = addAdditionalData(netinfo, additionaldata)
      }
      n = addDerivedNetworkData(n)
      n = extendNetworkInfoForVisualisation(n)
#      browser()
      n
    }
    
    restartAll <- function(additionaldata) {
      ni = loadNetworkInfo(rv$thenetworkinfo, additionaldata)
      rv$thenetworkinfo = ni
      rv$thenodeselected = ""
      rv$thecurrentview = "Main"
      rv$theigraph = ni$network
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, rv$thecurrentview)
 #     browser()

      disable("checkuploadeddata")
      disable("adduploadeddata")
      
      # the is the reactive variable from the module that will produce the data to load
      # a list of nodes and links
      #rv$thedatauploader = callModule(uploadData, "upload", "upload")
      
      nodes = c("Patient")
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
      updateTabsetPanel(session, "theAppPage", selected = "Main")
      
    }
    
    #
    # Initialiseer de data die de view bepaalt.
    isolate ({
      restartAll(NULL)
      })
    
    # Quit button
    observeEvent(input$quit, {
      js$closeWindow()
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
 #     rv$thecurrentview = input$theAppPage
    })


# About ----------------------------------------------------------

 
    observeEvent(input$showabout, {
      showModal(modalDialog(
        title = "About NetVis",
        tags$p("An experimental browser for network graphs, in this case communication in the care sector in the Netherlands"),
        tags$p("Gert Florijn, 2020")
      ))
    })


    
#     # 
#     # Dit is de event handler als de grafiek getekend is.
#     # Goede plek om the bevriezen? Dat kan via de proxy
#     # 
#     observeEvent(input$graph_panel_initialized, {
#         p = visNetworkProxy(input$graph_panel)
# #        cat('Initialized proxy \n')
#         rv$thegraphproxy = p
#     })
# 
   
   
     # ==========
    # Selectie van een node - twee methodes. dubbelop?
    
    observeEvent(input$current_node_id,  {
      #cat("input_current_node ", input$current_node_id, "\n")
    })

    observeEvent(input$graph_panel_selected, {
      #cat("Node selected ", input$graph_panel_selected, "\n")
      rv$thenodeselected = input$graph_panel_selected
 #     browser()
      updateTextInput(session, "nodefield", value = rv$thenodeselected)
    })
    
    
    # Selectie van een groep, (nog) niet gebruikt
    #  observeEvent(input$graph_panel_selectedBy, {
    #    cat("Observe-graph_panel_selectedBy ", input$graph_panel_selectedBy, "\n")
    #  })
    
    # # Rechter muis knop. Nu nog geen actie aan verbonden
    # observeEvent(input$oncontext, {
    # }) 
    
    # Double click ==> focus op de betreffende node
    observeEvent(input$doubleClick, {
      #browser()
      rv$thenodeselected = input$doubleClick$nodes[[1]]
      #cat('switch focus', rv$thenodeselected, '\n')
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    }) 
    
    # ===
    #
    
    # == UI for node selectie afhandeling
    observeEvent(rv$thenodeselected, {
      #cat("Selected ", rv$thenodeselected, "\n")
      rv$themessage = " "
      # toggleState("hidefromview", rv$thenodeselected != "")
      # toggleState("switchfocus", rv$thenodeselected != "")
      rv$theurl = ""
      haveurl = rv$thenodeselected != "" && V(rv$theigraph)[rv$thenodeselected]$url != ""
      
      if (haveurl) {
        rv$theurl = V(rv$theigraph)[rv$thenodeselected]$url
        rv$themessage = paste0("Zie voor meer informatie ", rv$theurl)
      }
      toggleState("launchbrowser", haveurl) #does not work on deployed apps
    })
    
    #click on linkmenu for selected node
    observeEvent(input$nodemenuclick, {
      if (rv$thenodeselected != "")
        growViewByLinks(c(rv$thenodeselected), input$nodemenuclick)
    } )
    
    #click on linkmenu for all nodes in view
    observeEvent(input$viewmenuclick, {
        growViewByLinks(znops.nodesInView(rv$theigraph, rv$thecurrentview), input$viewmenuclick)
    } )
    
    
    growViewByLinks <- function(nodes, l) {
#browser()
      linktypes = c(l)
      if (l == "all")
        linktypes = rv$thenetworkinfo$linktypes
      for (n in nodes) {
        rv$theigraph = znops.voegVriendenToeAanView(rv$theigraph, rv$thecurrentview, n, 
                                                    linktypes)
      }
      rv$forcerepaint = TRUE
    }
   
    # Creeer en teken de graaf opnieuw
    #
    observeEvent(input$showgraph, {
      rv$forcerepaint = TRUE
    })
    
    
 

    # Verstop node
    observeEvent(input$hidefromview, {
      rv$theigraph = znops.verwijderNodesUitView(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    })
  
    # Focus de view op een node
    observeEvent(input$switchfocus, {
      rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, rv$thenodeselected)
    }) 
    
 
     # Show the whole underlying network
    observeEvent(input$showall, {
      rv$theigraph = znops.toonAllesInView(rv$theigraph, rv$thecurrentview)
    }) 
    
    observeEvent(input$launchbrowser, {
      rv$themessage("not available yet")
      #browseURL(rv$theurl)
    })
    
 

# Data view output --------------------------------------------------------

    output$dataviewnodes <- renderTable({
      rv$thenetworkinfo$rawnodes
    })    
    
    output$dataviewlinks <- renderTable({
#      browser()
      rv$thenetworkinfo$rawlinks
    })    
    
    
    
# Handle uploaded data ----------------------------------------------------

    prepareUploadSettings <- function() {
      toggleState("checkuploadeddata", FALSE)
      toggleState("adduploadeddata", FALSE)
    }
    
    observeEvent(input$prepareupload, {
      #get the data from the module
      rv$thedatauploader = callModule(uploadData, "upload", "upload")
      toggleState("checkuploadeddata", TRUE)
      toggleState("adduploadeddata", FALSE)
    })
    
    checkloadresult <- reactiveVal()
    
    # Try to add the loaded nodes and links to the network and restart all
    #
    observeEvent(input$checkuploadeddata, {
      #get the data from the module
      td = rv$thedatauploader() 
      rv$theuploadeddata = td
      
      nfl = unique(c(td$links[["from"]], td$links[["to"]]))
      nodenames = c(td$nodes[["id"]], rv$thenetworkinfo$nodes$naam)
      missing = !(nfl %in% nodenames)
      result = nfl[missing]
      #    browser()
      toggleState("adduploadeddata", length(result) == 0)
      if (length(result) == 0) {
        checkloadresult("No issues found")
      }
      else {
        checkloadresult(paste0("Unknown nodes: ", result))
      }
    })
    
    output$uploadcheckmessage <- renderText({
      checkloadresult()
    })
    
    # Try to add the loaded nodes and links to the network and restart all
    #
    observeEvent(input$adduploadeddata, {
      #get the data from the module
      thedata = rv$theuploadeddata 
      rv$thedatauploader = NULL 
      toggleState("checkuploadeddata", FALSE)
      toggleState("adduploadeddata", FALSE)
      
      restartAll(thedata)
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
      
      rv$theigraph = znops.startViewOpGraaf(rv$theigraph, viewid)
      rv$theigraph = znops.copyViewInfo(rv$theigraph, rv$thecurrentview, viewid)
      
      gr <- callModule(frozenView, viewid, viewid, v$thevisgraph)

      # Voeg de tab toeg
      appendTab("theAppPage", tabp, select=TRUE)  
      
    })
    


# Export the graph --------------------------------------------------------

        
  #
  # Export the graph
  #
  output$export <- downloadHandler(
    filename = function() {
      f = paste('network-', Sys.Date(), '.html', sep='')
      rv$themessage = paste0("Export to ", f)
      f
    },
    content = function(con) {
      visSave(rv$thevisgraph, file=con)      
    }
  )
  
  
  output$generalmessage <- renderText({
    rv$themessage
  })

  #Starting point menu for showing nodes from different domains.
  output$startpointsmenu <- renderUI({
    tagList(
      selectInput("startingpoint", label=NULL,
                   choices=rv$thenetworkinfo$domains)
      
    )
  })
  
  # Focus de graaf op een set van nodes uit een domein
  observeEvent(input$startingpoint, {
    #browser()
    nodes = V(rv$theigraph)[V(rv$theigraph)$domein == input$startingpoint]$name
    rv$theigraph = znops.herstartViewOpNodes(rv$theigraph, rv$thecurrentview, nodes)
  })
  
  
  observeEvent(input$addnodefromtext, {
#    browser()
    node = V(rv$thenetworkinfo$network)[input$nodefield]
    if (!is.null(node)) {
      rv$theigraph = znops.voegNodesToeAanView(rv$theigraph, rv$thecurrentview, node)
     rv$forcerepaint = TRUE 
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
    res =  c(getNodeMenuEntryScriptFor("actor", "a"), 
             getNodeMenuEntryScriptFor("system", "s"),
             getNodeMenuEntryScriptFor("use", "u"), 
             getNodeMenuEntryScriptFor("object", "o"),
             getNodeMenuEntryScriptFor("part", "p"), 
             getNodeMenuEntryScriptFor("refer", "r"),
             getMenuEntryScriptForColor("all", "*", "black", "nodemenuclick"),
             getMenuEntryScriptForColor("all", "H", "grey", "hidefromview"),
             getMenuEntryScriptForColor("all", "F", "grey", "switchfocus")
    )
  }
  
  viewNodesSelectMenu <- function() {
    res =  c(getViewMenuEntryScriptFor("actor", "a"), 
             getViewMenuEntryScriptFor("system", "s"),
             getViewMenuEntryScriptFor("use", "u"), 
             getViewMenuEntryScriptFor("object", "o"),
             getViewMenuEntryScriptFor("part", "p"), 
             getViewMenuEntryScriptFor("refer", "r"),
             getMenuEntryScriptForColor("all", "*", "black", "viewmenuclick")
    )
  }
  
  nodefieldAddMenu  <- function() {
      res = c(
        getMenuEntryScriptForColor("all", "Add", "grey", "addnodefromtext")
      )  
  }
  
  viewSelectMenu  <- function() {
      res = c(
        getMenuEntryScriptForColor("all", "All", "grey", "showall"),
        getMenuEntryScriptForColor("all", ">View", "grey", "switch to view")
      )
  }
  
  output$nodefieldops <- renderUI ({
    tagList(
      HTML(
        nodefieldAddMenu()
      ),
      tags$br(),
      tags$small("Nodename")
    )    
  })
  
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
  
  output$viewselectmenu <- renderUI({
    tagList(
      HTML(
        viewSelectMenu()
      ),
      tags$small(" "),
      downloadLink("export", "Export"),
      tags$br(),
      tags$small(" "),
    )
  })

    # Dit ook nog eens uitproberen. Graaf niet opnieuw tekenen...
  # visNetwork(nodes, edges) %>%
  #   visEvents(type = "once", startStabilizing = "function() {
  #           this.moveTo({scale:0.1})}") %>%
  #   visPhysics(stabilization = FALSE)
  # 
  
  
  output$graph_panel <- renderVisNetwork({
       if (rv$forcerepaint)
         rv$forcerepaint = FALSE
     
      visual3 = rv$theigraph

      #Focus op de elementen in de view
      visual3 = visual3 - V(visual3)[!vertex_attr(visual3, rv$thecurrentview, V(visual3))]
      visual3 = visual3 - E(visual3)[!edge_attr(visual3, rv$thecurrentview, E(visual3))]
 
      if (input$ongericht) 
        visual3 = as.undirected(visual3, mode="each")

      visual3 = znvis.visNetworkVisualisatieSettings(visual3, rv$thenetworkinfo, input$images, 
                                                     input$showlinks, input$linklabels)

      # see https://www.w3schools.com/howto/howto_css_text_buttons.asp
      
      #cat(znapp.singleNodeSelectMenu())
      
      V(visual3)$title = HTML( 
        singleNodeSelectMenu()
      )
      # V(visual3)$title = HTML(
      #   "<button id='l1' style='border: none;display: inline-block;' type='button'
      #       onclick ='Shiny.setInputValue(\"nodemenuclick\",\"actor\",
      #       Math.random());'>A</button>
      #    <button id='l2' style='border: none;display: inline-block;' type='button'
      #       onclick ='Shiny.setInputValue(\"nodemenuclick\",\"use\",
      #       Math.random());'>U</button>")
      data3 <- toVisNetworkData(visual3)

      vnt = visNetwork(nodes=data3$nodes, edges=data3$edges)
      vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=FALSE)

      if (input$navigatie)
        vnt = visInteraction(vnt, navigationButtons = TRUE)
      if (!input$ongericht)
        vnt = visEdges(vnt, arrows="to")
      
      vnt = visEvents(vnt, 
          doubleClick="function (event) {  Shiny.setInputValue(\"doubleClick\", event); }",
          oncontext="function (event) {  Shiny.setInputValue(\"oncontext\", event); }"              
      )
      
      #visPhysics(vnt, stabilization = FALSE)
      rv$thevisgraph = vnt

      #
      # Gebruik van Igraph voor layout, 
      # vnt = visIgraph(visual3, layout=input$layout, smooth=input$smooth) 
      # 
      # vnt = visOptions(vnt, nodesIdSelection = TRUE, collapse=TRUE)
      # if (input$navigatie)
      #   vnt = visInteraction(vnt, navigationButtons = TRUE)
      # rv$thevisgraph = vnt
      
      vnt
      })
}

shinyApp(ui = ui, server = server)
