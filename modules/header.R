# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
headerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # HEADER
    br(),
    
    fluidRow(
      column(2,
            imageOutput(ns("ehu.logo"), height = "auto")
      ),
      column(10,
             titlePanel(h1("Quantitative real-time PCR data: cycle threshold analysis", align="center"))
      )
    ),
    
    br(),
    
    # MAIN MENU
    conditionalPanel(
      condition = paste0("output['", ns("file.uploaded"), "']"),
      # Javascript receives just a String, it needs to be produced.
      # Show the panel only if there are uploaded files.
      
      navbarPage("",
                 tabPanel("Data"),
                 tabPanel("Raw visualisation"),
                 tabPanel("Filtering"),
                 tabPanel("Normalisation"),
                 tabPanel("Quality assessment"),
                 tabPanel("Data clustering"),
                 tabPanel("Differential expression"),
                 tabPanel("Fold changes"),
                 id = ns("nav.bar.current"),
                 collapsible = TRUE
      )
    )
  )
}

header <- function(input, output, session, rvalues) {
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
    
  observeEvent(input$nav.bar.current,{
    rvalues$nav.bar.current <- input$nav.bar.current
  })
  # When current nav bar selection changes, change global rvalue.
  
  output$ehu.logo <- renderImage({
    ns <- session$ns
    
    width  <- eval(parse(text=paste0("session$clientData$\"output_", ns("ehu.logo"), "_width\"")))
    height <- eval(parse(text=paste0("session$clientData$\"output_", ns("ehu.logo"), "_heigth\"")))
    filename <- normalizePath(file.path('./images/ehu_logo.jpg'))
    list(src = filename,
         width = width,
         height = height)
  }, deleteFile = FALSE)
}