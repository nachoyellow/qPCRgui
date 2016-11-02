# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
coefVariationUI <- function(id) {  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("coef.variations.samples")),
        checkboxInput(inputId = ns("coef.variations.stratify"),
                      label = "Stratify by type?",
                      value = TRUE)
        
      ),
      mainPanel(
        textInput(inputId = ns("coef.variations.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("coef.variations.plot")),
        download_plot_ui(ns("coef.variations.plot.format"), ns("coef.variations.plot.width"),
                         ns("coef.variations.plot.height"), ns("coef.variations.plot.resolution"),
                         ns("coef.variations.plot.save"))
      )
    )
  )
}

coefVariation <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$coef.variations.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("coef.variations.samples"), 
                  label = "Choose samples:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  output$coef.variations.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$coef.variations.samples)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$coef.variations.samples,"sample"]
      if (as.logical(input$coef.variations.stratify)) args$stratify <- "type" else args$stratify <- NULL
      if (input$coef.variations.title != "") args$main <- input$coef.variations.title
      lrv$args <- args
      do.call(plotCVBoxes, args)
    }
  })
  
  observeEvent({c(lrv$args, input$coef.variations.plot.format, 
                  input$coef.variations.plot.height,
                  input$coef.variations.plot.width, 
                  input$coef.variations.plot.resolution)}, {
                    output$coef.variations.plot.save <- download_plot_server(plotCVBoxes, lrv$args, 
                                                                    input$coef.variations.plot.format, 
                                                                    input$coef.variations.plot.height,
                                                                    input$coef.variations.plot.width, 
                                                                    input$coef.variations.plot.resolution)
                  })
}

