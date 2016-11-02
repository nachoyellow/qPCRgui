# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
scatterAcrossSamplesUI <- function(id) {  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("scatter.all.samples")),
        checkboxInput(inputId = ns("scatter.all.diagonal.line"), 
                      label = "Y=X diagonal line?", 
                      value = TRUE),
        numericInput(inputId = ns("scatter.all.ct.max"),
                     label = "Maximum Ct for correlation computation:",
                     value = 35,
                     min = 0,
                     max = 40,
                     step = 1)
      ),
      mainPanel(
        textInput(inputId = ns("scatter.all.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("scatter.all.plot")),
        download_plot_ui(ns("scatter.all.plot.format"), ns("scatter.all.plot.width"),
                         ns("scatter.all.plot.height"), ns("scatter.all.plot.resolution"),
                         ns("scatter.all.plot.save"))
      )
    )
  )
}

scatterAcrossSamples <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$scatter.all.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("scatter.all.samples"), 
                  label = "Choose samples:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  
  output$scatter.all.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$scatter.all.samples)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$scatter.all.samples,"sample"]
      args$Ct.max <- as.numeric(input$scatter.all.ct.max)
      args$diag <- as.logical(input$scatter.all.diagonal.line)
      if (input$scatter.all.title != "") args$main <- input$scatter.all.title
      lrv$args <- args
      do.call(plotCtPairs, args)
    }
  })
  
  
  observeEvent({c(lrv$args, input$scatter.all.plot.format, 
                  input$scatter.all.plot.height,
                  input$scatter.all.plot.width, 
                  input$scatter.all.plot.resolution)}, {
                    output$scatter.all.plot.save <- download_plot_server(plotCtPairs, lrv$args, 
                                                                    input$scatter.all.plot.format, 
                                                                    input$scatter.all.plot.height,
                                                                    input$scatter.all.plot.width, 
                                                                    input$scatter.all.plot.resolution)
                  })
}