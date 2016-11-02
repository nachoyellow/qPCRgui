# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
comparisonTwoSamplesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("scatter.sample.num.1")),
        uiOutput(outputId = ns("scatter.sample.num.2")),
        checkboxInput(inputId = ns("scatter.diagonal.line"), 
                      label = "Y=X diagonal line?", 
                      value = FALSE),
        checkboxInput(inputId = ns("scatter.cor.info"), 
                      label = "Correlation info within plot?", 
                      value = FALSE),
        numericInput(inputId = ns("scatter.ct.max"),
                     label = "Maximum Ct for correlation computation:",
                     value = 35,
                     min = 0,
                     max = 40,
                     step = 1)
      ),
      mainPanel(
        textInput(inputId = ns("scatter.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("scatter.plot")),
        download_plot_ui(ns("scatter.plot.format"), ns("scatter.plot.width"),
                         ns("scatter.plot.height"), ns("scatter.plot.resolution"),
                         ns("scatter.plot.save"))
      )
    )
  )
}

comparisonTwoSamples <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$scatter.sample.num.1 <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("scatter.sample.num.1"), 
                  label = "Sample number 1:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sample(sampleNames(rvalues$processed.data),1))
    }
  })
  
  output$scatter.sample.num.2 <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("scatter.sample.num.2"), 
                  label = "Sample number 2:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sample(sampleNames(rvalues$processed.data),1))
    }
  })
  
  output$scatter.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$scatter.sample.num.1) 
        && !is.null(input$scatter.sample.num.2) && !is.null(input$scatter.diagonal.line)
        && !is.null(input$scatter.cor.info) && !is.null(input$scatter.ct.max)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- c(pData(rvalues$processed.data)[input$scatter.sample.num.1,"sample"], 
                      pData(rvalues$processed.data)[input$scatter.sample.num.2,"sample"])
      args$col <- "type"
      args$diag <- input$scatter.diagonal.line
      args$cor <- input$scatter.cor.info
      args$Ct.max <- input$scatter.ct.max
      if (input$scatter.title != "") args$main <- input$scatter.title
      lrv$args <- args
      do.call(plotCtScatter, args)
    }
  })
  
  observeEvent({c(lrv$args, input$scatter.plot.format, 
                  input$scatter.plot.height,
                  input$scatter.plot.width, 
                  input$scatter.plot.resolution)}, {
                    output$scatter.plot.save <- download_plot_server(plotCtScatter, lrv$args, 
                                                                    input$scatter.plot.format, 
                                                                    input$scatter.plot.height,
                                                                    input$scatter.plot.width, 
                                                                    input$scatter.plot.resolution)
                  })
}