# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
spatialLayoutUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("sp.lay.sample.num")),
        selectInput(inputId = ns("sp.lay.plot.type"), 
                    label = "Plot type:", 
                    choices = c("Ct" = "Ct", "Feature type" = "type")),
        numericInput(inputId = ns("sp.lay.nrow"), 
                     label = "Number of rows:", 
                     value = 16,
                     step = 1),
        numericInput(inputId = ns("sp.lay.ncol"), 
                     label = "Number of columns:", 
                     value = 24,
                     step = 1),
        uiOutput(outputId = ns("sp.lay.ct.range")),
        numericInput(inputId = ns("sp.lay.well.size"), 
                     label = "Well size:", 
                     value = 3.1,
                     step = 0.1)
      ),
      mainPanel(
        textInput(inputId = ns("sp.lay.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("sp.lay.plot")),
        download_plot_ui(ns("sp.lay.plot.format"), ns("sp.lay.plot.width"),
                         ns("sp.lay.plot.height"), ns("sp.lay.plot.resolution"),
                         ns("sp.lay.plot.save"))
      )
    )
  )
}

spatialLayout <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$sp.lay.sample.num <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("sp.lay.sample.num"), 
                  label = "Choose sample:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sample(sampleNames(rvalues$processed.data),1))
    }
  })
  
  output$sp.lay.ct.range <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      sliderInput(inputId = ns("sp.lay.ct.range"), 
                  label = "Ct range to colour:", 
                  min = 1, 
                  max = as.integer(max(exprs(rvalues$processed.data))), 
                  value = c(10, 35), 
                  step = 1)
    }
  })
  
  output$sp.lay.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$sp.lay.sample.num) && !is.null(input$sp.lay.plot.type) && !is.null(input$sp.lay.title) &&
        !is.null(input$sp.lay.nrow) && !is.null(input$sp.lay.ncol) && !is.null(input$sp.lay.ct.range) &&
        !is.null(input$sp.lay.well.size)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$card <- pData(rvalues$processed.data)[input$sp.lay.sample.num,"sample"]
      args$plot <- input$sp.lay.plot.type
      if (input$sp.lay.title != "") args$main <- input$sp.lay.title
      args$nrow <- as.integer(input$sp.lay.nrow)
      args$ncol <- as.integer(input$sp.lay.ncol)
      args$col.range <- as.integer(input$sp.lay.ct.range)
      args$well.size <- as.numeric(input$sp.lay.well.size)
      lrv$args <- args
      do.call(plotCtCard, args)
    }
  })
  
  observeEvent({c(lrv$args, input$sp.lay.plot.format, 
                  input$sp.lay.plot.height,
                  input$sp.lay.plot.width, 
                  input$sp.lay.plot.resolution)}, {
                    output$sp.lay.plot.save <- download_plot_server(plotCtCard, lrv$args, 
                                                                                input$sp.lay.plot.format, 
                                                                                input$sp.lay.plot.height,
                                                                                input$sp.lay.plot.width, 
                                                                                input$sp.lay.plot.resolution)
                  })
}