# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
distributionOfCtValuesUI <- function(id) {  
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("dist.ct.samples")),
        uiOutput(outputId = ns("dist.ct.histogram.sample.num")),
        checkboxGroupInput(inputId = ns("boxplot.stratify"),
                           label = "Stratify boxplot by feature type?",
                           choices = c("Yes" = "type"),
                           selected = "type",
                           inline = TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Density plot",
            textInput(inputId = ns("dist.ct.density.title"), 
                      label = "Title:", 
                      placeholder = "Title of the plot."),
            plotOutput(outputId = ns("dist.ct.density.plot")),
            download_plot_ui(ns("dist.ct.density.plot.format"), ns("dist.ct.density.plot.width"),
                             ns("dist.ct.density.plot.height"), ns("dist.ct.density.plot.resolution"),
                             ns("dist.ct.density.plot.save"))
          ),
          tabPanel("Histogram plot",
            textInput(inputId = ns("dist.ct.histogram.title"), 
                      label = "Title:", 
                      placeholder = "Title of the plot."),
            plotOutput(outputId = ns("dist.ct.histogram.plot")),
            download_plot_ui(ns("dist.ct.histogram.plot.format"), ns("dist.ct.histogram.plot.width"),
                             ns("dist.ct.histogram.plot.height"), ns("dist.ct.histogram.plot.resolution"),
                             ns("dist.ct.histogram.plot.save"))
          ),
          tabPanel("Boxplot",
            textInput(inputId = ns("dist.ct.box.title"), 
                      label = "Title:", 
                      placeholder = "Title of the plot."),
            plotOutput(outputId = ns("dist.ct.box.plot")),
            download_plot_ui(ns("dist.ct.box.plot.format"), ns("dist.ct.box.plot.width"),
                             ns("dist.ct.box.plot.height"), ns("dist.ct.box.plot.resolution"),
                             ns("dist.ct.box.plot.save"))
          ),
          tabPanel("Summary", {
            DT::dataTableOutput(outputId = ns("dist.ct.summary"))
          })
        )
      )
    )
  )
}

distributionOfCtValues <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args.density <- NULL
  lrv$args.hist <- NULL
  lrv$args.box <- NULL
  
  output$dist.ct.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("dist.ct.samples"), 
                  label = "Choose samples for the density/box plot::", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  output$dist.ct.histogram.sample.num <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("dist.ct.histogram.sample.num"), 
                  label = "Choose sample for the histogram plot:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sample(sampleNames(rvalues$processed.data),1))
    }
  })
  
  output$dist.ct.density.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$dist.ct.samples)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$dist.ct.samples,"sample"]
      if (input$dist.ct.density.title != "") args$main <- input$dist.ct.density.title
      lrv$args.density <- args
      do.call(plotCtDensity, args)
    }
  })
  
  output$dist.ct.histogram.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$dist.ct.histogram.sample.num)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$card <- pData(rvalues$processed.data)[input$dist.ct.histogram.sample.num,"sample"]
      if (input$dist.ct.histogram.title != "") args$main <- input$dist.ct.histogram.title
      lrv$args.hist <- args
      do.call(plotCtHistogram, args)
    }
  })
  
  output$dist.ct.box.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$dist.ct.samples)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$dist.ct.samples,"sample"]
      if(!is.null(input$boxplot.stratify)) args$stratify <- input$boxplot.stratify else args["stratify"] <- list(NULL)
      if (input$dist.ct.box.title != "") args$main <- input$dist.ct.box.title
      lrv$args.box <- args
      browser()
      do.call(plotCtBoxes, args)
    }
  })
  
  output$dist.ct.summary <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data)) {
      summary(rvalues$processed.data)
    }
  }, selection="none")
  
  observeEvent({c(lrv$args.density, input$dist.ct.density.plot.format, 
                  input$dist.ct.density.plot.height,
                  input$dist.ct.density.plot.width, 
                  input$dist.ct.density.plot.resolution)}, {
                    output$dist.ct.density.plot.save <- download_plot_server(plotCtDensity, lrv$args.density, 
                                                                    input$dist.ct.density.plot.format, 
                                                                    input$dist.ct.density.plot.height,
                                                                    input$dist.ct.density.plot.width, 
                                                                    input$dist.ct.density.plot.resolution)
                  })
  
  observeEvent({c(lrv$args.hist, input$dist.ct.histogram.plot.format, 
                  input$dist.ct.histogram.plot.height,
                  input$dist.ct.histogram.plot.width, 
                  input$dist.ct.histogram.plot.resolution)}, {
                    output$dist.ct.histogram.plot.save <- download_plot_server(plotCtHistogram, lrv$args.hist, 
                                                                    input$dist.ct.histogram.plot.format, 
                                                                    input$dist.ct.histogram.plot.height,
                                                                    input$dist.ct.histogram.plot.width, 
                                                                    input$dist.ct.histogram.plot.resolution)
                  })
  
  observeEvent({c(lrv$args.box, input$dist.ct.box.plot.format, 
                  input$dist.ct.box.plot.height,
                  input$dist.ct.box.plot.width, 
                  input$dist.ct.box.plot.resolution)}, {
                    output$dist.ct.box.plot.save <- download_plot_server(plotCtBoxes, lrv$args.box, 
                                                                    input$dist.ct.box.plot.format, 
                                                                    input$dist.ct.box.plot.height,
                                                                    input$dist.ct.box.plot.width, 
                                                                    input$dist.ct.box.plot.resolution)
                  })
}