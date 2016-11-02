duplicatedFeaturesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("dupf.sample.num")),
        numericInput(inputId = ns("dupf.percent"),
                     label = "Percent differing more than:",
                     min = 0,
                     max = 100,
                     value = 20,
                     step = 1)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot",
                   textInput(inputId = ns("dupf.title"), 
                             label = "Title:", 
                             placeholder = "Title of the plot."),
                   plotOutput(outputId = ns("dupf.plot")),
                   download_plot_ui(ns("dupf.plot.format"), ns("dupf.plot.width"),
                                    ns("dupf.plot.height"), ns("dupf.plot.resolution"),
                                    ns("dupf.plot.save"))
          ),
          tabPanel("Details",
                   DT::dataTableOutput(outputId = ns("dupf.details"))
          )
        )
      )
    )
  )
}

duplicatedFeatures <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$dupf.sample.num <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("dupf.sample.num"), 
                  label = "Choose sample:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sample(sampleNames(rvalues$processed.data),1))
    }
  })
  
  output$dupf.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$dupf.sample.num) && !is.null(input$dupf.percent)) {
      if (!any(table(featureNames(rvalues$processed.data)) %% 2 != 0)) {
        args <- list()
        args$q <- rvalues$processed.data
        args$card <- pData(rvalues$processed.data)[input$dupf.sample.num,"sample"]
        args$percent <- as.numeric(input$dupf.percent)
        args$verbose <- FALSE
        if (input$dupf.title != "") args$main <- input$dupf.title
        lrv$args <- args
        reps <- do.call(plotCtReps, args)
        output$dupf.details <- DT::renderDataTable({
          data.frame(row.name=rownames(reps), data.frame(reps, row.names=NULL))
        }, selection="none", colnames= c("ID" = 1))
      }
      else {
        lrv$args <- NULL
      }
    }
  })
  
  observeEvent({c(lrv$args, input$dupf.plot.format, 
                  input$dupf.plot.height,
                  input$dupf.plot.width, 
                  input$dupf.plot.resolution)}, {
                    output$dupf.plot.save <- download_plot_server(plotCtReps, lrv$args, 
                                                                 input$dupf.plot.format, 
                                                                 input$dupf.plot.height,
                                                                 input$dupf.plot.width, 
                                                                 input$dupf.plot.resolution)
                  })
}