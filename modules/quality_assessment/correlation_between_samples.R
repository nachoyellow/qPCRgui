# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
correlationBetweenSamplesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = ns("cor.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot.")
      ),
      mainPanel(
        plotOutput(outputId = ns("cor.plot")),
        download_plot_ui(ns("cor.plot.format"), ns("cor.plot.width"),
                         ns("cor.plot.height"), ns("cor.plot.resolution"),
                         ns("cor.plot.save"))
      )
    )
  )
}

correlationBetweenSamples <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$cor.plot <- renderPlot({
    if (!is.null(rvalues$processed.data)) {
      args <- list()
      args$q <- rvalues$processed.data
      if (input$cor.title != "") {
        args$main <- input$cor.title
        plotCtCor(q = rvalues$processed.data,
                  main = input$cor.title)
      }
      else {
        plotCtCor(q = rvalues$processed.data)
      }
      lrv$args <- args
    }
  })
  
  observeEvent({c(lrv$args, input$cor.plot.format, 
                  input$cor.plot.height,
                  input$cor.plot.width, 
                  input$cor.plot.resolution)}, {
                    output$cor.plot.save <- download_plot_server(plotCtCor, lrv$args, 
                                                                    input$cor.plot.format, 
                                                                    input$cor.plot.height,
                                                                    input$cor.plot.width, 
                                                                    input$cor.plot.resolution)
                  })
  
}