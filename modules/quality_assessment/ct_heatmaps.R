# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
ctHeatmapsUI <- function(id) {  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = ns("ct.heatmap.dist"),
                    label = "Clustering by:",
                    choices = c("Euclidean distance" = "euclidean",
                                "Pearson correlation" = "pearson"),
                    selected = "pearson"),
        radioButtons(inputId = ns("show.gene.names"),
                     label = "Show gene names?",
                     choices = c("Yes" = TRUE, "No" = FALSE),
                     selected = FALSE,
                     inline = TRUE),
        radioButtons(inputId = ns("show.sample.names"),
                     label = "Show sample names?",
                     choices = c("Yes" = TRUE, "No" = FALSE),
                     selected = TRUE,
                     inline = TRUE)
      ),
      mainPanel(
        textInput(inputId = ns("ct.heatmap.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("ct.heatmap")),
        download_plot_ui(ns("ct.heatmap.plot.format"), ns("ct.heatmap.plot.width"),
                         ns("ct.heatmap.plot.height"), ns("ct.heatmap.plot.resolution"),
                         ns("ct.heatmap.plot.save"))
      )
    )
  )
}

ctHeatmaps <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$ct.heatmap <- renderPlot({
    if (!is.null(rvalues$processed.data)) {
      args <- list()
      args$q <- rvalues$processed.data
      if (input$ct.heatmap.title != "") args$main <- input$ct.heatmap.title
      args$dist <- input$ct.heatmap.dist
      if (!as.logical(input$show.gene.names)) args$gene.names <- ""
      if (!as.logical(input$show.sample.names)) args$sample.names <- ""
      lrv$args <- args
      do.call(plotCtHeatmap, args)
    }
  })
  
  
  observeEvent({c(lrv$args, input$ct.heatmap.plot.format, 
                  input$ct.heatmap.plot.height,
                  input$ct.heatmap.plot.width, 
                  input$ct.heatmap.plot.resolution)}, {
                    output$ct.heatmap.plot.save <- download_plot_server(plotCtHeatmap, lrv$args, 
                                                                    input$ct.heatmap.plot.format, 
                                                                    input$ct.heatmap.plot.height,
                                                                    input$ct.heatmap.plot.width, 
                                                                    input$ct.heatmap.plot.resolution)
                  })
}