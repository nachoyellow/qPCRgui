# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
clusteringUI <- function(id) {
  ns <- NS(id)
  
  # CLUSTERING
  # CLUSTERING MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Data clustering'", "&&", "output['", ns("file.uploaded"),"']"),
      navbarPage("",
                 tabPanel("Hierarchical clustering"),
                 tabPanel("Principal components analysis"),
                 id = ns("nav.bar.clustering.current"),
                 collapsible = TRUE),
      # CLUSTERING SUBMENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.clustering.current"),"'] == 'Hierarchical clustering'"),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = ns("hierarchical.clustering.type"),
                        label = "Clustering:",
                        choices = c("Genes" = "genes",
                                    "Samples" = "samples"),
                        selected = "genes"),
            selectInput(inputId = ns("hierarchical.clustering.dist"),
                        label = "Clustering by:",
                        choices = c("Euclidean distance" = "euclidean",
                                    "Pearson correlation" = "pearson"),
                        selected = "pearson"),
            numericInput(inputId = ns("hierarchical.clustering.n.cluster"),
                         label = "Number of clusters:",
                         value = NULL)
          ),
          mainPanel(
            textInput(inputId = ns("hierarchical.clustering.title"), 
                      label = "Title:", 
                      placeholder = "Title of the plot."),
            plotOutput(ns("hierarchical.clustering.plot")),
            download_plot_ui(ns("hierarchical.clustering.plot.format"), ns("hierarchical.clustering.plot.width"),
                             ns("hierarchical.clustering.plot.height"), ns("hierarchical.clustering.plot.resolution"),
                             ns("hierarchical.clustering.plot.save"))
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.clustering.current"),"'] == 'Principal components analysis'"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(inputId = ns("PCA.features"),
                         label = "Should features be plotted?",
                         choices = c("Yes" = TRUE, "No" = FALSE),
                         selected = TRUE,
                         inline = TRUE)
          ),
          mainPanel(
            plotOutput(outputId = ns("PCA.plot")),
            download_plot_ui(ns("PCA.plot.format"), ns("PCA.plot.width"),
                             ns("PCA.plot.height"), ns("PCA.plot.resolution"),
                             ns("PCA.plot.save"))
          )
        )
      )
    )
  )
}

clustering <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  lrv <- reactiveValues()
  lrv$args.hierarchical <- NULL
  lrv$args.PCA <- NULL
  
  output$hierarchical.clustering.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$hierarchical.clustering.type) &&
        !is.null(input$hierarchical.clustering.n.cluster)) {
      args <- list()
      args$q <- rvalues$processed.data
      if (input$hierarchical.clustering.title != "") args$main <- input$hierarchical.clustering.title
      args$type <- input$hierarchical.clustering.type
      args$dist <- input$hierarchical.clustering.dist
      if (!is.na(input$hierarchical.clustering.n.cluster)) args$n.cluster <- input$hierarchical.clustering.n.cluster
      if (input$hierarchical.clustering.title != "") args$main <- input$hierarchical.clustering.title
      lrv$args.hierarchical <- args
      do.call(clusterCt, args)
    }
  })
  
  output$PCA.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$PCA.features)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$features <- input$PCA.features
      lrv$args.PCA <- args
      do.call(plotCtPCA, args)
    }
  })
  
  observeEvent({c(lrv$args.hierarchical, input$hierarchical.clustering.plot.format, 
                  input$hierarchical.clustering.plot.height,
                  input$hierarchical.clustering.plot.width, 
                  input$hierarchical.clustering.plot.resolution)}, {
                    output$hierarchical.clustering.plot.save <- download_plot_server(clusterCt, lrv$args.hierarchical, 
                                                                    input$hierarchical.clustering.plot.format, 
                                                                    input$hierarchical.clustering.plot.height,
                                                                    input$hierarchical.clustering.plot.width, 
                                                                    input$hierarchical.clustering.plot.resolution)
                  })
  
  observeEvent({c(lrv$args.PCA, input$PCA.plot.format, 
                  input$PCA.plot.height,
                  input$PCA.plot.width, 
                  input$PCA.plot.resolution)}, {
                    output$PCA.plot.save <- download_plot_server(plotCtPCA, lrv$args.PCA, 
                                                                    input$PCA.plot.format, 
                                                                    input$PCA.plot.height,
                                                                    input$PCA.plot.width, 
                                                                    input$PCA.plot.resolution)
                  })
}