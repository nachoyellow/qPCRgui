# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
variationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("var.samples")),
        radioButtons(inputId = ns("var.variation"),
                     label = "Using:",
                     choices = c("Variation" = "var", "Standard deviation" = "sd"),
                     selected = "var",
                     inline = TRUE),
        uiOutput(outputId = ns("var.log")),
        radioButtons(inputId = ns("var.type"),
                     label = "Type of plot:",
                     choices = c("Summarised boxplot" = "summary", "Detailed scatter plot" = "detail"),
                     selected = "summary"),
        uiOutput(outputId = ns("var.detailed.featurenames"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot",
                   textInput(inputId = ns("var.title"), 
                             label = "Title:", 
                             placeholder = "Title of the plot."),
                   plotOutput(outputId = ns("var.box.plot")),
                   download_plot_ui(ns("var.plot.format"), ns("var.plot.width"),
                                    ns("var.plot.height"), ns("var.plot.resolution"),
                                    ns("var.plot.save"))
          ),
          tabPanel("Details: Var",
                   DT::dataTableOutput(outputId = ns("var.details.var"))
          ),
          tabPanel("Details: Mean",
                   DT::dataTableOutput(outputId = ns("var.details.mean"))
          )
        )
      )
    )
  )
}

variation <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$var.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("var.samples"), 
                  label = "Choose samples:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  output$var.log <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data) && !is.null(input$var.type)) {
      args <- list()
      args$inputId <- ns("var.log")
      args$label = "Base 10 logarithm"
      if (input$var.type == "detail") args$value <- FALSE else args$value <- TRUE
      do.call(checkboxInput, args)
    }
  })
  
  output$var.detailed.featurenames <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data) && input$var.type == "detail") {
      checkboxInput(inputId = ns("var.detailed.featurenames"),
                   label = "Identify individual outliers?",
                   value = TRUE)
    }
  })
  
  output$var.box.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$var.samples) && !is.null(input$var.variation) &&
        !is.null(input$var.type) && !is.null(input$var.log)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$var.samples,"sample"]
      args$variation <- input$var.variation
      args$type <- input$var.type
      args$log <- as.logical(input$var.log)
      if (input$var.type == "detail" && !is.null(input$var.detailed.featurenames)) args$add.featurenames <- as.logical(input$var.detailed.featurenames)
      if (input$var.type != "detail" && input$var.title != "") args$main <- input$var.title
      lrv$args <- args
      raw.variation <- do.call(plotCtVariation, args)
      output$var.details.var <- DT::renderDataTable({
        raw.variation[["Var"]]
      }, selection="none", colnames= c("ID" = 1))
      output$var.details.mean <- DT::renderDataTable({
        raw.variation[["Mean"]]
      }, selection="none", colnames= c("ID" = 1))
    }
  })
  
  observeEvent({c(lrv$args, input$var.plot.format, 
                  input$var.plot.height,
                  input$var.plot.width, 
                  input$var.plot.resolution)}, {
                    output$var.plot.save <- download_plot_server(plotCtVariation, lrv$args, 
                                                                    input$var.plot.format, 
                                                                    input$var.plot.height,
                                                                    input$var.plot.width, 
                                                                    input$var.plot.resolution)
                  })

}