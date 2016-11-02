# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
relativeQuantificationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("rq.diff.expr")),
        fluidRow(
          column(10,
                 uiOutput(outputId = ns("rq.feature.unique.listing"))),
          column(2,
                 checkboxInput(inputId = ns("feature.all"), 
                               label = "ALL", 
                               value = FALSE))
        ),
        selectInput(inputId = ns("rq.transform"), 
                    label = "Transform:", 
                    choices = c("Base 2 logarithm" = "log2", "Base 10 logarithm" = "log10", "None" = "none"), 
                    selected = "log2"),
        numericInput(inputId = ns("rq.p.val"),
                     label = "p-values' threshold",
                     value = 0.1,
                     min = 0,
                     max = 1,
                     step = 0.05),
        numericInput(inputId = ns("rq.p.sig"),
                     label = "Cut-off for significant p-values marked by *",
                     value = 0.05,
                     min = 0,
                     max = 1,
                     step = 0.05),
        numericInput(inputId = ns("rq.very.sig"),
                     label = "Cut-off for very significant p-values marked by \"",
                     value = 0.01,
                     min = 0,
                     max = 1,
                     step = 0.01),
        checkboxInput(inputId = ns("rq.mark.sig"),
                      label = "Mark significant features",
                      value = TRUE),
        checkboxInput(inputId = ns("rq.mark.un"),
                      label = "Mark data with unreliable target or calibrator samples",
                      value = TRUE),
        checkboxInput(inputId = ns("rq.legend"),
                      label = "Include legend",
                      value = TRUE)
      ),
      mainPanel(
        textInput(inputId = ns("rq.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("rq.plot")),
        download_plot_ui(ns("rq.plot.format"), ns("rq.plot.width"),
                         ns("rq.plot.height"), ns("rq.plot.resolution"),
                         ns("rq.plot.save"))
      )
    )
  )
}

relativeQuantification <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL
  
  output$rq.diff.expr <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$differential.expression)) {
      selectInput(inputId = ns("rq.diff.expr"),
                  label = "Select differential expression result:",
                  choices = names(rvalues$differential.expression))
    }
    else {
      helpText("You need to save a differential expression result to use this functionality.")
    }
  })
  
  output$rq.feature.unique.listing <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      tmp <- gsub("feature_unique_listing", paste0("\"", ns("rq.feature.unique.listing"), "\""), 
                  paste(readLines("./www/js/feature_selectize.js"), collapse="\n"))
      tmp <- gsub("feature_all", paste0("\"", ns("feature.all"), "\""), 
                  tmp)
      tagList(
        selectizeInput(inputId = ns("rq.feature.unique.listing"), 
                       label = "Choose features:",
                       choices = unique(featureNames(rvalues$processed.data)),
                       selected = sample(featureNames(rvalues$processed.data),10),
                       multiple = TRUE
        ),
        
        tags$script(HTML(tmp))
      )
    }
  })
  
  output$rq.plot <- renderPlot({
    if (!is.null(rvalues$differential.expression) && !is.null(input$rq.feature.unique.listing) &&
        !is.null(input$rq.diff.expr)) {
      args <- list()
      args$qDE <- rvalues$differential.expression[[input$rq.diff.expr]]
      args$genes <- input$rq.feature.unique.listing
      args$transform <- input$rq.transform
      args$p.val <- as.numeric(input$rq.p.val)
      args$mark.sig <- input$rq.mark.sig
      args$p.sig <- input$rq.p.sig
      args$p.very.sig <- as.numeric(input$rq.very.sig)
      args$mark.un <- input$rq.mark.un
      args$legend <- input$rq.legend
      if (input$rq.title != "") args$main <- input$rq.title
      lrv$args <- args
      do.call(plotCtRQ, args)
    }
  })
  
  observeEvent({c(lrv$args, input$rq.plot.format, 
                  input$rq.plot.height,
                  input$rq.plot.width, 
                  input$rq.plot.resolution)}, {
                    output$rq.plot.save <- download_plot_server(plotCtRQ, lrv$args, 
                                                                    input$rq.plot.format, 
                                                                    input$rq.plot.height,
                                                                    input$rq.plot.width, 
                                                                    input$rq.plot.resolution)
                  })
}