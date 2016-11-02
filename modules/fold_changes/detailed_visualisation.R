# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
detailedVisualisationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("dv.diff.expr")),
        fluidRow(
          column(10,
                 uiOutput(outputId = ns("dv.feature.unique.listing"))),
          column(2,
                 checkboxInput(inputId = ns("feature.all"), 
                               label = "ALL", 
                               value = FALSE))
        ),
        numericInput(inputId = ns("dv.p.val"),
                     label = "p-values' threshold",
                     value = 0.1,
                     min = 0,
                     max = 1,
                     step = 0.05),
        uiOutput(outputId = ns("dv.grouping")),
        uiOutput(outputId = ns("dv.groups.calibrator")),
        uiOutput(outputId = ns("dv.groups.target")),
        numericInput(inputId = ns("dv.p.sig"),
                     label = "Cut-off for significant p-values marked by *",
                     value = 0.05,
                     min = 0,
                     max = 1,
                     step = 0.05),
        numericInput(inputId = ns("dv.very.sig"),
                     label = "Cut-off for very significant p-values marked by \"",
                     value = 0.01,
                     min = 0,
                     max = 1,
                     step = 0.01),
        checkboxInput(inputId = ns("dv.mark.sig"),
                      label = "Mark significant features",
                      value = TRUE),
        checkboxInput(inputId = ns("dv.legend"),
                      label = "Include legend",
                      value = TRUE),
        numericInput(inputId = ns("dv.jitter"),
                     label = "Jittering factor along the x-axis",
                     value = 0.5,
                     min = 0,
                     max = 1,
                     step = 0.1)
      ),
      mainPanel(
        textInput(inputId = ns("dv.title"), 
                  label = "Title:", 
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("dv.plot")),
        download_plot_ui(ns("dv.plot.format"), ns("dv.plot.width"),
                         ns("dv.plot.height"), ns("dv.plot.resolution"),
                         ns("dv.plot.save"))
      )
    )
  )
}

detailedVisualisation <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$grouping <- NULL
  lrv$args <- NULL
  
  # OBSERVERS
  
  observeEvent({c(rvalues$files, input$dv.grouping)}, {
    if (!is.null(input$dv.grouping)) {
      lrv$grouping <- eval(parse(text = paste("rvalues$files", input$dv.grouping, sep = "$")))
    }
  })
  
  # OUTPUTS
  
  output$dv.grouping <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("dv.grouping"), 
                  label = "Grouping by:", 
                  choices = names(rvalues$files)[-1])
    }
  })
  
  output$dv.groups.calibrator <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping)) {
      selectInput(inputId = ns("dv.groups.calibrator"), 
                  label = "Calibrator:", 
                  choices = unique(as.character(lrv$grouping)),
                  selected = sample(as.character(lrv$grouping),1))
    }
  })
  
  output$dv.groups.target <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping) && !is.null(input$dv.groups.calibrator)) {
      selectInput(inputId = ns("dv.groups.target"), 
                  label = "Target:", 
                  choices = unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$dv.groups.calibrator],
                  selected = sample(unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$dv.groups.calibrator], 1))
    }
  })
    
  output$dv.diff.expr <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$differential.expression)) {
      selectInput(inputId = ns("dv.diff.expr"),
                  label = "Select differential expression result:",
                  choices = names(rvalues$differential.expression))
    }
    else {
      helpText("You need to save a differential expression result to use this functionality.")
    }
  })
  
  output$dv.feature.unique.listing <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      tmp <- gsub("feature_unique_listing", paste0("\"", ns("dv.feature.unique.listing"), "\""), 
                  paste(readLines("./www/js/feature_selectize.js"), collapse="\n"))
      tmp <- gsub("feature_all", paste0("\"", ns("feature.all"), "\""), 
                  tmp)
      tagList(
        selectizeInput(inputId = ns("dv.feature.unique.listing"), 
                       label = "Choose features:",
                       choices = unique(featureNames(rvalues$processed.data)),
                       selected = sample(featureNames(rvalues$processed.data),10),
                       multiple = TRUE
        ),
        
        tags$script(HTML(tmp))
      )
    }
  })
  
  output$dv.plot <- renderPlot({
    if (!is.null(rvalues$differential.expression) && !is.null(input$dv.feature.unique.listing) &&
        !is.null(input$dv.diff.expr) && !is.null(lrv$grouping) && !is.null(input$dv.groups.calibrator) &&
        !is.null(input$dv.groups.target) && !is.null(rvalues$processed.data)) {
      args <- list()
      args$qDE <- rvalues$differential.expression[[input$dv.diff.expr]]
      args$q <- rvalues$processed.data
      args$genes <- input$dv.feature.unique.listing
      args$p.val <- input$dv.p.val
      args$groups <- lrv$grouping
      args$calibrator <- input$dv.groups.calibrator
      args$target <- input$dv.groups.calibrator
      args$p.sig <- input$dv.p.sig
      args$p.very.sig <- input$dv.p.very.sig
      args$mark.sig <- input$dv.mark.sig
      args$legend <- input$dv.legend
      args$jitter <- input$dv.jitter
      if (input$dv.title != "") args$main <- input$dv.title
      lrv$args <- args
      do.call(plotCtSignificance, args)
    }
  })
  
  observeEvent({c(lrv$args, input$dv.plot.format, 
                  input$dv.plot.height,
                  input$dv.plot.width, 
                  input$dv.plot.resolution)}, {
                    output$dv.plot.save <- download_plot_server(plotCtSignificance, lrv$args, 
                                                                    input$dv.plot.format, 
                                                                    input$dv.plot.height,
                                                                    input$dv.plot.width, 
                                                                    input$dv.plot.resolution)
                  })
}