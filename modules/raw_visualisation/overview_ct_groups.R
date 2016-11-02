# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.

overviewCtGroupsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(10,
                 uiOutput(outputId = ns("feature.unique.listing"))),
          column(2,
                 checkboxInput(inputId = ns("feature.all"), 
                               label = "ALL", 
                               value = FALSE))
        ),
        uiOutput(outputId = ns("overview.ct.groups.samples")),
        uiOutput(outputId = ns("overview.ct.groups.grouping")),
        uiOutput(outputId = ns("overview.ct.groups.calibrator")),
        checkboxInput(inputId = ns("overview.ct.groups.conf.int"), 
                      label = "Confidence interval?", 
                      value = TRUE),
        checkboxInput(inputId = ns("overview.ct.groups.legend"), 
                      label = "Legend?", 
                      value = TRUE),
        selectInput(inputId = ns("overview.ct.groups.legend.pos"), 
                    label = "Legend:", 
                    choices = c("Top" = "top", "Top-right" = "topright", "Top-left" = "topleft", "Bottom" = "bottom", "Bottom-right" = "bottomright", "Bottom-left" = "bottomleft", "Left" = "left", "Right" = "right", "Center" = "center"), 
                    selected = "topright")
      ),
      mainPanel(
        textInput(inputId = ns("overview.ct.groups.title"),
                  label = "Title:",
                  placeholder = "Title of the plot."),
        plotOutput(outputId = ns("overview.ct.groups.plot")),
        download_plot_ui(ns("overview.ct.groups.plot.format"), ns("overview.ct.groups.plot.width"),
                         ns("overview.ct.groups.plot.height"), ns("overview.ct.groups.plot.resolution"),
                         ns("overview.ct.groups.plot.save"))
      )
    )
  )
}

overviewCtGroups <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$args <- NULL

  # OBSERVERS
  
  observeEvent({c(rvalues$files, input$overview.ct.groups.grouping)}, {
    if (!is.null(input$overview.ct.groups.grouping) && !is.null(rvalues$files)) {
      lrv$grouping <- rvalues$files[[input$overview.ct.groups.grouping]]
      #eval(parse(text = paste("rvalues$files", input$overview.ct.groups.grouping, sep = "$")))
    }
  })
  
  # OUTPUTS
  
  output$overview.ct.groups.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("overview.ct.groups.samples"), 
                  label = "Choose samples:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  output$feature.unique.listing <- renderUI({
    ns <- session$ns

    if (!is.null(rvalues$processed.data)) {
      tmp <- gsub("feature_unique_listing", paste0("\"", ns("feature.unique.listing"), "\""), 
           paste(readLines("./www/js/feature_selectize.js"), collapse="\n"))
      tmp <- gsub("feature_all", paste0("\"", ns("feature.all"), "\""), 
                  tmp)
      tagList(
        selectizeInput(inputId = ns("feature.unique.listing"), 
                       label = "Choose features:",
                       choices = unique(featureNames(rvalues$processed.data)),
                       selected = sample(featureNames(rvalues$processed.data),10),
                       multiple = TRUE
        ),
        
        tags$script(HTML(tmp))
      )
    }
  })
  
  output$overview.ct.groups.grouping <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$files)) {
      selectInput(inputId = ns("overview.ct.groups.grouping"), 
                  label = "Grouping by:", 
                  choices = names(rvalues$files)[-1])
    }
  })
  
  output$overview.ct.groups.calibrator <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping)) {
      selectInput(inputId = ns("overview.ct.groups.calibrator"), 
                  label = "Calibrator:", 
                  choices = c(" ", as.character(lrv$grouping)))
    }
  })
  
  output$overview.ct.groups.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$overview.ct.groups.samples) && !is.null(input$feature.all) &&
        !is.null(input$feature.unique.listing) && !is.null(lrv$grouping) && 
        !is.null(input$overview.ct.groups.calibrator) && !is.null(input$overview.ct.groups.legend) &&
        !is.null(input$overview.ct.groups.legend.pos) && !is.null(input$overview.ct.groups.conf.int) &&
        !is.null(input$overview.ct.groups.title)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$cards <- pData(rvalues$processed.data)[input$overview.ct.groups.samples,"sample"]
      if (as.logical(input$feature.all)) { 
        args$genes <- featureNames(rvalues$processed.data)
      }
      else {
        args$genes <- input$feature.unique.listing
      }
      args$groups <- as.character(lrv$grouping)[pData(rvalues$processed.data)[input$overview.ct.groups.samples,"sample"]]
      if (input$overview.ct.groups.calibrator != " ") args$calibrator <- input$overview.ct.groups.calibrator
      if (input$overview.ct.groups.title != "") args$main <- input$overview.ct.groups.title
      args$legend <- as.logical(input$overview.ct.groups.legend)
      args$args.legend <- c(x = input$overview.ct.groups.legend.pos)
      args$conf.int <- as.logical(input$overview.ct.groups.conf.int)
      lrv$args <- args
      do.call(plotCtOverview, args)
    }
  })
  
  observeEvent({c(lrv$args,input$overview.ct.groups.plot.format, 
    input$overview.ct.groups.plot.height,
    input$overview.ct.groups.plot.width, 
    input$overview.ct.groups.plot.resolution)}, {
      output$overview.ct.groups.plot.save <- download_plot_server(plotCtOverview, lrv$args, 
                                                                  input$overview.ct.groups.plot.format, 
                                                                  input$overview.ct.groups.plot.height,
                                                                  input$overview.ct.groups.plot.width, 
                                                                  input$overview.ct.groups.plot.resolution)
    })
}