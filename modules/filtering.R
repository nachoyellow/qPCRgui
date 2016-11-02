# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
filteringUI <- function(id) {
  ns <- NS(id)
  
  # FILTERING MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Filtering'", "&&", "output['", ns("file.uploaded"),"']"),
      fluidPage(
        column(6,
               wellPanel(
                 fluidRow(
                   column(2,
                          numericInput(inputId = ns("category.ct.max"),
                                       label = "Max Ct:",
                                       value = 35,
                                       min = 0,
                                       max = 40,
                                       step = 1)),
                   column(2,
                          numericInput(inputId = ns("category.ct.min"),
                                       label = "Min Ct:",
                                       value = 10,
                                       min = 0,
                                       max = 40,
                                       step = 1)),
                   column(8,
                          numericInput(inputId = ns("category.quantile"),
                                       label = "Accepted quantile interval for standard deviation:",
                                       value = 0.9,
                                       min = 0,
                                       max = 1,
                                       step = 0.1)) 
                 ),
                 fluidRow(
                   column(6,
                          uiOutput(outputId = ns("category.grouping"))),
                   column(6,
                          textInput(inputId = ns("category.flag.out.text"),
                                    label = "Flag to set unreliable:",
                                    value = "Failed")) 
                 ),
                 checkboxInput(inputId = ns("category.replicates"), 
                               label = "Collapse Ct from replicated genes w/n samples for standard deviation?", 
                               value = TRUE),
                 checkboxInput(inputId = ns("category.flags"),
                               label = "Consider flags?",
                               value = TRUE)
               )
        ),
        column(6,
               wellPanel(
                 uiOutput(outputId = ns("category.samples")),
                 checkboxGroupInput(inputId = ns("category.stratify"),
                                    label = "Stratify by feature type?",
                                    choices = c("Yes" = "type"),
                                    selected = NULL,
                                    inline = TRUE)
               )
        )
      ),
      fluidPage(
        fluidRow(
          wellPanel(align = "center",
                    actionButton(inputId = ns("category.set.action"),
                                 label = "Filter")
          )
        ),
        fluidRow(
          tabsetPanel(
            tabPanel(title = "Plots",
                     column(6,
                            conditionalPanel(
                              condition = paste0("output['", ns("category.plot"),"']"),
                              textInput(inputId = ns("category.plot.title"), 
                                        label = "Title:", 
                                        placeholder = "Title of the plot.")
                            ),
                            plotOutput(ns("category.plot"), height = "400px"),
                            conditionalPanel(
                              condition = paste0("output['", ns("category.plot"),"']"),
                              download_plot_ui(ns("category.plot.format"), ns("category.plot.width"),
                                               ns("category.plot.height"), ns("category.plot.resolution"),
                                               ns("category.plot.save")))
                     ),
                     column(6,
                            fluidRow(),
                            plotOutput(ns("category.plot.feature"), height = "600px"),
                            conditionalPanel(
                              condition = paste0("output['", ns("category.plot.feature"),"']"),
                              download_plot_ui(ns("category.plot.feature.format"), ns("category.plot.feature.width"),
                                               ns("category.plot.feature.height"), ns("category.plot.feature.resolution"),
                                               ns("category.plot.feature.save")))
                     )
            ),
            tabPanel(title = "Details",
                     verbatimTextOutput(ns("category.results")))
          )
        )
      )
    ) 
  )
}

filtering <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  lrv <- reactiveValues()
  lrv$grouping <- NULL
  lrv$args <- NULL
  lrv$args.feature <- NULL
  
  # OBSERVERS
  
  observeEvent(input$category.grouping, {
    if (!is.null(input$category.grouping)) {
      lrv$grouping <- rvalues$files[[input$category.grouping]]
      #lrv$grouping <- eval(parse(text = paste("rvalues$files", input$category.grouping, sep = "$")))
    }
  })
  
  # OUTPUTS
  
  output$category.samples <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("category.samples"), 
                  label = "Choose samples:", 
                  choices = sampleNames(rvalues$processed.data),
                  selected = sampleNames(rvalues$processed.data),
                  multiple = TRUE)
    }
  })
  
  output$category.grouping <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("category.grouping"), 
                  label = "Grouping by:", 
                  choices = names(rvalues$files)[-1])
    }
  })
  
  set_category <- eventReactive(input$category.set.action,{
    if (!is.null(rvalues$processed.data) && !is.null(lrv$grouping) && !is.null(input$category.ct.max) &&
        !is.null(input$category.ct.min) && !is.null(input$category.replicates) && !is.null(input$category.quantile) &&
        !is.null(input$category.samples)) {
      args <- list()
      args$q <- rvalues$processed.data
      args$groups <- as.character(lrv$grouping)
      args$Ct.max <- as.numeric(input$category.ct.max)
      args$Ct.min <- as.numeric(input$category.ct.min)
      args$replicates <- as.logical(input$category.replicates)
      args$quantile <- as.numeric(input$category.quantile)
      args$verbose <- TRUE
      results <- capture.output(split = TRUE,
        rvalues$processed.data <- do.call(setCategory, args)
      )
      output$category.results <- renderText({paste0(results, collapse="\n")})
      plot.args <- list(q = rvalues$processed.data,
                        cards = pData(rvalues$processed.data)[input$category.samples,"sample"],
                        by.feature = FALSE)
      if (!is.null(input$category.stratify)) plot.args$stratify <- input$category.stratify
      plot.args.feature <- plot.args
      plot.args.feature$by.feature = TRUE
      list(plot.args = plot.args, plot.args.feature = plot.args.feature)
    }
  })
  
  output$category.plot <- renderPlot({
    args <- set_category()$plot.args
    if (input$category.plot.title != "") args$main <- input$category.plot.title
    lrv$args <- args
    do.call(plotCtCategory, args)
  })
  
  output$category.plot.feature <- renderPlot({
      args <- set_category()$plot.args.feature
      args$cexRow = 0.1
      lrv$args.feature <- args
      do.call(plotCtCategory, args)
  })
  
  observeEvent({c(lrv$args,input$category.plot.format, 
                  input$category.plot.height,
                  input$category.plot.width, 
                  input$category.plot.resolution)}, {
                    output$category.plot.save <- download_plot_server(plotCtCategory, lrv$args, 
                                                                                input$category.plot.format, 
                                                                                input$category.plot.height,
                                                                                input$category.plot.width, 
                                                                                input$category.plot.resolution)
                  })
  
  observeEvent({c(lrv$args,input$category.plot.feature.format, 
                  input$category.plot.feature.height,
                  input$category.plot.feature.width, 
                  input$category.plot.feature.resolution)}, {
                    output$category.plot.feature.save <- download_plot_server(plotCtCategory, lrv$args.feature, 
                                                                                input$category.plot.feature.format, 
                                                                                input$category.plot.feature.height,
                                                                                input$category.plot.feature.width, 
                                                                                input$category.plot.feature.resolution)
                  })
}