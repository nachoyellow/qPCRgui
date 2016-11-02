# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
mannWhitneyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("mann.whitney.grouping")),
        uiOutput(outputId = ns("mann.whitney.groups.1")),
        uiOutput(outputId = ns("mann.whitney.groups.2")),
        uiOutput(outputId = ns("mann.whitney.calibrator")),
        selectInput(inputId = ns("mann.whitney.hypothesis"),
                    label = "Choose hypothesis:",
                    choices = c("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less"),
                    selected = "two.sided",
                    multiple = FALSE),
        checkboxInput(inputId = ns("mann.whitney.paired"),
                      label = "Use paired t-test",
                      value = FALSE),
        checkboxInput(inputId = ns("mann.whitney.replicates"),
                      label = "Combine replicated genes",
                      value = TRUE),
        checkboxInput(inputId = ns("mann.whitney.sort"),
                      label = "Sort by p-values?",
                      value = TRUE),
        checkboxInput(inputId = ns("mann.whitney.stringent"),
                      label = "Flag unreliable/undetermined measurements as \"Undetermined\"?",
                      value = TRUE),
        selectInput(inputId = ns("mann.whitney.p.adjust"),
                    label = "p-value adjustment method:",
                    choices = c("Benjamini & Hochberg" = "BH", "Bonferroni correction" = "bonferroni"),
                    selected = "BH",
                    multiple = FALSE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = ns("mann.whitney.results")),
        conditionalPanel(
          condition =  paste0("output['", ns("mann.whitney.results"), "']"),
          actionButton(inputId = ns("mann.whitney.set.action"),
                       label = "Save Mann-Whitney result") 
        )
      )
    )
  )
}

mannWhitney <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$grouping <- NULL
  lrv$result <- NULL
  
  # OBSERVERS
  
  observeEvent({c(rvalues$files, input$mann.whitney.grouping)}, {
    if (!is.null(input$mann.whitney.grouping)) {
      lrv$grouping <- eval(parse(text = paste("rvalues$files", input$mann.whitney.grouping, sep = "$")))
    }
  })
  
  observeEvent(input$mann.whitney.set.action, {
    if (!is.null(lrv$result)) {
      if (!is.null(rvalues$differential.expression)) {
        rvalues$differential.expression[["Mann-Whitney"]] <- lrv$result
      }
      else {
        rvalues$differential.expression <- list("Mann-Whitney" = lrv$result)
      }
    }
  })
  
  # OUTPUTS
  
  output$mann.whitney.grouping <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("mann.whitney.grouping"), 
                  label = "Grouping by:", 
                  choices = names(rvalues$files)[-1])
    }
  })
  
  output$mann.whitney.calibrator <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data) && !is.null(input$mann.whitney.groups.1) && !is.null(input$mann.whitney.groups.2)) {
      selectInput(inputId = ns("mann.whitney.calibrator"), 
                  label = "Calibrator:", 
                  choices = c(input$mann.whitney.groups.1, input$mann.whitney.groups.2),
                  selected = sample(c(input$mann.whitney.groups.1, input$mann.whitney.groups.2), 1))
    }
  })
  
  output$mann.whitney.groups.1 <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping)) {
      selectInput(inputId = ns("mann.whitney.groups.1"), 
                  label = "Compare 1:", 
                  choices = unique(as.character(lrv$grouping)),
                  selected = sample(as.character(lrv$grouping),1))
    }
  })
  
  output$mann.whitney.groups.2 <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping) && !is.null(input$mann.whitney.groups.1)) {
      selectInput(inputId = ns("mann.whitney.groups.2"), 
                  label = "Compare 2:", 
                  choices = unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$mann.whitney.groups.1],
                  selected = sample(unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$mann.whitney.groups.1], 1))
    }
  })
  
  output$mann.whitney.results <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data) && !is.null(lrv$grouping) && !is.null(input$mann.whitney.calibrator)) {
      args <- list()
      args$q <- rvalues$processed.data[,which(lrv$grouping %in% c(input$mann.whitney.groups.1, input$mann.whitney.groups.2))]
      args$groups <- lrv$grouping[which(lrv$grouping %in% c(input$mann.whitney.groups.1, input$mann.whitney.groups.2))]
      args$calibrator <- input$mann.whitney.calibrator
      args$alternative <- input$mann.whitney.hypothesis
      args$paired <- input$mann.whitney.paired
      args$replicates <- input$mann.whitney.replicates
      args$sort <- input$mann.whitney.sort
      args$stringent <- input$mann.whitney.stringent
      args$p.adjust <- input$mann.whitney.p.adjust
      lrv$result <- do.call(mannwhitneyCtData, args)
    }
  }, options= list(pageLength = 10), selection="none", colnames= c("ID" = 1))
}