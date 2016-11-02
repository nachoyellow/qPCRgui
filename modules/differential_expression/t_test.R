# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
tTestUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("t.test.grouping")),
        uiOutput(outputId = ns("t.test.groups.1")),
        uiOutput(outputId = ns("t.test.groups.2")),
        uiOutput(outputId = ns("t.test.calibrator")),
        selectInput(inputId = ns("t.test.hypothesis"),
                    label = "Choose hypothesis:",
                    choices = c("Two-sided" = "two.sided", "Greater" = "greater", "Less" = "less"),
                    selected = "two.sided",
                    multiple = FALSE),
        checkboxInput(inputId = ns("t.test.paired"),
                      label = "Use paired t-test",
                      value = FALSE),
        checkboxInput(inputId = ns("t.test.replicates"),
                      label = "Combine replicated genes",
                      value = TRUE),
        checkboxInput(inputId = ns("t.test.sort"),
                      label = "Sort by p-values?",
                      value = TRUE),
        checkboxInput(inputId = ns("t.test.stringent"),
                      label = "Flag unreliable/undetermined measurements as \"Undetermined\"?",
                      value = TRUE),
        selectInput(inputId = ns("t.test.p.adjust"),
                    label = "p-value adjustment method:",
                    choices = c("Benjamini & Hochberg" = "BH", "Bonferroni correction" = "bonferroni"),
                    selected = "BH",
                    multiple = FALSE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = ns("t.test.results")),
        conditionalPanel(
          condition = paste0("output['", ns("t.test.results"), "']"),
          actionButton(inputId = ns("t.test.set.action"),
                       label = "Save t-test result") 
        )
      )
    )
  )
}

tTest <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$grouping <- NULL
  lrv$result <- NULL
  
  # OBSERVERS
  
  observeEvent({c(rvalues$files, input$t.test.grouping)}, {
    if (!is.null(input$t.test.grouping)) {
      lrv$grouping <- eval(parse(text = paste("rvalues$files", input$t.test.grouping, sep = "$")))
    }
  })
  
  observeEvent(input$t.test.set.action, {
    if (!is.null(lrv$result)) {
      if (!is.null(rvalues$differential.expression)) {
        rvalues$differential.expression[["t-test"]] <- lrv$result
      }
      else {
        rvalues$differential.expression <- list("t-test" = lrv$result)
      }
    }
  })
  
  # OUTPUTS
  
  output$t.test.grouping <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      selectInput(inputId = ns("t.test.grouping"), 
                  label = "Grouping by:", 
                  choices = names(rvalues$files)[-1])
    }
  })
  
  output$t.test.calibrator <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data) && !is.null(input$t.test.groups.1) && !is.null(input$t.test.groups.2)) {
      selectInput(inputId = ns("t.test.calibrator"), 
                  label = "Calibrator:", 
                  choices = c(input$t.test.groups.1, input$t.test.groups.2),
                  selected = sample(c(input$t.test.groups.1, input$t.test.groups.2), 1))
    }
  })
  
  output$t.test.groups.1 <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping)) {
      selectInput(inputId = ns("t.test.groups.1"), 
                  label = "Compare 1:", 
                  choices = unique(as.character(lrv$grouping)),
                  selected = sample(as.character(lrv$grouping),1))
    }
  })
  
  output$t.test.groups.2 <- renderUI({
    ns <- session$ns
    
    if (!is.null(lrv$grouping) && !is.null(input$t.test.groups.1)) {
      selectInput(inputId = ns("t.test.groups.2"), 
                  label = "Compare 2:", 
                  choices = unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$t.test.groups.1],
                  selected = sample(unique(as.character(lrv$grouping))[!unique(as.character(lrv$grouping)) %in% input$t.test.groups.1], 1))
    }
  })
  
  output$t.test.results <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data) && !is.null(lrv$grouping) && !is.null(input$t.test.calibrator) && input$t.test.calibrator != "") {
      args <- list()
      args$q <- rvalues$processed.data[,which(lrv$grouping %in% c(input$t.test.groups.1, input$t.test.groups.2))]
      args$groups <- lrv$grouping[which(lrv$grouping %in% c(input$t.test.groups.1, input$t.test.groups.2))]
      args$calibrator <- input$t.test.calibrator
      args$alternative <- input$t.test.hypothesis
      args$paired <- input$t.test.paired
      args$replicates <- input$t.test.replicates
      args$sort <- input$t.test.sort
      args$stringent <- input$t.test.stringent
      args$p.adjust <- input$t.test.p.adjust
      lrv$result <- do.call(ttestCtData, args)
    }
  }, options= list(pageLength = 10), selection="none", colnames= c("ID" = 1))
}