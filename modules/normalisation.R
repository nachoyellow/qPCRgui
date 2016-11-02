# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
normalisationUI <- function(id) {
  ns <- NS(id)
  
  # NORMALISATION MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Normalisation'", "&&", "output['", ns("file.uploaded"),"']"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(10,
                   uiOutput(outputId = ns("norm.deltaCt.feature.unique.listing"))),
            column(2,
                   checkboxInput(inputId = ns("feature.all"), 
                                 label = "ALL", 
                                 value = FALSE))
          ),
          uiOutput(outputId = ns("norm.method")),
          actionButton(inputId = ns("norm.set.action"),
                       label = "Normalise")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(title = "Plots",
                     plotOutput(ns("norm.plot"))),
            tabPanel(title = "Details",
                     uiOutput(ns("norm.details")))
          )
        )
      )
    )
  )
}

normalisation <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  lrv <- reactiveValues()
  lrv$args <- NULL
  lrv$results <- NULL
  
  observeEvent(input$norm.deltaCt.feature.unique.listing, {
    print(input$norm.deltaCt.feature.unique.listing)
  })
  
  observeEvent(input$norm.set.action, {
    showModal(modalDialog(title = "Normalisation confirmation", footer = modalButton("Dismiss"),
                size = "m", easyClose = TRUE))
    rvalues$processed.data <- normalizeCtData(rvalues$processed.data, norm = input$norm.method)
    rvalues$normalised <- TRUE
  })
  
  output$norm.deltaCt.feature.unique.listing <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      tmp <- gsub("feature_unique_listing", paste0("\"", ns("norm.deltaCt.feature.unique.listing"), "\""), 
                  paste(readLines("./www/js/feature_selectize.js"), collapse="\n"))
      tmp <- gsub("feature_all", paste0("\"", ns("feature.all"), "\""), 
                  tmp)
      tagList(
        selectizeInput(inputId = ns("norm.deltaCt.feature.unique.listing"), 
                       label = "Choose features for deltaCt normalisation:",
                       choices = unique(featureNames(rvalues$processed.data)),
                       selected = featureNames(rvalues$processed.data)[1],
                       multiple = TRUE
        ),
        
        tags$script(HTML(tmp))
      )
    }
  })
  
  output$norm.method <- renderUI({
    ns <- session$ns
    
    selectInput(inputId = ns("norm.method"), 
                label = "Normalisation method:", 
                choices = c("Quantile normalisation" = "quantile", "Rank invariant scaling" = "scale.rank", "Rank invariant normalisation" = "norm.rank",
                            "deltaCt normalisation" = "deltaCt", "Geometric mean normalisation" = "geometric.mean"))
  })
  
  result <- NULL
  output$norm.plot <- renderPlot({
    if (!is.null(rvalues$processed.data) && !is.null(input$norm.deltaCt.feature.unique.listing) && is.null(rvalues$normalised)) {
      res.q.norm <- capture.output(split = TRUE,
        q.norm <- normalizeCtData(rvalues$processed.data, norm="quantile") # Using quantile normalization, makes distribution of Ct identical across samples
      )
      res.sr.norm <- capture.output(split = TRUE,
        sr.norm <- normalizeCtData(rvalues$processed.data, norm="scale.rank")
      )
      res.nr.norm <- capture.output(split = TRUE,
        nr.norm <- normalizeCtData(rvalues$processed.data, norm="norm.rank")
      )
      res.d.norm <- capture.output(split = TRUE,
        d.norm <- normalizeCtData(rvalues$processed.data, norm="deltaCt", deltaCt.genes=input$norm.deltaCt.feature.unique.listing) # Substracts the mean of the choses controls from all other values in the feature set-
      )
      res.g.norm <- capture.output(split = TRUE,
        g.norm <- normalizeCtData(rvalues$processed.data, norm="geometric.mean") # Average Ct value for each sample, scales all Ct values according to the ratio of these mean Ct values across samples.
      )
      results <- list(res.sr.norm=res.sr.norm, res.nr.norm=res.nr.norm, res.d.norm=res.d.norm, res.g.norm=res.g.norm)
      lrv$results <- results
      
      output$norm.details <- renderUI({
        ns <- session$ns
        
        if (!is.null(lrv$results)) {
          args <- list()
          for (norm in names(lrv$results)) {
            result <- lrv$results[[norm]]
            if (length(result) > 0) {
              args[[norm]] <- verbatimTextOutput(outputId = ns(paste0("norm.", norm)))
            }
          }
          do.call(tagList, args)
        } 
      })
      
      for (norm in names(lrv$results)) {
        outputName <- paste0("norm.", norm)
        result <- paste0(results[[norm]], collapse = "\n")
        output[[outputName]] <- eval(parse(text=paste0("renderText({\"",
                  result,
        "\"})")))
      }
      # Plotting normalizations
      col <- rep(brewer.pal(6, "Spectral"), each=384)
      col2 <- brewer.pal(5, "Dark2")
      par(mfrow=c(3,2), mar=c(2,2,2,2))
      # All methods individually
      plot(exprs(rvalues$processed.data), exprs(q.norm), pch=20, main="Quantile normalisation", col=col)
      plot(exprs(rvalues$processed.data), exprs(sr.norm), pch=20, main="Rank invariant scaling", col=col)
      plot(exprs(rvalues$processed.data), exprs(nr.norm), pch=20, main="Rank invariant normalisation", col=col)
      plot(exprs(rvalues$processed.data), exprs(d.norm), pch=20, main="deltaCt normalisation", col=col)
      plot(exprs(rvalues$processed.data), exprs(g.norm), pch=20, main="Geometric mean normalisation", col=col)
      # Just a single sample, across methods
      plot(exprs(rvalues$processed.data)[,3], exprs(q.norm)[,3], pch=20, col=col2[1], main="Comparison of methods for sample 3", ylim=c(-10,40))
      points(exprs(rvalues$processed.data)[,3], exprs(sr.norm)[,3], pch=20, col=col2[2])
      points(exprs(rvalues$processed.data)[,3], exprs(nr.norm)[,3], pch=20, col=col2[3])
      points(exprs(rvalues$processed.data)[,3], exprs(d.norm)[,3], pch=20, col=col2[4])
      points(exprs(rvalues$processed.data)[,3], exprs(g.norm)[,3], pch=20, col=col2[5])
      legend(8, 40, legend=c("Quantile", "Rank.invariant scaling", "Rank.invariant normalization", "deltaCt", "Geometric.mean"), col=col2, lwd=2, bty="n")
    }
  })
  # details <- reactive({
  #   if (!is.null(lrv$results)) {
  #     browser()
  # 
  #     for (norm in names(lrv$results)) {
  #       result <- lrv$results[[norm]]
  #       output$paste0("norm.", norm) <- renderText({
  #         paste0(result, collapse="\n")
  #       })
  #     } 
  #   }
  # })
  
}