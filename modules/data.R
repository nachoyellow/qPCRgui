# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
dataUI <- function(id) {
  ns <- NS(id)
  
  # DATA
  # DATA MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Data'", "&&", "output['", ns("file.uploaded"),"']"),
      actionButton(inputId = ns("reset.data.button"), label="Reset to raw data"),
      navbarPage("",
                 tabPanel("File data"),
                 tabPanel("Phenotype data"),
                 tabPanel("Feature data"),
                 tabPanel("Expression data"),
                 id = ns("nav.bar.data.current"),
                 collapsible = TRUE
      ),
      # DATA SUB-MENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.data.current"),"'] == 'File data'"),
        DT::dataTableOutput(ns("fileData.table"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.data.current"),"'] == 'Phenotype data'"),
        DT::dataTableOutput(ns("pData.table")),
        textOutput(ns("rows.out.samples")),
        actionButton(inputId = ns("delete.samples.button"), label="Delete selected samples")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.data.current"),"'] == 'Feature data'"),
        DT::dataTableOutput(ns("fData.table")),
        textOutput(ns("rows.out.features")),
        actionButton(inputId = ns("delete.features.button"), 
                     label="Delete selected features")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.data.current"),"'] == 'Expression data'"),
        DT::dataTableOutput(ns("exprs.table"))
      )
    )  
  )
}

data <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  
  # DATA TABLES RELATED OUTPUTS
  output$fileData.table <- DT::renderDataTable({
    if (!is.null(rvalues$files)) {
      files <- rvalues$files
      #data.frame(row.name=rownames(files), data.frame(files, row.names=NULL))
    }
  }, options= list(pageLength = 10), selection = "none", colnames= c("ID" = 1))
  
  # ID needs a column name. 10 rows per page.
  output$pData.table <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data)) {
      pData <- pData(rvalues$processed.data)
      data.frame(row.name=rownames(pData), data.frame(pData, row.names=NULL))
      # Sample names are needed to add explicitly.
    }
  }, options= list(pageLength = 10), colnames= c("ID" = 1))
  
  output$rows.out.samples <- renderText({
    paste(c("You selected these rows on the page:", input$pData.table_rows_selected), colapse = " ")
  })
  
  output$fData.table <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data)) fData(rvalues$processed.data)
  }, 
  options = list(pageLength = 10), colnames= c("ID" = 1))
  
  output$rows.out.features <- renderText({
    paste(c("You selected these rows on the page:", input$fData.table_rows_selected))
  })
  
  output$exprs.table <- DT::renderDataTable({
    if (!is.null(rvalues$processed.data)) {
      exprs <- exprs(rvalues$processed.data)
      data.frame(row.name= rownames(exprs), data.frame(exprs, row.names=NULL))
      # Gene names are needed to add explicitly.
    } 
  }, 
  options = list(pageLength = 10), server=FALSE, selection="none", colnames= c("ID" = 1))
  # Expression table rows are non-selectable.
  
  # OBSERVERS
  observeEvent(input$reset.data.button, {
    rvalues$processed.data <- rvalues$raw.data
    rvalues$files <- rvalues$raw.files
    rvalues$normalised <- NULL
  })
  
  observeEvent(input$delete.samples.button, {
    rvalues$processed.data <- rvalues$processed.data[,-input$pData.table_rows_selected]
    rvalues$files <- rvalues$files[-input$pData.table_rows_selected,]
  })
  
  observeEvent(input$delete.features.button, {
    rvalues$processed.data <- rvalues$processed.data[-input$fData.table_rows_selected,]
  })
}