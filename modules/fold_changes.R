source("./modules/fold_changes/relative_quantification.R")
source("./modules/fold_changes/detailed_visualisation.R")

# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
foldChangesUI <- function(id) {
  ns <- NS(id)
  
  # FOLD CHANGES
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Fold changes'", "&&", "output['", ns("file.uploaded"),"']"),
      uiOutput(ns("nav.bar.fold.changes.current")),
      # FOLD CHANGES MENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.fold.changes.current"),"'] == 'Relative quantification'"),
        relativeQuantificationUI(ns("relative_quantification"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.fold.changes.current"),"'] == 'Detailed visualisation'"),
        detailedVisualisationUI(ns("detailed_visualisation"))
      )
    )
  )
}

foldChanges <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  
  # FOLD CHANGES RELATED MODULES
  callModule(relativeQuantification, "relative_quantification", rvalues)
  callModule(detailedVisualisation, "detailed_visualisation", rvalues)
  
  output$nav.bar.fold.changes.current <- renderUI({
    ns <- session$ns
    
    navbarPage("",
               tabPanel("Relative quantification"),
               tabPanel("Detailed visualisation"),
               id = ns("nav.bar.fold.changes.current"),
               collapsible = TRUE)
  })
}