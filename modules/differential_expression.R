source("./modules/differential_expression/t_test.R")
source("./modules/differential_expression/mann_whitney.R")

# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function.
differentialExpressionUI <- function(id) {
  ns <- NS(id)
  
  # DIFFERENTIAL EXPRESSION MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Differential expression'", "&&", "output['", ns("file.uploaded"),"']"),
      uiOutput(ns("nav.bar.diff.expr.current")),
      # DIFFERENTIAL EXPRESSION SUBMENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.diff.expr.current"),"'] == 't-test'"),
        tTestUI(ns("t_test"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.diff.expr.current"),"'] == 'Mann-Whitney'"),
        mannWhitneyUI(ns("mann_whitney"))
      )
    )
  )
}

differentialExpression <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  
  # RAW VISUALISATION RELATED MODULES
  callModule(tTest, "t_test", rvalues)
  callModule(mannWhitney, "mann_whitney", rvalues)
  
  output$nav.bar.diff.expr.current <- renderUI({
    ns <- session$ns
    
    navbarPage("",
               tabPanel("t-test"),
               tabPanel("Mann-Whitney"),
               id = ns("nav.bar.diff.expr.current"),
               collapsible = TRUE)
  })
}