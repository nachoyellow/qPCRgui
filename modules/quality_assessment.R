source("./modules/quality_assessment/correlation_between_samples.R")
source("./modules/quality_assessment/distribution_of_ct_values.R")
source("./modules/quality_assessment/comparison_two_samples.R")
source("./modules/quality_assessment/scatter_across_samples.R")
source("./modules/quality_assessment/ct_heatmaps.R")
source("./modules/quality_assessment/coef_variations.R")

# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
qualityAssessmentUI <- function(id) {
  ns <- NS(id)
  
  # QUALITY ASSESSMENT
  # QUALITY ASSESSMENT MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Quality assessment'", "&&", "output['", ns("file.uploaded"),"']"),
      uiOutput(ns("nav.bar.quality.assessment.current")),
      # QUALITY ASSESSMENT SUBMENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Correlation between samples'"),
        correlationBetweenSamplesUI(ns("correlation_between_samples"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Distribution of Ct values'"),
        distributionOfCtValuesUI(ns("distribution_of_ct_values"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Comparison of Ct values for two samples'"),
        comparisonTwoSamplesUI(ns("comparison_two_samples"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Scatter across samples'"),
        scatterAcrossSamplesUI(ns("scatter_across_samples"))
      ),      
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Ct heatmaps'"),
        ctHeatmapsUI(ns("ct_heatmaps"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.quality.assessment.current"),"'] == 'Coefficients of variation'"),
        coefVariationUI(ns("coef_variations"))
      )
    )
  )
}

qualityAssessment <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.
  
  # QUALITY ASSESSMENT RELATED MODULES
  callModule(correlationBetweenSamples, "correlation_between_samples", rvalues)
  callModule(distributionOfCtValues, "distribution_of_ct_values", rvalues)
  callModule(comparisonTwoSamples, "comparison_two_samples", rvalues)
  callModule(scatterAcrossSamples, "scatter_across_samples", rvalues)
  callModule(ctHeatmaps, "ct_heatmaps", rvalues)
  callModule(coefVariation, "coef_variations", rvalues)
  
  output$nav.bar.quality.assessment.current <- renderUI({
    ns <- session$ns
   
    navbarPage("",
                   tabPanel("Correlation between samples"),
                   tabPanel("Distribution of Ct values"),
                   tabPanel("Comparison of Ct values for two samples"),
                   tabPanel("Scatter across samples"),                   
                   tabPanel("Ct heatmaps"),
                   tabPanel("Coefficients of variation"),
                   id = ns("nav.bar.quality.assessment.current"),
                   collapsible = TRUE)
  })
}