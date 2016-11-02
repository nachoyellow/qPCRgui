source("./modules/raw_visualisation/overview_ct_groups.R")
source("./modules/raw_visualisation/spatial_layout.R")
source("./modules/raw_visualisation/variation.R")
source("./modules/raw_visualisation/duplicated_features.R")

# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
rawVisualisationUI <- function(id) {
  ns <- NS(id)
  
  # RAW VISUALISATION
  # RAW VISUALISATION MENU
  tagList(
    conditionalPanel(
      # Javascript receives just a String, it needs to be produced.
      condition = paste0("output['", ns("nav.bar.current"),"'] == 'Raw visualisation'", "&&", "output['", ns("file.uploaded"),"']"),
      uiOutput(ns("nav.bar.raw.visualisation.current")),
      # RAW VISUALISATION SUBMENU
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.raw.visualisation.current"),"'] == 'Overview of Ct across groups'"),
        overviewCtGroupsUI(ns("overview_ct_groups"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.raw.visualisation.current"),"'] == 'Spatial layout'"),
        spatialLayoutUI(ns("spatial_layout"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.raw.visualisation.current"),"'] == 'Variation within/across samples'"),
        variationUI(ns("variation"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("nav.bar.raw.visualisation.current"),"'] == 'Duplicated features within samples'"),
        duplicatedFeaturesUI(ns("duplicated_features"))
      )
    )
  )
}

rawVisualisation <- function(input, output, session, rvalues) {
  output$nav.bar.current <- reactive(rvalues$nav.bar.current)
  output$file.uploaded <- reactive(rvalues$file.uploaded)
  outputOptions(output, "nav.bar.current", suspendWhenHidden=FALSE)
  outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
  # This is needed for non-explicit outputs to be used in conditional panels' condition.

  # RAW VISUALISATION RELATED MODULES
  callModule(overviewCtGroups, "overview_ct_groups", rvalues)
  callModule(spatialLayout, "spatial_layout", rvalues)
  callModule(variation, "variation", rvalues)
  callModule(duplicatedFeatures, "duplicated_features", rvalues)
  
  output$nav.bar.raw.visualisation.current <- renderUI({
    ns <- session$ns
    
    if (!is.null(rvalues$processed.data)) {
      tabs <- list()
      tabs[[1]] <- ""
      tabs[[2]] <- tabPanel("Overview of Ct across groups")
      tabs[[3]] <- tabPanel("Spatial layout")
      tabs[[4]] <- tabPanel("Variation within/across samples")
      if (!any(table(featureNames(rvalues$processed.data)) %% 2 != 0)) {
        tabs[[5]] <- tabPanel("Duplicated features within samples")
      }
      args <- list()
      args$id <- ns("nav.bar.raw.visualisation.current")
      args$collapsible <- TRUE
      do.call(navbarPage, c(tabs, args))
    }
  })
}