library(shiny)
library("HTqPCR")
library("DT")

source("./modules/header.R")
source("./modules/data_upload.R")
source("./modules/data.R")
source("./modules/raw_visualisation.R")
source("./modules/filtering.R")
source("./modules/normalisation.R")
source("./modules/quality_assessment.R")
source("./modules/clustering.R")
source("./modules/differential_expression.R")
source("./modules/fold_changes.R")
source("./modules/tools/download_plot.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 100MB.
options(shiny.maxRequestSize = 100*1024^2)
options(xtable.include.colnames=F)

ui <- fluidPage(theme = "app.css", title = "Quantitative real-time PCR data: cycle threshold analysis",
  # HEADER
  headerUI("header"),
  
  # DATA UPLOAD
  dataUploadUI("data_upload"),
  
  # DATA
  dataUI("data"),
  
  # RAW VISUALISATION
  rawVisualisationUI("raw_visualisation"),
  
  # FILTERING
  filteringUI("filtering"),
  
  # NORMALISATION
  normalisationUI("normalisation"),
  
  # QUALITY ASSESSMENT
  qualityAssessmentUI("quality_assessment"),
  
  # CLUSTERING
  clusteringUI("clustering"),
  
  # DIFFERENTIAL EXPRESSION
  differentialExpressionUI("differential_expression"),
  
  # FOLD CHANGES
  foldChangesUI("fold_changes"),
  
  br()
)

server <- function(input, output) {
  # REACTIVE VALUES
  rvalues <- reactiveValues()
  
  rvalues$nav.bar.current <- NULL
  
  rvalues$raw.file.names <- NULL
  rvalues$raw.files <- NULL
  rvalues$file.names <- NULL
  rvalues$directory <- NULL
  rvalues$files <- NULL
  rvalues$file.uploaded <- NULL
  
  rvalues$raw.data <- NULL
  rvalues$processed.data <- NULL
  rvalues$differential.expression <- NULL
  rvalues$normalised <- NULL
  
  # HEADER
  callModule(header, "header", rvalues)
  
  # DATA UPLOAD
  callModule(dataUpload, "data_upload", rvalues)
  
  # DATA
  callModule(data, "data", rvalues)
  
  # RAW VISUALISATION
  callModule(rawVisualisation, "raw_visualisation", rvalues)
  
  # FILTERING
  callModule(filtering, "filtering", rvalues)
  
  # NORMALISATION
  callModule(normalisation, "normalisation", rvalues)
  
  # QUALITY ASSESSMENT
  callModule(qualityAssessment, "quality_assessment", rvalues)
  
  # CLUSTERING
  callModule(clustering, "clustering", rvalues)
  
  # DIFFERENTIAL EXPRESSION
  callModule(differentialExpression, "differential_expression", rvalues)
  
  # FOLD CHANGES
  callModule(foldChanges, "fold_changes", rvalues)
}

shinyApp(ui = ui, server = server)