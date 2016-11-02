library(shiny)

download_plot_ui <- function(format.id, width.id, height.id, resolution.id, button.save.id) { 
  fluidRow(
    column(2,
           selectInput(inputId = format.id, label = "File format",
                       choices = list("pdf" = "pdf",
                                    "eps" = "eps",
                                    "png" = "png",
                                    "jpg" = "jpg"))),
    column(2,
           numericInput(inputId = width.id, label = "Width",
                        value = 1920, min = 100, max = 2500)),
    column(2,
           numericInput(inputId = height.id, label = "Height",
                        value = 1080, min = 100, max = 2500)),
    column(2,
           numericInput(inputId = resolution.id, label = "Resolution (in dpi)", 
                        value = 200, min = 50, max = 600)),
    column(2, br(),
           downloadButton(outputId = button.save.id, label = "Save plot")))
}

download_plot_server <- function(plot_function, args, format, height, width, resolution) {
  downloadHandler(filename = paste("plot.", format, sep=""),
                  content = function(file) {
                    switch(format,
                           "pdf" = {
                             pdf(file = file,
                                 width = width/resolution,
                                 height = height/resolution)
                           },
                           "eps" = {
                             postscript(file = file,
                                        width = width/resolution,
                                        height = height/resolution)
                           },
                           "png" = {
                             png(file = file,
                                 width = width,
                                 res = resolution,
                                 height = height)
                           },
                           "jpg" = {
                             jpeg(file = file,
                                 width = width,
                                 res = resolution,
                                 height = height)
                           }
                    )
                    do.call(plot_function, args)
                    dev.off()           
                  },
                  contentType=paste('.', format, sep = ''))
}
 