# Namespace is needed for modular UI functions. NS(id) creates a namespace for this explicit ID.
# Namespace is used for all input/output variables in the UI function. (ns)
dataUploadUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    conditionalPanel(
      condition = paste0("!output['",ns("file.uploaded"),"']"),
      # Javascript receives just a String, it needs to be produced.
      
      helpText("Choose one:"),
      radioButtons(inputId = ns("radio"), label = NULL, 
                   choices = list("Sample data" = 1, "Upload file" = 2), 
                   selected = 1, 
                   inline = TRUE),
      conditionalPanel(
        condition = paste0("input['",ns("radio"),"'] == 1"),
        actionButton(inputId = ns("sample.data.submit"),
                     label = "Submit") 
      ),
      conditionalPanel(
        condition = paste0("input['",ns("radio"),"'] == 2"),
        fileInput (inputId = ns("data.loader"), 
                   label = "Upload Files", 
                   multiple = TRUE) # Multiple files are required.
      )
    ) 
  )
}

dataUpload <- function(input, output, session, rvalues) {
  lrv <- reactiveValues()
  lrv$sample.data <- NULL
  
  observeEvent(input$sample.data.submit, {
    lrv$sample.data <- TRUE
  })
  
  getRawData <- reactive({
    data <- rvalues$raw.data
    # Check for changes in the loaded files, reprocess in the case of the following:
    # (1) Files have been added to the file input and file names haven't been processed.
    # (2) Processed file names' length and file input's file names' length are different.
    # (3) Some names are different amongst processed and file input ones.
    if (!is.null(input$data.loader) && 
        (is.null(rvalues$file.names) || 
         length(input$data.loader$name)!=length(rvalues$file.names) || 
         any(sort(input$data.loader$name)!=sort(rvalues$file.names)))) {
      # Create a temporary directory
      rvalues$directory <- paste0(tempdir(),"/",gsub(" ","_",date()))
      dir.create(rvalues$directory)
      
      # Rename the files to reset their original name and copy them to the temporary directory
      sapply(1:nrow(input$data.loader), 
             FUN=function(i) {
               file.copy(input$data.loader$datapath[i], paste0(rvalues$directory, "/",
                                                               input$data.loader$name[i]))
             })
      
      # Update the rvalues object
      rvalues$raw.file.names <- sort(input$data.loader$name)
      rvalues$raw.files <- read.delim(file.path(rvalues$directory, "files.txt"))
      rvalues$raw.data <- readCtData(files=rvalues$raw.files$File, path=rvalues$directory)
      rvalues$file.names <- rvalues$raw.file.names
      rvalues$files <- rvalues$raw.files
      rvalues$processed.data <- rvalues$raw.data
      data <- rvalues$raw.data
    }
    
    else {
      if (!is.null(lrv$sample.data) && lrv$sample.data) {
        rvalues$directory <- file.path(".", "HTqPCR_samples")
        rvalues$raw.file.names <- c("sample1.txt", "sample2.txt", "sample3.txt", "sample4.txt", "sample5.txt", "sample6.txt")
        rvalues$raw.files <- read.delim(file.path(rvalues$directory, "files.txt"))
        rvalues$raw.data <- readCtData(files=rvalues$raw.files$File, path=rvalues$directory)
        rvalues$file.names <- rvalues$raw.file.names
        rvalues$files <- rvalues$raw.files
        rvalues$processed.data <- rvalues$raw.data
        data <- rvalues$raw.data
        lrv$sample.data <- FALSE
      }
    }
    return(data)
  })
  
  file.uploaded <- reactive({
    # If there is data uploaded to the system, getRawData shouldn't be NULL.
    return(!is.null(getRawData()))
  })
  observeEvent(file.uploaded(), {
    rvalues$file.uploaded <- file.uploaded()
    output$file.uploaded <- reactive(rvalues$file.uploaded)
    outputOptions(output, "file.uploaded", suspendWhenHidden=FALSE)
    # This is needed for non-explicit outputs to be used in conditional panels' condition.
  })

}