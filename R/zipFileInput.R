#' Shiny UI module for upload of text (csv, tab) file
#'
#'
#'
#' @param id Namespace identifier
#' @param label File input label
#' @param accept Mime type (i.e. must be one of)
#' @return file input
#'
#'
#'
#' @export
zipFileInput <- function(id, label, accept) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("frame_file"), label, multiple = F,
              accept = accept)
  )
}

#' Shiny server module for upload of text (csv, tab) file
#'
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param sep column separator, one of , ; or "\\t"
#'
#'
#' @return returns datatable with upload data by using data.table's fread function
#'
#'
#' @export

zipFile <- function(input, output, session, sep = ",") {
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$frame_file, message = FALSE))
    input$frame_file
  })

  frameFile<-reactive({
    data.table::fread(userFile()$datapath, sep = sep)
  })

  return(frameFile)

}
