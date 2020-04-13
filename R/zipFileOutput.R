#' Shiny UI module for data download
#'
#' @description Designed to handle large frame files by a separated compilation and download process.
#'
#' @param id Namespace identifier
#' @param label Download button label
#'
#' @return Download of Sample resources
#'
#' @importFrom shinyjs useShinyjs
#'
#' @export

downloadBigUI<-function(id, label) {
  ## style
  invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;visibility:hidden; ")
  ns<-NS(id)
  options(java.parameters = "- Xmx5g")
  tagList(
    useShinyjs(rmd = T),
    actionButton(inputId = ns("dwl_frame"), label = label, width = "100%"),
    downloadButton(ns("dwl_frame_dwl"), "Not visible",
                   style=invisibleButton)
  )
}



#' Shiny server module for download of 2 csv files in singel zip file
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param frame_data frame for download (1. dataset)
#' @param design design for download (2. dataset)
#' @param sample the final sample
#' @param seed the seed of the random process to reproduce (inlcude in file name)
#' @param modid the id of namespace to be used in shinyjs
#'
#' @importFrom readr write_csv
#' @importFrom shinyjs runjs
#' @importFrom stringr str_remove_all
#'
#' @export

downloadBig<-function(input, output, session,
                      frame_data, design, sample,
                      seed, modid) {

  ####################################
  ##  2. Download
  ##  2.1. Prepare Data
  fs_frame<-reactiveVal(); ds_frame<-reactiveVal()
  sa_frame<-reactiveVal()
  observeEvent(input$dwl_frame, {
    fs_frame(frame_data); ds_frame(design); sa_frame(sample)
    ## REQUIRES TO USE FULL NAME (WITH ns PART provided by modid)
    runjs(paste0("$('#", modid,"-dwl_frame_dwl')[0].click();"))
  })

  ## 2.2 Download Data
  output$dwl_frame_dwl <- downloadHandler(filename = function() {
    st<-paste0(str_remove_all(Sys.time(), "[:space:]|[:punct:]"), "_", seed())
    paste("StratifiedFrame-", st, ".zip", sep="")
  },
  content = function(file) {
    st<-paste0(str_remove_all(Sys.time(), "[:space:]|[:punct:]"), "_", seed())
    fs <- paste("StratifiedFrame-", st, ".csv", sep="")
    ds <- paste("Design-", st, ".csv", sep="")
    sa <- paste("Sample-", st, ".csv", sep="")
    temp.dir<-tempdir()
    wdOld<-getwd()
    setwd(temp.dir)
    on.exit(setwd(wdOld))
    withProgress(message = paste('Preparing data for download'),
                 value = 0, {
                   ##  CSV only (too big for excel)
                   write_csv(fs_frame(), fs)
                   write_csv(ds_frame(), ds)
                   write_csv(sa_frame(), sa)
                   incProgress(1/2)
                   zip::zipr(zipfile=file, files= c(fs, ds, sa), include_directories = F)
                   incProgress(1/2)
                 })
  }, contentType = "application/zip")
}





















