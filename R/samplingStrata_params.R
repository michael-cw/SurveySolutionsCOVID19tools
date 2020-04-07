#' Shiny UI module to create a rhandsontable for SamplingStrata inputs
#'
#'
#'
#' @param id Namespace identifier
#'
#'
#' @return rhandsontable with SamplingStrata input parameters
#'
#'
#' @export
#'
samplingStrataInput_para <- function(id) {
  ns <- NS(id)
  tagList (
    actionButton(ns("start_strat"), "Start Stratified Sampling", width = "100%"),
    br(),
    rHandsontableOutput(ns("hotout"), height = 400, width = "100%")
  )
}


#' Shiny server module to create a rhandsontable for SamplingStrata inputs
#'
#'
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset frame dataset used for the stratification
#' @param domain_var the selected domain variables
#' @param target_var the selected target variables
#'
#'
#' @export
#'
#'
samplingStrataOutput_para <- function(input, output, session,
                                      dataset,
                                      target_var,
                                      domain_var) {
  frame_CV<-eventReactive(target_var(),{
    FF<-dataset
    domain_var<-domain_var()
    target_var<-target_var()
    shiny::validate(need(domain_var, message = F))
    shiny::validate(need(target_var, message = F))
    FF<-data.table(FF)
    frame_CV<-matrix(0.05, nrow = length(unique(FF[[domain_var]])),
                     ncol = (length(target_var)))
    frame_CV<-data.frame(frame_CV)
    names(frame_CV)<-c(sprintf("CV%d", 1:length(target_var)))
    frame_CV
  })

  output$hotout<-renderRHandsontable({
    frame_CV<-frame_CV()
    rhandsontable((frame_CV))
  })

  frame_CV_in<-eventReactive(input$start_strat, {
    #shiny::validate(need(hot, message = F))
    frame_CV<-hot_to_r(input$hotout)
    FF<-dataset
    frame_CV$DOM<-rep("DOM1", length(unique(FF[[domain_var()]])))
    frame_CV$domainvalue<-1:length(unique(FF[[domain_var()]]))
    if(dir.exists("simulation")) unlink("simulation", recursive = T)
    if(dir.exists("output")) unlink("output", recursive = T)
    (frame_CV)
  })

  return(list(frame_CV_in = frame_CV_in))
}
