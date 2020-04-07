#' Shiny UI module to select variables for balanced sampling
#'
#' @param id Namespace identifier
#'
#'
#' @return Sample Size
#'
#' @export
#'

samplingCubeInput <- function(id) {
  ns1 <- NS(id)

  tagList(
    radioButtons(ns1("targetVarType"), "Targe Variable Type",
                 c("Categorical"=1, "Continous"=2),
                 selected = 1,
                 inline = T
    ),
    selectizeInput(ns1("targetvar"), "Target Variable", choices = c(""),
                   multiple=F,
                   options = list(
                     placeholder = 'Upload frame first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    numericInput(ns1("precision"),
                 label = "Required Precision",
                 value = 0.05,
                 min = 0,
                 max = 0.5,
                 step = 0.01
    ),
    numericInput(ns1("sampleSize"),
                 label = "Sample Size",
                 value = 0,
                 min = 0,
                 step = 1
    ),
    br(), br(),
    selectizeInput(ns1("bal_var"), "Balancing Variable(s)", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload frame first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  )


}

#' Shiny server module for sampling::samplecube balancing variable selection
#'
#' @description This module creates the inputs and also updates them after dataset is
#' available. Due to the construction of this module, the inputs are availble in the
#' global environment under input$[id-ns(inputs)]. This means to reference i.e to
#' the selectizeInput ns("strat_var_cont) with the hypotetical id "stratInputs" you have
#' to use input$`stratInputs-strat_var_cont`.
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset is the dataset provided by the user
#'
#' @importFrom PracTools nCont nProp
#' @importFrom stats sd
#' @export

samplingCubeOutput <- function(input, output, session, dataset) {

  observe({
    FF<-dataset
    shiny::validate(need(FF, message = F))
    #################################
    ## Target Var
    updateSelectizeInput(session = session, inputId = "targetvar",
                         label = "Target Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    updateSelectizeInput(session = session, inputId = "bal_var",
                         label = "Balancing Variables",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
  })
  allInputs<-reactive({
    list(input$targetvar, input$precision)
  })

  observeEvent(allInputs(), {
    FF<-dataset
    req(FF); req(input$targetvar)
    shiny::validate(need(input$precision>0, message = F))
    ############################
    ## Sample Size

    if(input$targetVarType==1) {
      pmean<-mean(FF[[input$targetvar]], na.rm = T)
      shiny::validate(need(0<pmean & pmean<1, message = F))
      # srs<-ceiling(sample.size.prop(e = input$precision,
      #                               P = pmean,
      #                               N = nrow(FF), level = 0.95)$n)
      srs<-ceiling(nProp(CV0 = input$precision, pU = pmean, N = nrow(FF)))
    } else if(input$targetVarType==2) {
      pmean<-mean(FF[[input$targetvar]], na.rm = T)
      pvar<-stats::var(FF[[input$targetvar]], na.rm = T)
      # srs<-ceiling(sample.size.mean(e =input$precision*pmean,
      #                               S = psd,
      #                               N = nrow(FF)
      # )$n)
      srs<-ceiling(nCont(CV0 = input$precision, N = nrow(FF), ybarU = pmean, S2 = pvar))
    }
    updateNumericInput(session = session, inputId ="sampleSize",
                       label = "Sample Size",
                       value = srs,
                       min = 0,
                       step = 1
    )



  })
}



























