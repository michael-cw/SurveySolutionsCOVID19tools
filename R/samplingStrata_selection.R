#' Shiny UI module for SamplingStrata variable selection
#'
#' @description This module creates the inputs and also updates them after dataset is
#' available. Due to the construction of this module, the inputs are availble in the
#' global environment under input$[id-ns(inputs)]. This means to reference i.e to
#' the selectizeInput ns("strat_var_cont) with the hypotetical id "stratInputs" you have
#' to use input$`stratInputs-strat_var_cont`.
#'
#' @param id Namespace identifier
#'
#' @return file with data.table of uploaded data
#'
#'
#' @export
samplingStrataInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    selectizeInput(ns("domain_var"), "Domain Variable", choices = c(""),
                   options = list(
                     placeholder = 'Upload frame first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("target_var"), "Target Variable(s)", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload frame first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("strat_var_cat"), "Categorical Variable", choices = c(""),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload frame first',
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    ),
    selectizeInput(ns("strat_var_cont"), "Continous Variable", choices = character(0),
                   multiple=T,
                   options = list(
                     placeholder = 'Upload frame first'
                   )
    )
  )
}

#' Shiny server module for SamplingStrata variable selection
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
#' @export


samplingStrataOutput <- function(input, output, session, dataset) {
  ##  2. Update Input fields
  observe({
    FF<-dataset
    shiny::validate(need(FF, message = F))
    #################################
    ## Domain
    updateSelectizeInput(session = session, inputId = "domain_var",
                         label = "Domain Variable",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    #################################
    ## Target Var
    updateSelectizeInput(session = session, inputId = "target_var",
                         label = "Target Variable(s)",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    #################################
    ## Stratification CAT
    updateSelectizeInput(session = session, inputId = "strat_var_cat",
                         label = "Categorical Variable(s)",
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) bellow',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )

    #################################
    ## Stratification CONT
    updateSelectizeInput(session = session, inputId = "strat_var_cont",
                         label = "Continous Variable(s)", server = T,
                         choices = names(FF),
                         options = list(
                           placeholder = 'Select variable(s) bellow'
                         )
    )
  })

}
