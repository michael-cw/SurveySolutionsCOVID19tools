#' Shiny UI module to start balanced sampling
#'
#' @param id Namespace identifier
#'
#'
#' @return rhandsontable with SamplingStrata input parameters
#'
#' @export
samplingCubeUI<-function(id) {
  ns<-NS(id)
  options(java.parameters = "- Xmx5g")
  tagList(
    actionButton(ns("start_balanced"), "Start Cube Sampling", width = "100%")
  )
}

#' Shiny server module for simulation start
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset frame dataset used for the stratification
#' @param sampleSize sample size
#' @param balancedVariable set of variables to balance on
#' @param nsample Number of Simulation runs (recommended is 1000)
#'
#' @return Data frame containing the final sample
#'
#' @importFrom future plan multicore
#' @importFrom doFuture registerDoFuture
#' @importFrom foreach foreach "%dopar%"
#' @importFrom dplyr "%>%" group_by summarise
#'
#' @export

samplingCubeSRV<-function(input, output, session, dataset, sampleSize, balancedVariable, nsample = 500) {

  finalSample<-eventReactive(input$start_balanced, {
    FF<-dataset
    ss<- sampleSize()
    bv<-balancedVariable()
    req(FF, ss, bv)
    ## Cube sample procedure
    ## - requires sample size
    ## - calculate pik
    ## 1. Pik
    FF[,pik:=ss/.N]
    X<-as.matrix(FF[,bv, with = F])
    finalsamp<-FF[BalancedSampling::cube(FF$pik, X),]
    finalsamp
  })

  finalDesign<-reactive({
    bv<-balancedVariable()
    req(bv, finalSample())
    X<-finalSample()[,bv, with = F]
    sumTab<-data.table(sapply(as.data.frame(X) , function(x) cbind(
      mean = mean(x),
      sd = stats::sd(x),
      median = stats::median(x),
      minimum = min(x),
      maximum = max(x),
      s.size = length(x)),
      simplify = T))
    sumTab[,Variable:=c("Mean", "SD", "Median", "Min", "Max", "n")]
    sumTab
  })
  #####################
  ## eval results
  evalsamp<-reactive({
    FF<-finalSample()
    req(FALSE)

  })

  return(list(
    finalSample = finalSample,
    finalDesign = finalDesign
  ))
}



















