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
#' @param target_var the variable of interest
#' @param sampleSize sample size
#' @param balancedVariable set of variables to balance on
#' @param nsample Number of Simulation runs (recommended is 1000)
#' @param seed Set seed for MC with doRNG
#'
#' @return Data frame containing the final sample
#'
#' @importFrom foreach foreach "%dopar%"
#' @importFrom dplyr "%>%" group_by summarise
#' @importFrom doRNG "%dorng%"
#'
#' @export

samplingCubeSRV<-function(input, output, session, dataset, sampleSize, balancedVariable, target_var, nsample = 1000, seed) {

  finalFrame<- eventReactive(input$start_balanced,{
    FF<-dataset
    ss<- sampleSize()
    bv<-balancedVariable()
    req(FF, ss, bv)
    FF[,Prob:=ss/.N]
    FF
  })
  finalSample<-reactive({
    FF<-finalFrame()
    req(FF)
    ss<- sampleSize()
    bv<-balancedVariable()
    X<-as.matrix(FF[,bv, with = F])
    set.seed(seed())
    finalsamp<-FF[BalancedSampling::cube(FF$Prob, X),]
    finalsamp$weight<-nrow(FF)/nrow(finalsamp)
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
  evalResults<-reactive({
    shiny::withProgress(message = 'Simulation in progress',
                        detail = 'This may take a while...', value = 0.25, {
                          FF<-dataset
                          ss<- sampleSize()
                          bv<-balancedVariable()
                          tv<-target_var()
                          seed<-seed()
                          ## Cube sample procedure
                          ## - requires sample size
                          ## - calculate pik
                          ## 1. Pik
                          FF<-FF[,c(tv, bv), with=F]
                          FF[,Prob:=ss/.N]
                          FF[,weight:=1/Prob]
                          ## 2. UID
                          FF[,uid:=1:.N]
                          ## 3. Vars
                          #X<-as.matrix(FF[,bv, with = F])
                          ## 4. pop mean
                          pop_mean<-mean(FF[[tv]], na.rm = T)
                          pop_pop<-nrow(FF)
                          ## banlancing vars matrix
                          X<-as.matrix(FF[,c(bv), with=F])
                          ############################
                          ## MC approach
                          ##    -->no data.table inside !!!
                          FF<-as.data.frame(FF)
                          pack_dp_sp<-c("BalancedSampling", "sampling", "ReGenesees")
                          ## 3. Cluster Set up
                          cores<-parallel::detectCores()-0
                          cl<-parallel::makeCluster(cores) #change the 2 to your number of CPU cores
                          ## 4. Create var
                          tv<-paste0("~", tv)
                          ## 5. Set SEED
                          doParallel::registerDoParallel(cl)
                          doRNG::registerDoRNG(seed)
                          TIME<-system.time(
                            final<-foreach::foreach(i=1:nsample, .packages = pack_dp_sp,
                                                     #.combine="rbind",
                                                     .multicombine = T,
                                                     .verbose = F,
                                                     .export = c("FF", "tv", "X", "pop_mean", "ss", "pop_pop"),
                                                     .errorhandling="pass") %dopar% {
                                                       options(warn=-1)
                                                       finalsamp<-FF[BalancedSampling::cube(FF$Prob, X),]
                                                       finalsamp$weight<-pop_pop/nrow(finalsamp)
                                                       final_samp_pp_svy<-ReGenesees::e.svydesign(data=finalsamp, ids=~uid,
                                                                                                  weights=~weight, check.data = F)
                                                       #final_samp_pp_svy<-ReGenesees::des.addvars(final_samp_pp_svy, ones=1)
                                                       #######################################
                                                       ## Age
                                                       tab<-as.data.frame(ReGenesees::svystatTM(final_samp_pp_svy, y = eval(parse(text=tv)),
                                                                                                estimator = c("Mean"),
                                                                                                vartype = c("se", "cv"),
                                                                                                na.rm=F))
                                                       tab$bias<-((abs(tab$Mean-pop_mean))/pop_mean)
                                                       return(tab)
                                                     }
                          )
                          parallel::stopCluster(cl)
                          shiny::incProgress(0.5)
                          ## 5. Build data.table
                          final<-data.table::rbindlist(final)
                          final[CV=="NaN", CV:=0][SE=="NaN", SE:=0][]
                          #############################
                          ## write plot files out
                        })
    final
  })

  return(list(
    finalSample = finalSample,
    finalDesign = finalDesign,
    final = evalResults,
    finalFrame = finalFrame
  ))
}



















