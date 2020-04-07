#' Shiny UI module for simulation start
#'
#'
#'
#' @param id Namespace identifier
#'
#' @return CV and Relative Bias plot
#'
#'
#' @export


samplingStrataUI<-function(id) {
  ns<-NS(id)
  options(java.parameters = "- Xmx5g")
  tagList(
    fluidRow(
      column(4,
             imageOutput(ns("cv_plot"), width = 200, height = 200)
      ),
      column(4,
             imageOutput(ns("rel_bias"), width = 200, height = 200)
      ),
      column(4,
             DT::dataTableOutput(ns("design_summary"))
      )
    ),
    fluidRow(
      DT::dataTableOutput(ns("design_table"))
    )
  )
}


#' Shiny server module for simulation start
#'
#' @param input standard shiny input
#' @param output standard shiny output
#' @param session standard shiny session object
#' @param dataset frame dataset used for the stratification
#' @param domain_var the selected domain variables
#' @param target_var the selected target variables
#' @param strat_var_cat the selected categorical stratification variables
#' @param strat_var_cont the selected continous stratification variables (require bin to be set)
#' @param start_strat simulation start button
#' @param frame_CV_in dataframe of CVs by domain
#' @param nBins number of bins for the k-means clustering implemented by \code{SamplingStrata::var.bin()} for continous stratification variables
#' @param seed the random seed required for the optimization to be reproducable
#' @param minStrat this is the minimum number of units required per stratum
#'
#'
#' @return Data frame containing the final sample
#'
#' @importFrom future plan multicore
#' @importFrom doFuture registerDoFuture
#' @importFrom foreach foreach "%dopar%"
#' @importFrom dplyr "%>%" group_by summarise
#'
#' @export


samplingStrataSRV<-function(input, output, session, dataset, domain_var,
                            target_var, strat_var_cat, strat_var_cont,
                            start_strat, frame_CV_in, nBins, seed, minStrat) {
  ## styles
  smTab<-list(dom="tp")
  ## reactives
  solutionsList<-reactiveValues(); frame_XY_final<-reactiveVal(NULL); FF_final<-reactiveVal(NULL)
  observeEvent(start_strat() ,{
    ###################################################
    ## Collecting inputs
    FF<-dataset

    frame_CV<-frame_CV_in()
    domain_var<-domain_var()
    target_var<-target_var()
    strat_var_cat<-strat_var_cat()
    strat_var_cont<-strat_var_cont()
    strat_var<-c(strat_var_cat, strat_var_cont)
    ###################################################
    shiny::validate(need(domain_var, message = F))
    shiny::validate(need(target_var, message = F),
                    need(strat_var, message = F),
                    need(frame_CV, message = F))
    FF<-data.table(FF)
    inComplCases<-sum(!complete.cases(FF))
    if (inComplCases>0) {
      shinyalert::shinyalert("Incomplete Cases", paste(inComplCases, "Incomplete Observations have been dropped.
                                                       If you are ok with this, go ahead, otherwise re-upload the
                                                       frame with all complete cases."))
      FF<-FF[complete.cases(FF)]
    }
    FF[,ID:=1:.N]
    shiny::validate(need(FF, message = F))
    #################################################
    ##  1. Kmeans on CONT

    if (!is.null(strat_var_cont)){
      ##########################################
      ## if CONT, run loop with var.bin   #
      ## if CAT, take directly   #
      ##    - HOW TO MINIMIZE VARIANCE  ??     #
      ##########################################
      ##  MULITCORE LOOP
      ##  2. N simu and packages
      simu<-length(strat_var_cont)
      pack_dp_sp<-c("data.table", "SamplingStrata")
      nBins<-nBins()
      registerDoFuture()
      plan(multicore)
      final<-foreach(i=1:simu, .packages = pack_dp_sp,
                     .combine=cbind,
                     .multicombine = T,
                     #.export = c("a"),
                     #.verbose = T,
                     .errorhandling="pass") %dopar% {
                       t_cont<-strat_var_cont[i]
                       #set(FF, j=t_cont, value=var.bin(FF[[t_cont]], bins=3))
                       catVec<-var.bin(FF[[t_cont]], bins=nBins)
                       return(catVec)
                     }
      if(is.list(final)) {
        showNotification(paste("Error", paste(final$message, "please correct!")), closeButton = T,
                         type = "error")
        req(FALSE)
      }
      final<-data.table(final)
      names(final)<-strat_var_cont
      set(FF, j=strat_var_cont, value=NULL)
      FF<-copy(cbind(FF, final))

    }
    allVars<-c(domain_var, "ID", target_var, strat_var)
    if(is.numeric(FF[[domain_var]])) {
      FF[,domain_var:= as.character(get(domain_var))]
    }
    frame_XY<-FF[,allVars, with=F]
    names(frame_XY)<-c("domainvalue",
                       "ID",
                       sprintf("%s%d", "Y", 1:length(target_var)),
                       sprintf("%s%d", "X", 1:(length(strat_var))))
    ##LOOP FOR NUMERIC
    strat_varX<-c("domainvalue",sprintf("%s%d", "X", 1:(length(strat_var))))
    frame_XY<-data.table(frame_XY)
    for (s in strat_varX) {
      frame_XY[, (s) := as.numeric(as.factor(get(s)))]
    }
    frame_XY<-frame_XY[complete.cases(frame_XY)]
    frame_XY<-as.data.frame(frame_XY)
    frame_XY_final(frame_XY)
    FF_final(FF)
  })

  #####################################
  ## stratum optimization is carried out separate.
  observe({

    domain_var<-domain_var()
    target_var<-target_var()
    strat_var_cat<-strat_var_cat()
    strat_var_cont<-strat_var_cont()
    strat_var<-c(strat_var_cat, strat_var_cont)
    req(strat_var)
    ###########################################################
    ###     EXECUTE ISOLATE OPTIMIZATION
    withProgress(message = paste('Preparing data ...'),
                 value = 0,{
                   ##  1. Subset File
                   frameCVin<-(frame_CV_in())
                   frameXYin<-(frame_XY_final())
                   minStr<-minStrat()
                   req(frameXYin)
                   frame_STRAT_in<-buildStrataDF(frameXYin)
                   checkInput(frameCVin, frame_STRAT_in, frameXYin)
                   incProgress(message = "Searching for Optimal Number of Strata ...", amount = 0.2)

                   set.seed(seed())
                   isolate({
                     solution<-tryCatch(
                       {optimizeStrata(errors = force(frameCVin),realAllocation = F,
                                       strata = force(frame_STRAT_in), minnumstr = force(minStr),
                                       writeFiles = F, iter = 50, pops = 20, cores = future::availableCores()-2,
                                       strcens = F, parallel = T, showPlot = F)},
                       error = function(e) {showNotification(paste(" Optimization Error! Please try a different specification."), closeButton = T,
                                                             type = "error");
                         return((NULL))
                       })

                   })
                   req(solution)
                   solutionsList$frame_STRAT_in_summay<-data.table(solution$aggr_strata %>%
                                                                     dplyr::group_by(DOM1) %>%
                                                                     summarise(NumberOfStrata=n_distinct(STRATO),
                                                                               SampleSize=sum(SOLUZ)))
                   solutionsList$n_total<-sum(ceiling(solution$aggr_strata$SOLUZ))
                   newstrata <- updateStrata(frame_STRAT_in,
                                             solution,
                                             writeFiles = F)
                   framenew <- updateFrame(frameXYin, newstrata, writeFiles=TRUE)
                   incProgress(message = "Evaluating Solution ...", amount = 0.4)
                   framenew$DOMAINVALUE<-as.numeric((framenew$DOMAINVALUE))
                   checkframenew<-framenew
                   checksolution<-solution
                   ##################
                   ## check mc
                   solutionsList$eval<-tryCatch(evalSolution(frame = framenew,
                                                             outstrata = solution$aggr_strata,
                                                             nsampl=100,
                                                             writeFiles=T,
                                                             progress=F),
                                                error = function(e) print(e))
                   framenew<-data.table(framenew)
                   framenew[,STRATUM1:=(as.numeric(sprintf("%d%d", DOMAINVALUE, LABEL)))]
                   ## Design table
                   design<-data.table(solution$aggr_strata)
                   design[,STRATUM1:=(as.numeric(sprintf("%d%d", DOM1, STRATO)))]
                   solutionsList$design<-design
                   solutionsList$FF_strat<-merge(FF_final(), framenew, by="ID")
                   incProgress(message = "Preparing results ...", amount = 0.4)
                 })
  })

  ###########################################
  ##  PLOT
  ###########################################

  output$cv_plot<-renderImage({
    fr<-solutionsList$eval
    shiny::validate(need(fr, message = F))
    cv_graph<-list(
      src="simulation/cv.png",
      contentType = "image/png",
      alt = "CV")
    return(cv_graph)
  }, deleteFile = F)
  output$rel_bias<-renderImage({
    sol<-solutionsList$eval
    shiny::validate(need(sol, message = F))
    bias_graph<-list(
      src="simulation/rel_bias.png",
      contentType = "image/png",
      alt = "Bias")
    return(bias_graph)
  }, deleteFile = F)
  ## All strata table
  output$design_table<-DT::renderDataTable({
    DT::datatable(solutionsList$design)
  })
  ## Summary table
  output$design_summary<-DT::renderDataTable({
    tab<-solutionsList$design
    req(tab)
    Total<-tab[,list(Sample = sum(SOLUZ), Cost = sum(COST), Strata = length(unique(STRATUM1)))][,DOM1:="Total"]
    Domain<-tab[,list(Sample = sum(SOLUZ), Cost = sum(COST), Strata = length(unique(STRATUM1))), by=DOM1]
    tab<-rbindlist(list(Total, Domain), idcol = "Level", fill = T)
    DT::datatable(tab, rownames = F, options = smTab)
  })
  ## return
  return(list(
    finalSample=reactive({solutionsList$FF_strat}),
    finalDesign=reactive({solutionsList$design})
  ))

}
