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
#' @param nsample number of simulation runs, default is 1000.
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
                            start_strat, frame_CV_in, nBins, seed, minStrat,
                            nsample = 1000) {
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
    CHECK<-c(strat_var, domain_var, target_var)

    ###################################################
    shiny::validate(need(domain_var, message = F))
    shiny::validate(need(target_var, message = F),
                    need(strat_var, message = F),
                    need(frame_CV, message = F))
    FF<-data.table(FF)
    ## Selct only variables required
    FF<-FF[,c(domain_var, target_var, strat_var), with=F]
    inComplCases<-sum(!complete.cases(FF))
    ## Check for complete cases!!!
    if (inComplCases>0) {
      showNotification(paste(inComplCases, "Incomplete Observations have been dropped!
                                            If you are ok with this, go ahead, otherwise re-upload the
                                            frame with all complete cases."),
                       duration = NULL,
                       closeButton = T,
                       id = "complCases",
                       type = "error")
      FF<-FF[complete.cases(FF)]
      ## Check for empty
      if (nrow(FF)==0) {
        removeNotification("complCases")
        showNotification(paste("ERROR!", "No complete cases available. Upload required data please."),
                         duration = NULL,
                         closeButton = T,
                         id = "NoComplCases",
                         type = "error")
        req(FALSE)
      }
      shinyalert::shinyalert("Incomplete Cases", paste(inComplCases, "Incomplete Observations have been dropped.
                                                       If you are ok with this, go ahead, otherwise re-upload the
                                                       frame with all complete cases."))
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
      FFcont<-as.data.frame(FF[,strat_var_cont, with=F])
      pack_dp_sp<-c("SamplingStrata")
      nBins<-nBins()
      pack_dp_sp<-c("sampling", "ReGenesees", "dplyr")
      ## 3. Cluster Set up
      cores<-data.table::getDTthreads()
      cl<-parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      final<-foreach(i=1:simu, .packages = pack_dp_sp,
                     .combine=cbind,
                     .multicombine = T,
                     .errorhandling="pass") %dopar% {
                       t_cont<-strat_var_cont[i]
                       catVec<-var.bin(FFcont[,t_cont], bins=nBins)
                       return(catVec)
                     }
      parallel::stopCluster(cl)
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
    ##LOOP FOR NUMERIC TRANSFORMATION of all X and domainvalue
    strat_varX<-c("domainvalue",sprintf("%s%d", "X", 1:(length(strat_var))))
    for (s in strat_varX) {
      if(!is.numeric(frame_XY[[s]])) frame_XY[, (s) := as.numeric(as.factor(get(s)))]
    }
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
    ###       --> core check is base on data.table openMD
    withProgress(message = paste('Preparing data ...'),
                 value = 0,{
                   ##  1. Subset File
                   frameCVin<-(frame_CV_in())
                   frameXYin<-(frame_XY_final())
                   minStr<-minStrat()
                   req(frameXYin)
                   frame_STRAT_in<-buildStrataDF(frameXYin)
                   checkInput(frameCVin, frame_STRAT_in, frameXYin)
                   domcount<-frame_STRAT_in %>% dplyr::group_by(DOM1) %>% dplyr::summarise(count=dplyr::n_distinct(STRATO))
                   incProgress(message = "Searching for Optimal Number of Strata ...", amount = 0.2)
                   if ((data.table::getDTthreads())>1) {
                     ## multicore
                     set.seed(seed())
                     isolate({
                       solution<-tryCatch(
                         {SamplingStrata::optimStrata(
                           method = "atomic",
                           errors = force(frameCVin),
                           #realAllocation = F,
                           framesamp = force(frameXYin),
                           minnumstr = force(minStr),
                           nStrata = ceiling(domcount$count*0.5),
                           writeFiles = F,
                           iter = 100,
                           pops = 20,
                           #cores = force(data.table::getDTthreads()),
                           strcens = F,
                           parallel = T,
                           showPlot = F
                         )},
                         error = function(e) {showNotification(paste(" Optimization Error! Please try a different specification.
                                                                   The error message is: ", e), closeButton = T,
                                                               type = "error");
                           return((NULL))
                         })

                     })
                   } else {
                     ## single core
                     showNotification(paste("No multicore support available on this system, using single core for optimization"), closeButton = T,
                                      type = "warning")
                     set.seed(seed())
                     isolate({
                       solution<-tryCatch(
                         {SamplingStrata::optimStrata(
                           method = "atomic",
                           errors = force(frameCVin),
                           #realAllocation = F,
                           framesamp = force(frameXYin),
                           minnumstr = force(minStr),
                           nStrata = ceiling(domcount$count*0.5),
                           writeFiles = F,
                           iter = 50,
                           pops = 20,
                           cores = parallel::detectCores(),
                           strcens = F,
                           parallel = F,
                           showPlot = F
                         )},
                         error = function(e) {showNotification(paste(" Optimization Error! Please try a different specification.
                                                                   The error message is: ", e), closeButton = T,
                                                               type = "error");
                           return((NULL))
                         })

                     })
                   }
                   req(solution)
                   ## CREATE DESIGN TABLE
                   frame_STRAT_in_summay<-data.table(solution$aggr_strata %>%
                                                       dplyr::group_by(DOM1) %>%
                                                       summarise(NumberOfStrata=n_distinct(STRATO),
                                                                 SampleSize=sum(SOLUZ)))
                   solutionsList$frame_STRAT_in_summay<-frame_STRAT_in_summay
                   ## UPDATE FRAME
                   newstrata <- updateStrata(frame_STRAT_in,
                                             solution,
                                             writeFiles = F)
                   framenew <- updateFrame(frameXYin, newstrata, writeFiles=TRUE)
                   framenew$STRATUM1<-(as.numeric(sprintf("%d%d", framenew$DOMAINVALUE, framenew$LABEL)))
                   ## UPDATE DESIGN
                   aggr_strata<-as.data.table(solution$aggr_strata)
                   aggr_strata$STRATUM1<-(as.numeric(sprintf("%d%d", aggr_strata$DOM1, aggr_strata$STRATO)))
                   framenew<-as.data.table(framenew)
                   framenew<-merge(FF_final(), framenew, by="ID")
                   incProgress(message = "Evaluating Solution ...", amount = 0.4)
                   #####################################################
                   ## Check frame for single unit strata
                   ff<-framenew[,list(count=.N),by = list(DOMAINVALUE, STRATUM1)]
                   setorderv(ff, c("DOMAINVALUE", "STRATUM1"))
                   ff2<-frameCheck2(ff, PSU = "STRATUM1", n_hh = minStr, STRATUM = "DOMAINVALUE")
                   ## 1. Update frame with new strat
                   framenew<-merge(framenew, ff2[,.(DOMAINVALUE, STRATUM1, psu2, count, stratum2)], by=c("DOMAINVALUE","STRATUM1"))
                   data.table::setnames(framenew, "STRATUM1", "STRATUM1old")
                   data.table::setnames(framenew, "psu2","STRATUM1")
                   ## 2. Update design with new strata
                   aggr_strata<-merge(aggr_strata, (ff2[,.(STRATUM1, psu2)]), by = "STRATUM1")
                   data.table::setnames(aggr_strata, "STRATUM1", "STRATUM1old")
                   data.table::setnames(aggr_strata, "psu2","STRATUM1")
                   aggr_strata[,STRATO1:=as.numeric(stringr::str_remove(as.character(STRATUM1), as.character(DOM1)))]
                   aggr_strata<-aggr_strata[,list(SOLUZ=sum(SOLUZ), M1=mean(M1),
                                                  S1 = mean(S1), N=sum(N), COST = sum(COST)), by = list(DOM1, STRATUM1)]
                   aggr_strata[,SOLUZ:=ceiling(SOLUZ)]
                   aggr_strata<-data.table::setorderv(aggr_strata, "STRATUM1")

                   ###########################################
                   ## SIMULATION
                   ## 1. population mean
                   pop_mean<-framenew[,list(Mean = mean(get(target_var), na.rm = T)), by = c((domain_var))]
                   setorderv(pop_mean, domain_var)
                   pop_mean<-as.data.frame(pop_mean)
                   FF<-as.data.frame(framenew)
                   FF$uid<-1:nrow(FF)
                   ## 2. Stratum Sample Sizes
                   n_strat<-aggr_strata$SOLUZ
                   names(n_strat)<-as.character(sort(unique(FF$STRATUM1)))
                   #n_strat<-n_strat[order(names(n_strat))]
                   FF<-FF[order(FF$STRATUM1),]
                   pack_dp_sp<-c("sampling", "ReGenesees", "dplyr")
                   ## 3. Cluster Set up
                   cores<-data.table::getDTthreads()
                   cl<-parallel::makeCluster(cores) #change the 2 to your number of CPU cores
                   ## 4. Create var
                   tv<-paste0("~", target_var)
                   dom<-paste0("~", domain_var)
                   ## 5. Set SEED
                   doParallel::registerDoParallel(cl)
                   doRNG::registerDoRNG(seed())
                   TIME<-system.time(
                     final<<-foreach::foreach(i=1:nsample, .packages = pack_dp_sp,
                                              #.combine="rbind",
                                              .multicombine = T,
                                              .verbose = F,
                                              .export = c("FF", "n_strat", "pop_mean", "dom", "tv"),
                                              .errorhandling="pass") %dopar% {
                                                options(warn=-1)
                                                st=sampling::strata(FF,stratanames=c("STRATUM1"),
                                                                    size=n_strat, method="srswor")
                                                finalsamp<-sampling::getdata(FF, st)
                                                finalsamp$weight<-1/finalsamp$Prob
                                                finalsamp$STRATUM1<-as.factor(finalsamp$STRATUM1)
                                                final_samp_pp_svy<-ReGenesees::e.svydesign(data=finalsamp, ids=~uid, strata = ~STRATUM1,
                                                                                           weights=~weight, check.data = F)
                                                #######################################
                                                ## Age
                                                tab<-as.data.frame(ReGenesees::svystatTM(final_samp_pp_svy, y = eval(parse(text=tv)),
                                                                                         by = eval(parse(text=dom)),
                                                                                         estimator = c("Mean"),
                                                                                         vartype = c("se", "cv"),
                                                                                         na.rm=F))
                                                names(tab)<-c("Domain", "Mean", "SE", "CV")
                                                tab$bias<-((abs(tab$Mean-pop_mean$Mean))/pop_mean$Mean)
                                                return(tab)
                                              }
                   )
                   parallel::stopCluster(cl)
                   shiny::incProgress(0.5)
                   ## 5. Build data.table
                   final<-tryCatch(
                     {data.table::rbindlist(final)},
                     error = function(e) {showNotification(paste(" Optimization Error! Please try a different specification.
                                                                   The error message is: ", final[[1]]), closeButton = T,
                                                           type = "error");
                       return((NULL))
                     }
                   )
                   final[CV=="NaN", CV:=0][SE=="NaN", SE:=0][]
                   ## CREATE SAMPLE
                   set.seed(seed = seed())
                   st=sampling::strata(FF,stratanames=c("STRATUM1"),
                                       size=n_strat, method="srswor")
                   finalsamp<-sampling::getdata(FF, st)
                   finalsamp$weight<-1/finalsamp$Prob
                   finalsamp$STRATUM1<-as.factor(finalsamp$STRATUM1)
                   ## Design table
                   design<-data.table(aggr_strata)
                   ############################################
                   solutionsList$design<-design
                   solutionsList$FF_strat<-FF
                   solutionsList$simu<-final
                   solutionsList$finalsamp<-finalsamp
                   incProgress(message = "Preparing results ...", amount = 0.4)
                 })
  })

  ######################################################
  ## return
  return(list(
    finalDesign=reactive({solutionsList$design}),
    finalFrame=reactive({solutionsList$FF_strat}),
    final = reactive({solutionsList$simu}),
    finalSample = reactive({solutionsList$finalsamp})
  ))

}
