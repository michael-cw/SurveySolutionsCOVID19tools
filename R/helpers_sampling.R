# ##########################################################################################################################
# ##    STRATIFIED SAMPLING DATA TABLE
# ##########################################################################################################################
# strataDT<-function(data, STRATUM="stratum", n, returnDT=T){
#   data_in<-copy(data)
#   if(!is.data.table(data_in)) data<-data.table(data_in)
#   if(length(n)==1) n<-rep(n,length(unique(data_in[[STRATUM]])))
#   stratum_q<-substitute(STRATUM)
#
#   old_class<-class(data_in[[STRATUM]])
#   #print(STRATUM)
#   #print(str(data_in))
#   if(!is.factor(data_in[[STRATUM]])) data_in[,(STRATUM) :=as.factor(get((stratum_q)))]
#
#   setkeyv(data_in, STRATUM)
#   data_in[,pik_str:=numeric(.N)]
#   loops<-levels(data_in[[STRATUM]])
#   fullDT<-data_in[0]
#   k=1
#   for (s in loops){
#     tmp<-data_in[.(s)]
#     n0<-n[k]
#     tmp[,pik_str:=n0/.N]
#     tmp<-tmp[,.SD[sample(.N, n0)]]
#     fullDT<-rbind(fullDT, tmp)
#     k=k+1
#   }
#
#   if(old_class=="numeric") fullDT[,(STRATUM) :=as.numeric(as.character(get(stratum_q)))]
#   return(fullDT)
# }
#
#
# ##########################################################################################################################
# ##    PPS SAMPLING DATA TABLE
# ##########################################################################################################################
# ppsDT<-function(data, clu="cluster", n, sizevar="HH_count"){
#   data_in<-data
#   if(!is.data.table(data))
#     data_in<-data.table(data)
#
#   setkeyv(data_in, clu)
#   ##print(key(data_in))
#   ##  Preparing the data
#   int<-sum(data_in[[sizevar]])/n
#   data_in[,pik:=n*(data_in[[sizevar]]/sum(data_in[[sizevar]]))]
#   ##  Creating Random Order
#   data_in[sample(.N)]
#   ##  Cumulative sum
#   data_in[,cumul:=cumsum(data_in[[sizevar]])]
#   ##  Random start
#   rn<-runif(1, 0, int)
#   ##  Creat intervall groups and select firs per group
#   data_in[,int_group:=cut(cumul, seq(rn, (n)*int, by=int), labels=FALSE)]
#   data_out<-data_in
#   data_out<-data_out[,.SD[1], by=int_group]
#   data_out<-data_out[,c("cumul", "int_group"):=NULL]
#   data_out<-setkeyv(data_out, clu)
#   return(data_out)
# }
#
#
#
# ##########################################################################################################################
# ##    PSU sampling with stratification at PSU level
# ##########################################################################################################################
#
# strataDT_psu<-function(data, STRATUM1="stratum", n1, PSU="stratum2", n2 , returnDT=T){
#   ##  Copy the data
#   data_in<-copy(data)
#   ##  Check for DT
#   if(!is.data.table(data_in)) data_in<-data.table(data_in)
#   ##  Check if single number or vector for STR1
#   if(length(n1)==1) n1<-rep(n1,length(unique(data_in[[STRATUM1]])))
#
#   ##  Save old class
#   old_class1<-class(data_in[[STRATUM1]])
#   old_class2<-class(data_in[[PSU]])
#   ##  Change to FACTOR
#   if(!is.factor(data_in[[STRATUM1]])) data_in[,(STRATUM1) :=as.factor(get(STRATUM1))]
#   if(!is.factor(data_in[[PSU]])) data_in[,(PSU) :=as.factor(get(PSU))]
#   ##  STRATUM1
#   setkeyv(data_in, STRATUM1)
#   data_in[,pik_str:=numeric(.N)]
#   loops<-levels(data_in[[STRATUM1]])
#   fullDT<-data_in[0]
#   k=1
#   for (s in loops){
#     tmp<-data_in[.(s)]
#     n0<-n1[k]
#     loops2<-levels(data_in[[PSU]])
#     N00<-table(tmp[[PSU]])
#     n00<-round((N00/sum(N00))*n0, digits = 0)
#     j=1
#     #print("********")
#     #print(tmp)
#     setkeyv(tmp, PSU)
#     for (p in loops2){
#       tmp1<-tmp[.(p)]
#       ##print(tmp1)
#       if(nrow(tmp)==0) next
#       n000<-n00[j]
#       #############################################################
#       ##    ATTENTION: when n is smaller 1, then set to 1 (THINK about other solutions)
#       if(n000==0) n000<-1
#       #############################################################
#       tmp1[,pik_str:=n000/.N]
#       tmp1<-tmp1[,.SD[sample(.N, n000)]]
#       fullDT<-rbind(fullDT, tmp1)
#       j=j+1
#     }
#     k=k+1
#   }
#   fullDT<-fullDT[complete.cases(fullDT)]
#   if(old_class1=="numeric") fullDT[,(STRATUM1) :=as.numeric(as.character(get(STRATUM1)))]
#   return(fullDT)
# }
#
# ##########################################################################################################################
# ##    PSU sampling with stratification at PSU level und PPS
# ##########################################################################################################################
#
# strataDT_psu_pps<-function(data, clu="cluster", STR="stratum", n, sizevar="HH_count"){
#   data_in<-copy(data)
#   if(!is.data.table(data))
#     data_in<-data.table(data)
#
#   setkeyv(data_in, clu)
#   if(length(n)==1) n<-rep(n,length(unique(data_in[[STR]])))
#
#   #print(n)
#   old_class<-class(data_in[[STR]])
#   if(!is.factor(data_in[[STR]])) data_in[,(STR) :=as.factor(get(STR))]
#
#   setkeyv(data_in, STR)
#   data_in[,pik_str:=numeric(.N)]
#   loops<-levels(data_in[[STR]])
#   fullDT<-data_in[0]
#   k=1
#   for (s in loops){
#     tmp<-data_in[.(s)]
#     n0<-n[k]
#     int<-sum(tmp[[sizevar]])/n0
#     tmp[,pik_str:=n0*(tmp[[sizevar]]/sum(tmp[[sizevar]]))]
#
#     ##  Creating Random Order
#     tmp[sample(.N)]
#     ##  Cumulative sum
#     tmp[,cumul:=cumsum(tmp[[sizevar]])]
#     ##  Random start
#     rn<-runif(1, 0, int)
#     ##  Creat intervall groups and select firs per group
#     tmp[,int_group:=cut(cumul, seq(rn, (n0)*int, by=int), labels=FALSE)]
#     tmp<-tmp
#     tmp<-tmp[,.SD[1], by=int_group]
#     tmp<-tmp[,c("cumul", "int_group"):=NULL]
#     tmp<-setkeyv(tmp, clu)
#
#     fullDT<-rbind(fullDT, tmp)
#     k=k+1
#   }
#   return(fullDT)
# }
#
# ##########################################################################################################################
# ##    Non data.table sampling functions (FASTER!!!)
# ##########################################################################################################################
#
# # sampFunction_balance<-function(yFF = yFF, pik = pik, X = X, ss = ss) {
# #   finalsamp<-yFF[BalancedSampling::cube(pik, X)]
# #   pik<-rep.int(ss/length(finalsamp), length(finalsamp))
# #   pik1<-outer(pik, pik, "*")
# #   Mean=(sampling::HTestimator(y = finalsamp,
# #                               pik = pik)[1,1])/sampling::HTestimator(y = rep(1, length(finalsamp)),
# #                                                                      pik = pik)[1,1]
# #   SE = ((sampling::varHT(y = finalsamp, pikl = pik1)/(ss-1))^0.5)
# #   tab<-data.frame(Mean = Mean, SE = SE)
# #   tab$CV<-SE/Mean
# #   tab$bias<-(Mean-pop_mean)/pop_mean
# #   return(tab)
# # }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
