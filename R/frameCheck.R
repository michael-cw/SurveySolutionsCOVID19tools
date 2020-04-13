#' CHECK EA size
#'
#'  VERSION 2.0.0
#'
#'
#' @param data frame dataset
#' @param STRATUM stratum identifier
#' @param PSU psu identifier
#' @param n_hh minium psu size
#' @param COUNT varialbe containing population count
#'
#' @importFrom stats na.omit
#'
frameCheck<-function(data=NULL, STRATUM = NULL, PSU="EACODE2", n_hh=12, COUNT="count"){
  frame_EA<-copy(data)
  print("ok")
  if(!is.data.table(frame_EA)) frame_EA<-as.data.table(frame_EA)

  ###############################################################
  ##      Requires a STRATA
  ##  1. copy the specified variables
  frame_EA<-frame_EA[, HHcount2:=get(COUNT)]
  frame_EA<-frame_EA[, psu2:=get(PSU)]
  frame_EA<-frame_EA[, stratum2:=get(STRATUM)]
  ##  2. create a rolling sum
  frame_EA <- frame_EA[, .(psu2=psu2,HHcount2 =HHcount2,
                           roll = cumsum(HHcount2)), by = stratum2]
  ##  3. group over the rolling sum
  rollSeq<-seq(from=-1, to=max(frame_EA[["roll"]], na.rm = T), by=n_hh)
  frame_EA[,newGroup:=cut(roll, c(-Inf, rollSeq, +Inf))]
  ##  4. create new design variables and delete helpers
  frame_EA[,newPSU:=.SD[1, psu2], by=.(newGroup)]
  frame_EA[,newGroup:=NULL]
  frame_EA[,roll:=NULL]
  frame_EA<-frame_EA[complete.cases(frame_EA)]
  outFrame<-copy(frame_EA)
  outFrame[,newCount:=sum(HHcount2), by=.(newPSU)][]
  names(outFrame)<-c(STRATUM, PSU, COUNT, "psu2", "HHcount2")
  return(outFrame)
}

frameCheck2<-function(data=NULL, STRATUM = NULL, PSU="EACODE2", n_hh=12, COUNT="count"){
  frame_EA<-copy(data)
  print("ok")
  if(!is.data.table(frame_EA)) frame_EA<-as.data.table(frame_EA)

  ###############################################################
  ##      MULTIPLE STRATA
  if(!is.null(STRATUM)){
    frame_EA<-frame_EA[, HHcount2:=get(COUNT)]
    frame_EA<-frame_EA[, psu2:=get(PSU)]
    frame_EA<-frame_EA[, stratum2:=get(STRATUM)]
    setkeyv(frame_EA, c("stratum2", "psu2"))
    i<-1
    while(min(frame_EA[["HHcount2"]], na.rm = T)<n_hh) {
      #frame_EA<-frame_EA[order(c(stratum, psu2))]
      if(i==1) {
        print("ok2")
        print(min(frame_EA[["HHcount2"]], na.rm = T))
        frame_EA<-frame_EA[ , c("psu2") := list(ifelse(HHcount2<n_hh,
                                                       ifelse(data.table::shift(stratum2, 1L, type="lag")==stratum2 & .I!=1,
                                                              data.table::shift(psu2, 1L, type="lag"), data.table::shift(psu2, 1L, type="lead")), psu2))]
        ##################################
        ## AGGREGATION:
        ##    1. Get the sum of ORIGINAL HH count by the new PSU
        ##    2. Replace it if new HHcount2 < sample size
        ##    3. Delete the temporary
        ##    4. Replace new HHcount2 by group maximum

        frame_EA[,tmp:=sum(na.omit(get(COUNT))) ,by=psu2][HHcount2<n_hh, HHcount2:=tmp][,tmp:=NULL][,HHcount2:=max(HHcount2), by=psu2]
        i=i+1

      } else {
        print("ok3")
        print(min(frame_EA[["HHcount2"]], na.rm = T))
        frame_EA[ , c("psu2") := list(ifelse(HHcount2<n_hh,
                                             ifelse(data.table::shift(stratum2, 1L, type="lag")==stratum2 & .I!=1,
                                                    data.table::shift(psu2, 1L, type="lag"), data.table::shift(psu2, 1L, type="lead")), psu2))]
        frame_EA[,tmp:=sum(na.omit(get(COUNT))) ,by=psu2][HHcount2<n_hh, HHcount2:=tmp][,tmp:=NULL][,HHcount2:=max(HHcount2), by=psu2]

        i=i+1
      }
      print(i)
      if(i==200) break
    }
  } else {
    ###############################################################
    ##      SINGLE STRATUM
    ##        - create stratum, delete at the end
    frame_EA<-frame_EA[,stratum:=rep(1,.N)]
    frame_EA<-frame_EA[, HHcount2:=get(COUNT)]
    frame_EA<-frame_EA[, psu2:=get(PSU)]
    setkeyv(frame_EA, c("stratum", "psu2"))
    i<-1
    while(min(frame_EA[["HHcount2"]], na.rm = T)<n_hh) {
      #frame_EA<-frame_EA[order(c(stratum, psu2))]
      if(i==1) {
        frame_EA<-frame_EA[ , c("psu2") := list(ifelse(HHcount2<n_hh,
                                                       ifelse(data.table::shift(stratum, 1L, type="lag")==stratum & .I!=1,
                                                              data.table::shift(psu2, 1L, type="lag"), data.table::shift(psu2, 1L, type="lead")), psu2))]
        ##################################
        ## AGGREGATION:
        ##    1. Get the sum of ORIGINAL HH count by the new PSU
        ##    2. Replace it if new HHcount2 < sample size
        ##    3. Delete the temporary
        ##    4. Replace new HHcount2 by group maximum

        frame_EA[,tmp:=sum(na.omit(get(COUNT))) ,by=psu2][HHcount2<n_hh, HHcount2:=tmp][,tmp:=NULL][,HHcount2:=max(HHcount2), by=psu2]
        i=i+1

      } else {
        frame_EA[ , c("psu2") := list(ifelse(HHcount2<n_hh,
                                             ifelse(data.table::shift(stratum, 1L, type="lag")==stratum & .I!=1,
                                                    data.table::shift(psu2, 1L, type="lag"), data.table::shift(psu2, 1L, type="lead")), psu2))]
        frame_EA[,tmp:=sum(na.omit(get(COUNT))) ,by=psu2][HHcount2<n_hh, HHcount2:=tmp][,tmp:=NULL][,HHcount2:=max(HHcount2), by=psu2]

        i=i+1
      }
      if(i==500) break
    }
  }
  frame_EA<-frame_EA[complete.cases(frame_EA)][]
  return(frame_EA)


}
