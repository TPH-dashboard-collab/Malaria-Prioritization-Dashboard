df<-read_csv("TZ_subset_10regions_1seed.csv")

##EIR mean, we need one single value for impact metric (averaging, cumsum, filter on year)
df%>%
  filter(EIR_CI=="EIR_mean")%>%
  arrange(year)%>%
  mutate(nUncompCum=cumsum(nUncomp))%>%
  filter(year==2030)%>%
  rename_with(~ str_remove(., "active_int_"), starts_with("active_int_"))->df1


counterfactual_lgc<-function(x){
  res <- lapply(which(x), function(i) {
    y <- x
    y[i] <- FALSE
    names(y) <- names(x)
    y
  })
  return(res)
}

filter_lgc<-function(df,
                     target,
                     interv=c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                              "STD_Nets","PBO_Nets","IG2_Nets",
                              "SMC","PMC")){
  subdf <- df[, .SD, .SDcols = interv]
  out<- df[rowSums(subdf == matrix(target, nrow(df), length(target), byrow = TRUE)) == length(target)]
  return(out)
}

per_interv_impact<-function(df,
                            strata=c(admin_2="Bunda District Council",
                                     age_group="0-100",
                                     scenario_name="CM_IG2_Nets_IRS_PMC",
                                     plan="NSP"),##simulation for which per intervention impact should be calculated, order of first three names is important
                            indicator=c("nUncompCum"),
                            interv=c("LSM","IRS","IPTSc","Vaccine","CM","ICCM",
                                     "STD_Nets","PBO_Nets","IG2_Nets",
                                     "SMC","PMC"),
                            keep_col=NULL){
  #strata names for joining, bust be first three in that order
  strat_join<-names(strata)[1:3]
  ##get intervention logicals for strata scenario
  strata<-as.data.table(t(strata))
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  target<-as.logical(df[strata][,..interv])
  names(target)<-interv
  
  ##prepare counterfactual data needed for calculation
  counterfactual_cols<-c(strat_join,indicator,interv)
  if(!is.null(keep_col)){counterfactual_cols<-c(counterfactual_cols,keep_col)}
  lapply(counterfactual_lgc(target),
         function(x) {
           setkeyv(df, strat_join[1:2])
           setkeyv(strata, strat_join[1:2])
           filter_lgc(df[strata][,..counterfactual_cols],x)}
  )->counterfactual_df
  counterfactual_df <- counterfactual_df[lapply(counterfactual_df, nrow) > 0]
  
  ##calculate difference between indicators in strata and counterfactuals
  setkeyv(df, strat_join)
  setkeyv(strata, strat_join)
  strata_df<-df[strata][,..counterfactual_cols]
  
  lapply(counterfactual_df,
         function(x) {
           setkeyv(x, strat_join[1:2])#join on district an age
           setkeyv(strata, strat_join[1:2])#join on district an age
           foo<-x[strata_df]#join on district an age
           for (k in indicator){
             foo[,paste0("IMPACT_",k):=get(k)-get(paste0("i.",k))]
           }
           foo_cols <- c(strat_join[strat_join!="scenario_name"],"i.scenario_name",paste0("IMPACT_",indicator))
           if(!is.null(keep_col)){foo_cols<-c(foo_cols,keep_col)}
           melt(
             foo[, ..foo_cols],
             id.vars =  setdiff(foo_cols,indicator),
             measure.vars = paste0("IMPACT_",indicator),
             variable.name = "metric",
             value.name = "value"
           )->foo
           return(foo)
         }
  )->counterfactual_df
  
  lapply(names(counterfactual_df),
         function(x) {
           foo <- counterfactual_df[[x]]
           foo <- foo[,intervention:=x]
           setnames(foo, 
                    old = c("i.scenario_name"), 
                    new = c("scenario_name"))
           return(foo)
         }
  )->result
  
  result<-rbindlist(result)
  result<-result[,(paste0("IMPACT_",indicator)):=NULL]
  
  return(result)
}

df1<-as.data.table(df1)
##testing for a single district
test<-per_interv_impact(df1)

##testing for several districts
df1%>%
  filter(plan=="NSP")%>%
  select(admin_2,age_group,scenario_name,plan)->strata1##order of names here important!

rbindlist(
  lapply(split(strata1, seq_len(nrow(strata1))),
         function(x){
           per_interv_impact(df1,
                             indicator = c("cum_nUncomp"),
                             strata = c(admin_2=x$admin_2,age_group=x$age_group,scenario_name=x$scenario_name,plan=x$plan),
                             keep_col = NULL)
         }
  )
)->per_interv_impact_strata1

##start ranking and quantiles on impact values per intervention
per_interv_impact_strata1%>%
  filter(intervention=="IRS"&age_group=="0-100")

