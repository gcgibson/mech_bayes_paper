library(magrittr)

obs <- covidHubUtils::load_truth("JHU", "inc death") %>%
  dplyr::rename(true_value = value) %>%
  dplyr::select(-model)
dates <- seq(as.Date("2020-05-11"),as.Date("2020-10-19"),by="week")
dates_off_by_1  <- seq(as.Date("2020-05-11"),as.Date("2020-10-19"),by="week") - 1

state_obs <- obs[which(nchar(obs$location)==2),]
state_fips <- obs$location[which(nchar(obs$location)==2)]
state_fips <- setdiff(state_fips,c("US","78","69","66"))

mb_forecasts <- covidHubUtils::load_forecasts(models = c("UMass-MechBayes"),
                                           forecast_date = dates-1, 
                                           types = "point",locations=state_fips)


mb_forecasts$model


baseline_forecasts <- covidHubUtils::load_forecasts(models = c("COVIDhub-baseline"),
                                              forecast_date = dates, 
                                              types = "point",locations=state_fips)

lanl_forecasts <- covidHubUtils::load_forecasts(models = c("LANL-GrowthRate"),
                                                    forecast_date = dates, 
                                                    types = "point",locations=state_fips)

ucla_forecasts <- covidHubUtils::load_forecasts(models = c("UCLA-SuEIR"),
                                                forecast_date = dates_off_by_1, 
                                                types = "point",locations=state_fips)

cu_forecasts <- covidHubUtils::load_forecasts(models = c("CU-select"),
                                                forecast_date = dates_off_by_1, 
                                                types = "point",locations=state_fips)


ut_forecasts <- covidHubUtils::load_forecasts(models = c("UT-Mobility"),
                                              forecast_date = dates, 
                                              types = "point",locations=state_fips)


mobs_forecasts <- covidHubUtils::load_forecasts(models = c("MOBS-GLEAM_COVID"),
                                                                forecast_date = dates, 
                                                                types = "point",locations=state_fips)

ihme_forecasts <- covidHubUtils::load_forecasts(models = c("IHME-CurveFit"),
                                                forecast_date = dates, 
                                                types = "point",locations=state_fips)

yyg_forecasts <- covidHubUtils::load_forecasts(models = c("YYG-ParamSearch"),
                                                forecast_date = dates, 
                                                types = "point",locations=state_fips)
total_forecasts <- rbind(mb_forecasts,baseline_forecasts,lanl_forecasts,cu_forecasts,ut_forecasts,mobs_forecasts,cu_forecasts,ucla_forecasts,ihme_forecasts,yyg_forecasts)

library(dplyr)
total_forecasts_w_truth <- total_forecasts %>% left_join(state_obs,by=c("target_end_date","location"))
View(total_forecasts_w_truth)

total_forecasts_w_truth_point <- total_forecasts_w_truth[total_forecasts_w_truth$type == "point",]

total_forecasts_w_truth_point$ae <- abs(total_forecasts_w_truth_point$value-total_forecasts_w_truth_point$true_value)
tmp <- total_forecasts_w_truth_point %>% select(-quantile) 
total_forecasts_w_truth_point_complete <-  tmp[complete.cases(tmp),]

library(tidyverse)

total_forecasts_w_truth_point_complete_wide_1 <- total_forecasts_w_truth_point_complete[total_forecasts_w_truth_point_complete$target_variable.x == "inc death" & total_forecasts_w_truth_point_complete$target_variable.y == "inc death",] %>% select(-forecast_date) %>% select(-value)  %>% select(-true_value) 
total_forecasts_w_truth_point_complete_wide_1 <- total_forecasts_w_truth_point_complete_wide_1 %>% select(-target_variable.x) %>% select(-target_variable.y)
total_forecasts_w_truth_point_complete_wide_1 <- total_forecasts_w_truth_point_complete_wide_1[total_forecasts_w_truth_point_complete_wide_1$temporal_resolution == "wk",]
total_forecasts_w_truth_point_complete_wide_1 <- total_forecasts_w_truth_point_complete_wide_1 %>% select(-temporal_resolution)


tmp_fk <- total_forecasts_w_truth_point_complete_wide_1 %>% pivot_wider(names_from=model,values_from=ae,values_fn=mean)
total_forecasts_w_truth_point_complete_wide <- tmp_fk[complete.cases(tmp_fk),]



total_panel_a <- ggplot(total_forecasts_w_truth_point_complete_wide,aes(x=sort(`COVIDhub-baseline`),y=sort(`UMass-MechBayes`),col="COVIDhub-baseline")) + geom_point(size=.5) + theme_bw() + geom_abline(slope=1) +
  geom_point(aes(x=sort(`LANL-GrowthRate`),y=sort(`UMass-MechBayes`),col="LANL-GrowthRate"),size=.5)+
  geom_point(aes(x=sort(`UT-Mobility`),y=sort(`UMass-MechBayes`),col="UT-Mobility"),size=.5) + 
  geom_point(aes(x=sort(`IHME-CurveFit`),y=sort(`UMass-MechBayes`),col="IHME-CurveFit"),size=.5) + 
  geom_point(aes(x=sort(`UCLA-SuEIR`),y=sort(`UMass-MechBayes`),col="UCLA-SuEIR"),size=.5) + 
  geom_point(aes(x=sort(`YYG-ParamSearch`),y=sort(`UMass-MechBayes`),col="YYG-ParamSearch"),size=.5) + 
  geom_point(aes(x=sort(`CU-select`),y=sort(`UMass-MechBayes`),col="CU-select"),size=.5) + 
  
  coord_cartesian(ylim = c(0,3000),xlim=c(0,3000)) + ylab("MechBayes AE") + xlab("") + theme(legend.title=element_blank()) 

total_panel_b <- ggplot(total_forecasts_w_truth_point_complete_wide,aes(x=mean(`COVIDhub-baseline`),y=mean(`UMass-MechBayes`),col="COVIDhub-baseline",shape="Mean")) + geom_point() +
   geom_point(aes(x=mean(`LANL-GrowthRate`),y=mean(`UMass-MechBayes`),col="LANL-GrowthRate",shape="Mean")) +
  geom_point(aes(x=mean(`UT-Mobility`),y=mean(`UMass-MechBayes`),col="UT-Mobility",shape="Mean")) +
  geom_point(aes(x=mean(`IHME-CurveFit`),y=mean(`UMass-MechBayes`),col="IHME-CurveFit",shape="Mean")) +
  geom_point(aes(x=mean(`UCLA-SuEIR`),y=mean(`UMass-MechBayes`),col="UCLA-SuEIR",shape="Mean")) +
  geom_point(aes(x=mean(`YYG-ParamSearch`),y=mean(`UMass-MechBayes`),col="YYG-ParamSearch",shape="Mean")) +
  geom_point(aes(x=median(`COVIDhub-baseline`),y=median(`UMass-MechBayes`),col="COVIDhub-baseline",shape="Median")) +
  geom_point(aes(x=median(`LANL-GrowthRate`),y=median(`UMass-MechBayes`),col="LANL-GrowthRate",shape="Median")) +
  geom_point(aes(x=median(`UT-Mobility`),y=median(`UMass-MechBayes`),col="UT-Mobility",shape="Median")) +
  geom_point(aes(x=median(`IHME-CurveFit`),y=median(`UMass-MechBayes`),col="IHME-CurveFit",shape="Median")) +
  geom_point(aes(x=median(`UCLA-SuEIR`),y=median(`UMass-MechBayes`),col="UCLA-SuEIR",shape="Median")) +
  geom_point(aes(x=median(`CU-select`),y=median(`UMass-MechBayes`),col="CU-select",shape="Median"))   + geom_abline(slope=1) + theme_bw() + theme(legend.title=element_blank()) + ylab("") + xlab("Alternative AE")+
  geom_point(aes(x=median(`YYG-ParamSearch`),y=median(`UMass-MechBayes`),col="YYG-ParamSearch",shape="Median")) +
    geom_point(aes(x=quantile(`COVIDhub-baseline`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="COVIDhub-baseline",shape="95%")) +
    geom_point(aes(x=quantile(`LANL-GrowthRate`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="LANL-GrowthRate",shape="95%")) +
    geom_point(aes(x=quantile(`UT-Mobility`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="UT-Mobility",shape="95%")) +
    geom_point(aes(x=quantile(`IHME-CurveFit`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="IHME-CurveFit",shape="95%")) +
    geom_point(aes(x=quantile(`UCLA-SuEIR`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="UCLA-SuEIR",shape="95%")) +
    geom_point(aes(x=quantile(`CU-select`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="CU-select",shape="95%")) +
  geom_point(aes(x=quantile(`YYG-ParamSearch`,probs=c(.95)),y=quantile(`UMass-MechBayes`,probs=c(.95)),col="YYG-ParamSearch",shape="95%")) +
  
    coord_cartesian(ylim=c(0,200),xlim=c(0,200))
  
  


library(cowplot)

cowplot::plot_grid(total_panel_a,total_panel_b,nrow=2,align = "v")



ggplot(total_forecasts_w_truth_point_complete_wide,aes(x=location,y=target_end_date)) + geom_point() +facet_wrap(~model)
