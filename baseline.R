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




baseline_forecasts <- covidHubUtils::load_forecasts(models = c("COVIDhub-baseline"),
                                              forecast_date = dates, 
                                              types = "point",locations=state_fips)

ua_forecasts <- covidHubUtils::load_forecasts(models = c("UA-EpiCovDA"),
                                                    forecast_date = dates_off_by_1, 
                                                    types = "point",locations=state_fips)

usac_forecasts <- covidHubUtils::load_forecasts(models = c("USACE-ERDC_SEIR"),
                                              forecast_date = dates, 
                                              types = "point",locations=state_fips)

ow_forecasts <- covidHubUtils::load_forecasts(models = c("OliverWyman-Navigator"),
                                                forecast_date = dates_off_by_1, 
                                                types = "point",locations=state_fips)

gt_forecasts <- covidHubUtils::load_forecasts(models = c("GT-DeepCOVID"),
                                              forecast_date = dates, 
                                              types = "point",locations=state_fips)
jhu_forecasts <- covidHubUtils::load_forecasts(models = c("JHU_IDD-CovidSP"),
                                              forecast_date = dates_off_by_1, 
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


nd_forecasts <- covidHubUtils::load_forecasts(models = c("NotreDame-mobility"),
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

total_forecasts <- rbind(mb_forecasts,baseline_forecasts,ua_forecasts,usac_forecasts,ow_forecasts,jhu_forecasts,lanl_forecasts,ucla_forecasts,cu_forecasts,nd_forecasts,mobs_forecasts,yyg_forecasts)


forecast_dates_df <- total_forecasts %>% group_by(model) %>% summarize(target_end_date = unique(target_end_date))#,region=unique(location),horizon=unique(horizon))


ggplot(forecast_dates_df,aes(x=target_end_date,y=model)) + geom_point() + theme_bw() + xlab("Date")

forecast_locations_df <- total_forecasts %>% group_by(model) %>% summarize(location = unique(location))#,region=unique(location),horizon=unique(horizon))

ggplot(forecast_locations_df,aes(x=location,y=model)) + geom_point() + theme_bw() + xlab("Location")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



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


# drop USACE
total_panel_a <- ggplot(total_forecasts_w_truth_point_complete_wide,aes(x=sort(`COVIDhub-baseline`),y=sort(`UMass-MechBayes`),col="COVIDhub-baseline")) + geom_point(size=.5,alpha=.5) + theme_bw() + geom_abline(slope=1) +
  geom_point(aes(x=sort(`LANL-GrowthRate`),y=sort(`UMass-MechBayes`),col="LANL-GrowthRate"),size=.5,alpha=.5)+
  geom_point(aes(x=sort(`UCLA-SuEIR`),y=sort(`UMass-MechBayes`),col="UCLA-SuEIR"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`YYG-ParamSearch`),y=sort(`UMass-MechBayes`),col="YYG-ParamSearch"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`CU-select`),y=sort(`UMass-MechBayes`),col="CU-select"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`UA-EpiCovDA`),y=sort(`UMass-MechBayes`),col="UA-EpiCovDA"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`OliverWyman-Navigator`),y=sort(`UMass-MechBayes`),col="OliverWyman-Navigator"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`JHU_IDD-CovidSP`),y=sort(`UMass-MechBayes`),col="JHU_IDD-CovidSP"),size=.5,alpha=.5) + 
  geom_point(aes(x=sort(`MOBS-GLEAM_COVID`),y=sort(`UMass-MechBayes`),col="MOBS-GLEAM_COVID"),size=.5,alpha=.5) + 
  
  coord_cartesian(ylim = c(0,3000),xlim=c(0,3000)) + ylab("MechBayes Quantile of AE") + xlab("Alternate Model Quantile of AE")  + theme(legend.title=element_blank()) 

total_panel_b <- ggplot(total_forecasts_w_truth_point_complete_wide,aes(x=mean(`COVIDhub-baseline`),y=mean(`UMass-MechBayes`),col="COVIDhub-baseline",shape="Mean")) + geom_point() +
  geom_point(aes(x=mean(`LANL-GrowthRate`),y=mean(`UMass-MechBayes`),col="LANL-GrowthRate",shape="Mean"))+
  geom_point(aes(x=mean(`UCLA-SuEIR`),y=mean(`UMass-MechBayes`),col="UCLA-SuEIR",shape="Mean")) + 
  geom_point(aes(x=mean(`YYG-ParamSearch`),y=mean(`UMass-MechBayes`),col="YYG-ParamSearch",shape="Mean")) + 
  geom_point(aes(x=mean(`CU-select`),y=mean(`UMass-MechBayes`),col="CU-select",shape="Mean")) + 
  geom_point(aes(x=mean(`UA-EpiCovDA`),y=mean(`UMass-MechBayes`),col="UA-EpiCovDA",shape="Mean")) + 
  geom_point(aes(x=mean(`OliverWyman-Navigator`),y=mean(`UMass-MechBayes`),col="OliverWyman-Navigator",shape="Mean")) + 
  geom_point(aes(x=mean(`JHU_IDD-CovidSP`),y=mean(`UMass-MechBayes`),col="JHU_IDD-CovidSP",shape="Mean")) + 
  geom_point(aes(x=mean(`MOBS-GLEAM_COVID`),y=mean(`UMass-MechBayes`),col="MOBS-GLEAM_COVID",shape="Mean")) +
  
  geom_point(aes(x=median(`LANL-GrowthRate`),y=median(`UMass-MechBayes`),col="LANL-GrowthRate",shape="Median"))+
  geom_point(aes(x=median(`UCLA-SuEIR`),y=median(`UMass-MechBayes`),col="UCLA-SuEIR",shape="Median")) + 
  geom_point(aes(x=median(`YYG-ParamSearch`),y=median(`UMass-MechBayes`),col="YYG-ParamSearch",shape="Median")) + 
  geom_point(aes(x=median(`CU-select`),y=median(`UMass-MechBayes`),col="CU-select",shape="Median")) + 
  geom_point(aes(x=median(`UA-EpiCovDA`),y=median(`UMass-MechBayes`),col="UA-EpiCovDA",shape="Median")) + 
  geom_point(aes(x=median(`OliverWyman-Navigator`),y=median(`UMass-MechBayes`),col="OliverWyman-Navigator",shape="Median")) + 
  geom_point(aes(x=median(`JHU_IDD-CovidSP`),y=median(`UMass-MechBayes`),col="JHU_IDD-CovidSP",shape="Median")) + 
  geom_point(aes(x=median(`MOBS-GLEAM_COVID`),y=median(`UMass-MechBayes`),col="MOBS-GLEAM_COVID",shape="Median")) +
  
  geom_point(aes(x=quantile(`LANL-GrowthRate`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="LANL-GrowthRate",shape="0.95 Quantile"))+
  geom_point(aes(x=quantile(`UCLA-SuEIR`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="UCLA-SuEIR",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`YYG-ParamSearch`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="YYG-ParamSearch",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`CU-select`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="CU-select",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`UA-EpiCovDA`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="UA-EpiCovDA",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`OliverWyman-Navigator`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="OliverWyman-Navigator",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`JHU_IDD-CovidSP`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="JHU_IDD-CovidSP",shape="0.95 Quantile")) + 
  geom_point(aes(x=quantile(`MOBS-GLEAM_COVID`,probs=.95),y=quantile(`UMass-MechBayes`,probs=.95),col="MOBS-GLEAM_COVID",shape="0.95 Quantile"))+
  coord_cartesian(ylim=c(0,200),xlim=c(0,200)) + 
  xlab("Alternate Model Quantile of AE") + geom_abline(slope=1) + theme_bw()   + theme(legend.title=element_blank()) 
  
  

fig_5_total_alt <- cowplot::plot_grid(figure_5 + theme(legend.position="none") + ylab("") +xlab(""),
                                      figure_5_b + theme(legend.position="none")+   ylab("") +xlab(""),
                                      total_panel_a + theme(legend.position="none")+ ylab("") +xlab(""),
                                      total_panel_b + theme(legend.position="none")+ ylab("") +xlab(""),
                                      nrow=2,align="v",rel_heights = c(1,1.2),labels=c("A","B","C","D"))

ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","fig_5_total_alt.tiff"),fig_5_total_alt,device="tiff",width = 6,height=6)

library(cowplot)


total_forecasts_w_truth_point_complete_wide_w_names <- total_forecasts_w_truth_point_complete_wide %>% left_join(fips,by="location")
total_forecasts_w_truth_point_complete_wide_w_names$target <- paste0(total_forecasts_w_truth_point_complete_wide_w_names$horizon," week ahead")
eval_points <- ggplot(total_forecasts_w_truth_point_complete_wide_w_names,aes(y=abbreviation,x=target_end_date)) + geom_point() + facet_wrap(~target,nrow=1) + theme_bw() + ylab("Region") + xlab("Date")#+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("/Users/gcgibson/mech_bayes_paper/eval_points.png",eval_points,device = "png",width=8,height=10)
