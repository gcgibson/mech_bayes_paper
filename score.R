library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(lubridate)
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, "gcgibson", "casey!ili")
## read in fips
fips_csv <- read.csv(url("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv"))

## access scores for the COVID-19 Forecasts project
tmp <- scores(zoltar_connection, "https://www.zoltardata.com/api/project/44/") %>%

  mutate(wis = (abs_error+.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+.3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90)/12,
         mae=abs_error,error=error)

tmp <- tmp[tmp$target %in% paste0(1:4, " wk ahead inc death"),]
tmp_mech_bayes <- tmp[tmp$model %in%  c("UMass-MechBayes","COVIDhub-baseline"),]
tmp_mech_bayes$timezero <- as.Date(tmp_mech_bayes$timezero)
#tmp_mech_bayes <- tmp_mech_baye
tmp_subset <- tmp_mech_bayes %>%  dplyr::group_by(timezero,unit,model,target) %>% dplyr::summarize(mae=mean(mae),wis=mean(wis),error=mean(error))


#tmp_subset$timezero <- as.factor(tmp_subset$timezero)
tmp_subset <- tmp_subset[tmp_subset$timezero > "2020-04-20",]
tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero <- tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero -1
library(ggplot2)
mae_results_by_time_zero <- ggplot(tmp_subset[tmp_subset$timezero >= "2020-05-10",] %>% group_by(timezero,model) %>% summarize(mae=mean(mae)),aes(x=timezero,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) + 
  #annotate("text", x =as.Date("2020-04-15"), y = 150, label = "V1") + 
  annotate("text", x =as.Date("2020-05-12"), y = 150, label = "V1") +   
  annotate("text", x =as.Date("2020-07-01"), y = 150, label = "V2") + 
  #geom_ribbon(aes(xmin =as.Date("2020-04-05"),xmax=as.Date("2020-05-05") ),alpha=.1,fill='thistle1',colour='thistle1') +
  geom_ribbon(aes(xmin =as.Date("2020-05-05"),xmax=as.Date("2020-05-20") ),alpha=.1,fill='thistle3',colour='thistle3') +
  geom_ribbon(aes(xmin =as.Date("2020-05-20"),xmax=as.Date("2020-08-20") ),alpha=.1,fill='thistle4',colour='thistle4') + xlab("Date")
  


ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_time_zero.png",mae_results_by_time_zero,device="png",width=8,height=4)

fips_csv$unit <- fips_csv$location
tmp_subset_r <- tmp_subset %>% group_by(unit,model) %>% summarize(mae=mean(mae)) %>% left_join(fips_csv,by='unit')
tmp_subset_r <- tmp_subset_r[rev(order(tmp_subset_r$mae)),]
tmp_subset_r$abbreviation <- factor(tmp_subset_r$abbreviation,levels=unique(tmp_subset_r$abbreviation))
mae_results_by_region <- ggplot(tmp_subset_r,aes(x=abbreviation,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) + xlab("State") + ylab("MAE")

ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_region.png",mae_results_by_region,device="png",width=8,height=4)


#normalize by population
pops <- read.csv("/Users/gcgibson/Downloads/nst-est2019-alldata.csv")
pops$state <- pops$STATE
library(dplyr)
tmp_subset$state <- as.integer(tmp_subset$unit)
tmp_subset_w_pop <- tmp_subset %>% left_join(pops,by = "state")
mae_results_by_region_normalized <- ggplot(tmp_subset_w_pop %>% group_by(model) %>% summarize(mae=mean(mae), CENSUS2010POP=CENSUS2010POP[1]),aes(x=1,y=mae/CENSUS2010POP*1e6,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_region_normalized.png",mae_results_by_region_normalized,device="png",width=8,height=4)


mae_results_by_target <- ggplot(tmp_subset %>% group_by(target,model) %>% summarize(mae=mean(mae)),aes(x=target,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_target.png",mae_results_by_target,device="png",width=8,height=4)


wis_results_by_time_zero <- ggplot(tmp_subset[tmp_subset$timezero >= "2020-05-10",] %>% group_by(timezero,model) %>% summarize(wis=mean(wis)),aes(x=timezero,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) +
  #annotate("text", x =as.Date("2020-04-15"), y = 100, label = "V1") + 
  annotate("text", x =as.Date("2020-05-12"), y = 100, label = "V1") +   
  annotate("text", x =as.Date("2020-07-01"), y = 100, label = "V2") + 
  #geom_ribbon(aes(xmin =as.Date("2020-04-05"),xmax=as.Date("2020-05-05") ),alpha=.1,fill='thistle1',colour='thistle1') +
  geom_ribbon(aes(xmin =as.Date("2020-05-05"),xmax=as.Date("2020-05-20") ),alpha=.1,fill='thistle3',colour='thistle3') +
  geom_ribbon(aes(xmin =as.Date("2020-05-20"),xmax=as.Date("2020-08-20") ),alpha=.1,fill='thistle4',colour='thistle4') + xlab("Date")

ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_time_zero.png",wis_results_by_time_zero,device="png",width=8,height=4)


tmp_subset_rw <- tmp_subset %>% group_by(unit,model) %>% summarize(wis=mean(wis))%>% left_join(fips_csv,by='unit')
tmp_subset_rw <- tmp_subset_rw[rev(order(tmp_subset_rw$wis)),]
tmp_subset_rw$abbreviation <- factor(tmp_subset_rw$abbreviation,levels=unique(tmp_subset_rw$abbreviation))

wis_results_by_region <- ggplot(tmp_subset_rw,aes(x=abbreviation,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) + xlab("State")
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_region.png",wis_results_by_region,device="png",width=8,height=4)


wis_results_by_target <- ggplot(tmp_subset %>% group_by(target,model) %>% summarize(wis=mean(wis)),aes(x=target,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_target.png",wis_results_by_target,device="png",width=8,height=4)



error_results_by_time_zero <- ggplot(tmp_subset %>% group_by(unit,model) %>% summarize(error=mean(error)),aes(x=unit,y=error,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))

bias_by_timezero <- ggplot(tmp_subset %>% group_by(timezero,model) %>% summarize(error=mean(error)),aes(x=timezero,y=error,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/bias_by_timezero.png",bias_by_timezero,device="png",width=8,height=4)
