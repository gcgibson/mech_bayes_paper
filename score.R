library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(lubridate)
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, "gcgibson", "casey!ili")
## read in fips

## access scores for the COVID-19 Forecasts project
tmp <- scores(zoltar_connection, "https://www.zoltardata.com/api/project/44/") %>%

  mutate(wis = (abs_error+.01*interval_2+.025*interval_5+.05*interval_10+.1*interval_20+.15*interval_30+.2*interval_40+.25*interval_50+.3*interval_60+.35*interval_70+.40*interval_80+.45*interval_90)/12,
         mae=abs_error)
tmp_mech_bayes <- tmp[tmp$model %in%  c("UMass-MechBayes","COVIDhub-baseline"),]
tmp_mech_bayes$timezero <- as.Date(tmp_mech_bayes$timezero)
#tmp_mech_bayes <- tmp_mech_baye
tmp_subset <- tmp_mech_bayes %>%  dplyr::group_by(timezero,unit,model,target) %>% dplyr::summarize(mae=mean(mae),wis=mean(wis))


#tmp_subset$timezero <- as.factor(tmp_subset$timezero)
tmp_subset <- tmp_subset[tmp_subset$timezero > "2020-04-20",]
tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero <- tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero -1
library(ggplot2)
mae_results_by_time_zero <- ggplot(tmp_subset %>% group_by(timezero,model) %>% summarize(mae=mean(mae)),aes(x=timezero,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_time_zero.png",mae_results_by_time_zero,device="png",width=8,height=4)


mae_results_by_region <- ggplot(tmp_subset %>% group_by(unit,model) %>% summarize(mae=mean(mae)),aes(x=unit,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_region.png",mae_results_by_region,device="png",width=8,height=4)

mae_results_by_target <- ggplot(tmp_subset %>% group_by(target,model) %>% summarize(mae=mean(mae)),aes(x=target,y=mae,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_target.png",mae_results_by_target,device="png",width=8,height=4)


wis_results_by_time_zero <- ggplot(tmp_subset %>% group_by(timezero,model) %>% summarize(wis=mean(wis)),aes(x=timezero,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_time_zero.png",wis_results_by_time_zero,device="png",width=8,height=4)


wis_results_by_region <- ggplot(tmp_subset %>% group_by(unit,model) %>% summarize(wis=mean(wis)),aes(x=unit,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_region.png",mae_results_by_region,device="png",width=8,height=4)


wis_results_by_target <- ggplot(tmp_subset %>% group_by(target,model) %>% summarize(wis=mean(wis)),aes(x=target,y=wis,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_target.png",wis_results_by_target,device="png",width=8,height=4)


