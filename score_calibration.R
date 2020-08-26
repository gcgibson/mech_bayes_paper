library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(lubridate)
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, "gcgibson", "casey!ili")
## read in fips
fips_csv <- read.csv(url("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv"))

## access scores for the COVID-19 Forecasts project
#tmp <-  download_forecast(zoltar_connection, "https://www.zoltardata.com/api/model//")   
tmp <- do_zoltar_query(zoltar_connection, "https://www.zoltardata.com/api/project/44/", c("MechBayes"))

tmp <- tmp[tmp$target %in% paste0(1:4, " wk ahead inc death"),]
tmp_mech_bayes <- tmp[tmp$model %in%  c("UMass-MechBayes","COVIDhub-baseline"),]
tmp_mech_bayes$timezero <- as.Date(tmp_mech_bayes$timezero)
#tmp_mech_bayes <- tmp_mech_baye
tmp_subset <- tmp_mech_bayes %>%  dplyr::group_by(timezero,unit,model,target,truth) %>% dplyr::summarize(mae=mean(mae),wis=mean(wis),error=mean(error))
states_ <- unique(tmp_subset[tmp_subset$model == "UMass-MechBayes",]$unit) 
states_ <- states_[states_!="US" & states_ != "60" & states_ != "66"&states_ != "69" & states_ != "78" & states_ != "72"]
tmp_subset <- tmp_subset[tmp_subset$unit %in% states_,]

#tmp_subset$timezero <- as.factor(tmp_subset$timezero)
tmp_subset <- tmp_subset[tmp_subset$timezero > "2020-05-12",]
tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero <- tmp_subset[tmp_subset$model == "COVIDhub-baseline",]$timezero -1
library(ggplot2)
library(tidyr)

mae_results_by_time_zero_df <- tmp_subset[tmp_subset$timezero >= "2020-05-12" & tmp_subset$timezero <= "2020-08-02",] %>% group_by(timezero,model,unit,target) %>% summarize(mae=mean(mae))


mae_results_by_time_zero_df$timezero <- as.factor(mae_results_by_time_zero_df$timezero)
mae_results_by_time_zero <- ggplot(mae_results_by_time_zero_df,aes(x=timezero,y=log(.01+mae),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) 
mae_results_by_time_zero <- mae_results_by_time_zero + ylab("log(MAE)")

ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_time_zero_inc.png",mae_results_by_time_zero,device="png",width=7,height=4)

fips_csv$unit <- fips_csv$location
tmp_subset_r <- tmp_subset %>% group_by(unit,model,timezero,truth) %>% summarize(mae=mean(mae)) %>% left_join(fips_csv,by='unit')

#tmp_subset_r <- tmp_subset_r %>% spread(model,mae)
#tmp_subset_r$rmae <- tmp_subset_r$`UMass-MechBayes`/tmp_subset_r$`COVIDhub-baseline`
tmp_subset_r <- tmp_subset_r[rev(order(tmp_subset_r$truth)),]
tmp_subset_r$abbreviation <- factor(tmp_subset_r$abbreviation,levels=unique(tmp_subset_r$abbreviation))
regions_to_show <- levels(tmp_subset_r$abbreviation)[1:20]
mae_results_by_region <- ggplot(tmp_subset_r[tmp_subset_r$abbreviation %in% regions_to_show,],aes(x=abbreviation,y=log(.01+mae),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) + xlab("State") 
mae_results_by_region <- mae_results_by_region + ylab("log(MAE)")

ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_region_inc.png",mae_results_by_region,device="png",width=7,height=4)


#normalize by population
pops <- read.csv("/Users/gcgibson/Downloads/nst-est2019-alldata.csv")
pops$state <- pops$STATE
library(dplyr)
tmp_subset$state <- as.integer(tmp_subset$unit)
tmp_subset_w_pop <- tmp_subset %>% left_join(pops,by = "state")
mae_results_by_region_normalized <- ggplot(tmp_subset_w_pop %>% group_by(model) %>% summarize(mae=mean(mae), CENSUS2010POP=CENSUS2010POP[1]),aes(x=1,y=mae/CENSUS2010POP*1e6,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_region_normalized.png",mae_results_by_region_normalized,device="png",width=8,height=4)
tmp_pop_df <- tmp_subset_w_pop %>% group_by(model) %>% summarize(mae=max(mae), CENSUS2010POP=CENSUS2010POP[1])
tmp_pop_df$mae/tmp_pop_df$CENSUS2010POP*100000

target_df <- tmp_subset %>% group_by(target,model,timezero) %>% summarize(mae=mean(mae))
#target_df <- target_df%>% spread(model,mae)
#target_df$rmae <- target_df$`UMass-MechBayes`/target_df$`COVIDhub-baseline`

mae_results_by_target <- ggplot(target_df,aes(x=target,y=log(.01+mae),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
mae_results_by_target <- mae_results_by_target + ylab("log(MAE)")
ggsave("/Users/gcgibson/mech_bayes_paper/mae_results_by_target_inc.png",mae_results_by_target,device="png",width=7,height=4)

wis_results_by_time_zero_df <- tmp_subset[tmp_subset$timezero >= "2020-05-10",] %>% group_by(timezero,model,unit) %>% summarize(wis=mean(wis))
#wis_results_by_time_zero_df <- wis_results_by_time_zero_df %>% spread(model,wis)
#wis_results_by_time_zero_df$rwis <- wis_results_by_time_zero_df$`UMass-MechBayes`/wis_results_by_time_zero_df$`COVIDhub-baseline`
wis_results_by_time_zero <- ggplot(wis_results_by_time_zero_df,aes(x=as.factor(timezero),y=log(.01+wis),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) 
wis_results_by_time_zero <-wis_results_by_time_zero + ylab("log(WIS)")
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_time_zero_inc.png",wis_results_by_time_zero,device="png",width=7,height=4)


tmp_subset_rw <- tmp_subset %>% group_by(unit,model, timezero,truth) %>% summarize(wis=mean(wis))%>% left_join(fips_csv,by='unit')
tmp_subset_rw <- tmp_subset_rw[rev(order(tmp_subset_rw$wis)),]
tmp_subset_rw$abbreviation <- factor(tmp_subset_rw$abbreviation,levels=unique(tmp_subset_rw$abbreviation))
#tmp_subset_rw <- tmp_subset_rw %>% spread(model,wis)
#tmp_subset_rw$rwis <- tmp_subset_rw$`UMass-MechBayes`/tmp_subset_rw$`COVIDhub-baseline`
tmp_subset_rw <- tmp_subset_rw[rev(order(tmp_subset_rw$truth)),]
tmp_subset_rw$abbreviation <- factor(tmp_subset_rw$abbreviation,levels=unique(tmp_subset_rw$abbreviation))

wis_results_by_region <- ggplot(tmp_subset_rw[tmp_subset_rw$abbreviation %in% regions_to_show,],aes(x=abbreviation,y=log(.01+wis),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90)) + xlab("State")
wis_results_by_region <- wis_results_by_region + ylab("log(WIS)")
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_region_inc.png",wis_results_by_region,device="png",width=7,height=4)

wis_results_by_target_df <- tmp_subset %>% group_by(target,model,unit,timezero) %>% summarize(wis=mean(wis))
#wis_results_by_target_df <- wis_results_by_target_df %>% spread(model,wis)
#wis_results_by_target_df$rwis <- wis_results_by_target_df$`UMass-MechBayes`/wis_results_by_target_df$`COVIDhub-baseline`
wis_results_by_target <- ggplot(wis_results_by_target_df,aes(x=target,y=log(wis),col=model)) + geom_boxplot() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
wis_results_by_target <- wis_results_by_target + ylab("log(WIS)")
ggsave("/Users/gcgibson/mech_bayes_paper/wis_results_by_target_inc.png",wis_results_by_target,device="png",width=7,height=4)



error_results_by_time_zero <- ggplot(tmp_subset %>% group_by(unit,model) %>% summarize(error=mean(error)),aes(x=unit,y=error,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))

bias_by_timezero <- ggplot(tmp_subset %>% group_by(timezero,model) %>% summarize(error=mean(error)),aes(x=timezero,y=error,col=model)) + geom_point() + theme_bw() +  theme(axis.text.x = element_text(angle = 90))
ggsave("/Users/gcgibson/mech_bayes_paper/bias_by_timezero.png",bias_by_timezero,device="png",width=7,height=4)
