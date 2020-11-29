library(tidyverse)
library(data.table)


# read all csv files in a folder
do.call_rbind_fread <- function(path, pattern = "*.csv") {
  files = list.files(path, pattern, full.names = TRUE)
  do.call(rbind, lapply(files, function(x) fread(x, stringsAsFactors = FALSE)))
}

# define models
models <- c("UMass-MechBayesllonger_H","UMass-MechBayesllonger_H_df","UMass-MechBayesllonger_H_cdf")
# get mb_csv
mb_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/mech_bayes_paper/total/",models[1],"/"))

#align mb forecast date
mb_csv$forecast_date <- mb_csv$forecast_date + 1

# get bl csv
bl_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/mech_bayes_paper/total/",models[2],"/"))
bl_csv$forecast_date <- bl_csv$forecast_date + 1


#get bl 2

bl2_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/mech_bayes_paper/total/",models[3],"/"))

bl2_csv$forecast_date <- bl2_csv$forecast_date + 1

#define common dates
dates <-seq(as.Date("2020-05-11"),as.Date("2020-10-19"),by="week")


#subset mb and bl to common dates
mb_common_dates <- mb_csv[mb_csv$forecast_date %in% dates,]
bl_common_dates <- bl_csv[bl_csv$forecast_date %in% dates,]
bl2_common_dates <- bl2_csv[bl2_csv$forecast_date %in% dates,]

#subset mb and bl to quantile forecasts
mb_common_dates_quantile_only <- mb_common_dates[mb_common_dates$type == "quantile",]
bl_common_dates_quantile_only <- bl_common_dates[bl_common_dates$type == "quantile",]
bl2_common_dates_quantile_only <- bl2_common_dates[bl2_common_dates$type == "quantile",]

#subset mb and bl to states
mb_common_dates_quantile_only_states_only <- mb_common_dates_quantile_only[which(nchar(mb_common_dates_quantile_only$location) == 2)]
bl_common_dates_quantile_only_states_only <- bl_common_dates_quantile_only[which(nchar(bl_common_dates_quantile_only$location) == 2)]
bl2_common_dates_quantile_only_states_only <- bl2_common_dates_quantile_only[which(nchar(bl2_common_dates_quantile_only$location) == 2)]

# subset to inc death target

mb_common_dates_quantile_only_states_only_inc_death <- mb_common_dates_quantile_only_states_only[mb_common_dates_quantile_only_states_only$target %in% paste0(1:4, " wk ahead inc death"), ]
bl_common_dates_quantile_only_states_only_inc_death <- bl_common_dates_quantile_only_states_only[bl_common_dates_quantile_only_states_only$target %in% paste0(1:4, " wk ahead inc death"),]
bl2_common_dates_quantile_only_states_only_inc_death <- bl2_common_dates_quantile_only_states_only[bl2_common_dates_quantile_only_states_only$target %in% paste0(1:4, " wk ahead inc death"),]


#combine dataframes

joined_mb_and_bl <- mb_common_dates_quantile_only_states_only_inc_death %>% 
  left_join(bl_common_dates_quantile_only_states_only_inc_death,by=c("type", "forecast_date","target","target_end_date", "location","quantile")) %>%
  mutate(mb=value.x,bl=value.y) %>% select(-value.y) %>% select(-value.x)


# get truth 
truth <- read.csv("/Users/gcgibson/covid19-forecast-hub/data-truth/truth-Incident Deaths.csv")
#subset truth to states
truth_states <- truth[which(nchar(truth$location) == 2),]
# aggregate by week Sun-Sat
truth_states_by_week <- truth_states %>% group_by(week = epiweek(date),location) %>% summarise(value = sum(value),target_end_date = as.Date(tail(date,1)))
truth_states_by_week$target_end_date_str <- as.character(truth_states_by_week$target_end_date)
#join truth and forecasts
joined_mb_and_bl$target_end_date_str <- as.character(joined_mb_and_bl$target_end_date)

joined_mb_and_bl_truth <- joined_mb_and_bl %>% 
  left_join(truth_states_by_week,by=c("location","target_end_date_str")) %>% 
  select(-week) %>% select(-target_end_date.y) %>% mutate(target_end_date =target_end_date.x)

# subset to complete rows (with all truth dates present)

joined_mb_and_bl_truth_complete <- joined_mb_and_bl_truth[complete.cases(joined_mb_and_bl_truth),]

#calculate CP
joined_mb_and_bl_truth_complete_mb_wide <- joined_mb_and_bl_truth_complete %>% pivot_wider(names_from = "quantile",values_from=c("mb","bl"))
joined_mb_and_bl_truth_complete_mb_wide_cp <- joined_mb_and_bl_truth_complete_mb_wide %>% mutate(mb_cp_1 = ifelse(value <= mb_0.99 & value >= mb_0.01 ,1,0),
                                                                                                 mb_cp_5 = ifelse(value <= mb_0.975 & value >= mb_0.025 ,1,0),
                                                                                                 mb_cp_20 = ifelse(value <= mb_0.9 & value >= mb_0.1 ,1,0) ,
                                                                                                 mb_cp_50 = ifelse(value <= mb_0.75 & value >= mb_0.25 ,1,0) ,
                                                                                                 bl_cp_1 = ifelse(value <=bl_0.99 & value >= bl_0.01 ,1,0),
                                                                                                 bl_cp_5 = ifelse(value <= bl_0.975 & value >= bl_0.025 ,1,0),
                                                                                                 bl_cp_20 = ifelse(value <= bl_0.9 & value >= bl_0.1 ,1,0) ,
                                                                                                 bl_cp_50 = ifelse(value <= bl_0.75 & value >= bl_0.25 ,1,0))



cp_plot_df <- joined_mb_and_bl_truth_complete_mb_wide_cp %>% group_by(forecast_date) %>% 
  summarize(mb_cp_1=mean(mb_cp_1),mb_cp_5=mean(mb_cp_5),mb_cp_20=mean(mb_cp_20),mb_cp_50=mean(mb_cp_50),
            bl_cp_1=mean(bl_cp_1),bl_cp_5=mean(bl_cp_5),bl_cp_20=mean(bl_cp_20),bl_cp_50=mean(bl_cp_50))



### scatter plot of empirical coverage versus theoretical coverage

fig_9 <- ggplot(cp_plot_df,aes(x=forecast_date,y=mb_cp_5,col="MB 95%")) + geom_line()+
  geom_line(aes(x=forecast_date,y=bl_cp_5,col="BL 95%")) +
  geom_line(aes(x=forecast_date,y=mb_cp_20,col="MB 80%")) +
  geom_line(aes(x=forecast_date,y=bl_cp_20,col="BL 80%")) +
  theme_bw()  + ylab("Coverage Probability") + xlab("Forecast Date")


ggsave("fig_9.png",plot=fig_9,device="png",width=4,height=4)



cp_plot_df_long <- cp_plot_df %>% pivot_longer(cols=-forecast_date,names_to="model_cp",values_to="cp")
cp_plot_df_long$cp_level <- 1-2*as.numeric(substr(cp_plot_df_long$model_cp,7,7))/100
cp_plot_df_long$cp_level <- factor(cp_plot_df_long$cp_level)
cp_plot_df_long$cp_level <- recode(cp_plot_df_long$cp_level,"0.9"="0.8","0.96"="0.90","0.98"="0.95")
cp_plot_df_long$cp_model <- substr(cp_plot_df_long$model_cp,1,2)

fig_9 <- ggplot(cp_plot_df_long,aes(x=cp_level,cp,col=cp_model)) + geom_boxplot() +
  theme_bw()  + ylab("Coverage Probability") + xlab("Forecast Date")
fig_9
