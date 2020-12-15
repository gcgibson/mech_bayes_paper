library(tidyverse)
library(data.table)


# read all csv files in a folder
do.call_rbind_fread <- function(path, pattern = "*.csv") {
  files = list.files(path, pattern, full.names = TRUE)
  do.call(rbind, lapply(files, function(x) fread(x, stringsAsFactors = FALSE)))
}

# define models
models <- c("UMass-MechBayes","COVIDhub-baseline")
# get mb_csv
mb_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/covid19-forecast-hub/data-processed/",models[1],"/"))

#align mb forecast date
mb_csv$forecast_date <- mb_csv$forecast_date + 1

# get bl csv
bl_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/covid19-forecast-hub/data-processed/",models[2],"/"))

#define common dates
dates <-seq(as.Date("2020-05-11"),as.Date("2020-10-19"),by="week")


#subset mb and bl to common dates
mb_common_dates <- mb_csv[mb_csv$forecast_date %in% dates,]
bl_common_dates <- bl_csv[bl_csv$forecast_date %in% dates,]

#subset mb and bl to quantile forecasts
mb_common_dates_quantile_only <- mb_common_dates[mb_common_dates$type == "quantile",]
bl_common_dates_quantile_only <- bl_common_dates[bl_common_dates$type == "quantile",]

#subset mb and bl to states
mb_common_dates_quantile_only_states_only <- mb_common_dates_quantile_only[which(nchar(mb_common_dates_quantile_only$location) == 2)]
bl_common_dates_quantile_only_states_only <- bl_common_dates_quantile_only[which(nchar(bl_common_dates_quantile_only$location) == 2)]

# subset to inc death target

mb_common_dates_quantile_only_states_only_inc_death <- mb_common_dates_quantile_only_states_only[mb_common_dates_quantile_only_states_only$target %in% paste0(1:4, " wk ahead inc death"), ]
bl_common_dates_quantile_only_states_only_inc_death <- bl_common_dates_quantile_only_states_only[bl_common_dates_quantile_only_states_only$target %in% paste0(1:4, " wk ahead inc death"),]


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
  select(-week) %>% select(-target_end_date.y) %>% mutate(target_end_date =target_end_date.x) %>% select(-target_end_date.x)

# subset to complete rows (with all truth dates present)

joined_mb_and_bl_truth_complete <- joined_mb_and_bl_truth[complete.cases(joined_mb_and_bl_truth),]

#calculate CP
joined_mb_and_bl_truth_complete_mb_wide <- joined_mb_and_bl_truth_complete %>% pivot_wider(names_from = "quantile",values_from=c("mb","bl"))
joined_mb_and_bl_truth_complete_mb_wide_cp <- joined_mb_and_bl_truth_complete_mb_wide %>% mutate(mb_cp_95 = ifelse(value <= mb_0.975 & value >= mb_0.025 ,1,0),
                                                   mb_cp_80= ifelse(value <= mb_0.9 & value >= mb_0.1 ,1,0) ,
                                                   bl_cp_80= ifelse(value <= bl_0.9 & value >= bl_0.1 ,1,0),
                                                   mb_cp_50 = ifelse(value <= mb_0.75 & value >= mb_0.25 ,1,0) ,
                                                   bl_cp_95 = ifelse(value <= bl_0.975 & value >= bl_0.025 ,1,0),
                                                   bl_cp_20 = ifelse(value <= bl_0.9 & value >= bl_0.1 ,1,0) ,
                                                   bl_cp_50 = ifelse(value <= bl_0.75 & value >= bl_0.25 ,1,0),
                                                    mb_cp_70 = ifelse(value <= mb_0.85 & value >= mb_0.15 ,1,0),
                                                   bl_cp_70 = ifelse(value <= bl_0.85 & value >= bl_0.15 ,1,0),
                                                   mb_cp_60 = ifelse(value <= mb_0.8& value >= mb_0.2 ,1,0),
                                                   bl_cp_60 = ifelse(value <= bl_0.8 & value >= bl_0.2 ,1,0),
                                                   mb_cp_90 = ifelse(value <= mb_0.95& value >= mb_0.05 ,1,0),
                                                   bl_cp_90 = ifelse(value <= bl_0.95 & value >= bl_0.05 ,1,0),
                                                   mb_cp_98 = ifelse(value <= mb_0.99& value >= mb_0.01 ,1,0),
                                                   bl_cp_98 = ifelse(value <= bl_0.99 & value >= bl_0.01 ,1,0))



cp_plot_df <- joined_mb_and_bl_truth_complete_mb_wide_cp  %>% select(contains("_cp"))



### scatter plot of empirical coverage versus theoretical coverage


cp_plot_df_long <- cp_plot_df %>% pivot_longer(cols=colnames(cp_plot_df),names_to="model_cp",values_to="cp")
cp_plot_df_long$cp_model <- substr(cp_plot_df_long$model_cp,1,2)
cp_plot_df_long$cp_level <- as.numeric(substr(cp_plot_df_long$model_cp,7,8))/100
cp_plot_df_long <- cp_plot_df_long%>% group_by(cp_model,cp_level) %>% summarize(cp=mean(cp))
cp_plot_df_long$model <- as.factor(cp_plot_df_long$cp_model)
cp_plot_df_long$model <- cp_plot_df_long$model %>% recode("bl"="Baseline","mb"="MechBayes")
fig_9 <- ggplot(cp_plot_df_long[cp_plot_df_long$cp_level > .20,],aes(x=cp_level,y=cp,col=model,group=cp_model)) + geom_point() + geom_line()+
  geom_abline(slope=1,intercept=0,alpha=.5)+
  theme_bw()  + ylab("Empirical Coverage") +xlab("Theoretical Coverage")   +coord_cartesian(xlim=c(.5,1),ylim=c(.5,1))  + theme(legend.title=element_blank())

ggsave(filename = "fig_9.png",fig_9,width=6,height=4)

