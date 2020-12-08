library(tidyverse)
library(data.table)
library(lubridate)

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

#define common dates
dates <-seq(as.Date("2020-05-11"),as.Date("2020-10-18"),by="week")


#subset mb and bl to common dates
mb_common_dates <- mb_csv[mb_csv$forecast_date %in% dates,]
bl_common_dates <- bl_csv[bl_csv$forecast_date %in% dates,]

#subset mb and bl to point forecasts
mb_common_dates_point_only <- mb_common_dates[mb_common_dates$type == "point",]
bl_common_dates_point_only <- bl_common_dates[bl_common_dates$type == "point",]

#subset mb and bl to states
mb_common_dates_point_only_states_only <- mb_common_dates_point_only[which(nchar(mb_common_dates_point_only$location) == 2)]
bl_common_dates_point_only_states_only <- bl_common_dates_point_only[which(nchar(bl_common_dates_point_only$location) == 2)]

# remove US and territories 
territories <- c("60","66","78","69","US")
`%notin%` <- Negate(`%in%`)

mb_common_dates_point_only_states_only <- mb_common_dates_point_only_states_only[mb_common_dates_point_only_states_only$location %notin% territories,]
bl_common_dates_point_only_states_only <- bl_common_dates_point_only_states_only[bl_common_dates_point_only_states_only$location %notin% territories,]


# subset to inc death target

mb_common_dates_point_only_states_only_inc_death <- mb_common_dates_point_only_states_only[mb_common_dates_point_only_states_only$target %in% paste0(1:4, " wk ahead inc death"), ]
bl_common_dates_point_only_states_only_inc_death <- bl_common_dates_point_only_states_only[bl_common_dates_point_only_states_only$target %in% paste0(1:4, " wk ahead inc death"),]


#combine dataframes

joined_mb_and_bl <- mb_common_dates_point_only_states_only_inc_death %>% 
  left_join(bl_common_dates_point_only_states_only_inc_death,by=c("type", "forecast_date","target","target_end_date", "location")) %>%
  mutate(mb=value.x,bl=value.y) %>% select(-quantile.x) %>% select(-quantile.y) %>% select(-value.y) %>% select(-value.x)


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

#compute AE
joined_mb_and_bl_truth_complete$mb_ae <- abs(joined_mb_and_bl_truth_complete$mb -joined_mb_and_bl_truth_complete$value)
joined_mb_and_bl_truth_complete$bl_ae <- abs(joined_mb_and_bl_truth_complete$bl -joined_mb_and_bl_truth_complete$value)


# quantile_df

quantile_df <- data.frame(q=seq(0.025,.975,by=.05))
quantile_df$mb_q <- quantile(joined_mb_and_bl_truth_complete$mb_ae,probs=quantile_df$q)
quantile_df$bl_q <- quantile(joined_mb_and_bl_truth_complete$bl_ae,probs=quantile_df$q)

# empirical quantile using sorting instead
quantile_df <- data.frame(q=1:nrow(joined_mb_and_bl_truth_complete))
quantile_df$mb_q <- sort(joined_mb_and_bl_truth_complete$mb_ae)
quantile_df$bl_q <- sort(joined_mb_and_bl_truth_complete$bl_ae)

# theoretical quantile using sorting instead
quantile_df_theoretical <- data.frame(q=c(.8,.9,.95,.975,.99))
quantile_df_theoretical$mb_q <- quantile(joined_mb_and_bl_truth_complete$mb_ae,probs=quantile_df_theoretical$q)
quantile_df_theoretical$bl_q <- quantile(joined_mb_and_bl_truth_complete$bl_ae,probs=quantile_df_theoretical$q)





#### FIGURE 5 a TODO: label quantile values
library(ggplot2)
figure_5_ablation1 <- ggplot(quantile_df,aes(x=bl_q,y=mb_q,shape="MechBayes Fixed-Detection Death Only")) + geom_point(alpha=.2,size=1) + theme_bw()  + ylab("MechBayes Quantile of AE") +
  theme(aspect.ratio=1)  + geom_abline(intercept = 0,slope=1,alpha=.4) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[5],y=mb_q[5],col="99%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[4],y=mb_q[4],col="97.5%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[3],y=mb_q[3],col="95%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[2],y=mb_q[2],col="90%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[1],y=mb_q[1],col="80%"),size=2)  + xlab("MechBayes Fixed-Detection Death Only AE")  + coord_cartesian(xlim=c(0,1500),ylim=c(0,1500))

ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","figure_5_ablation1.png"),figure_5_ablation1,device="png",width = 6,height=6)


# define models
# get mb_csv
mb_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/mech_bayes_paper/total/",models[1],"/"))

#align mb forecast date
mb_csv$forecast_date <- mb_csv$forecast_date + 1

# get bl csv
bl_csv <- do.call_rbind_fread(paste0("/Users/gcgibson/mech_bayes_paper/total/",models[3],"/"))
bl_csv$forecast_date <- bl_csv$forecast_date + 1

#define common dates
dates <-seq(as.Date("2020-05-11"),as.Date("2020-10-18"),by="week")


#subset mb and bl to common dates
mb_common_dates <- mb_csv[mb_csv$forecast_date %in% dates,]
bl_common_dates <- bl_csv[bl_csv$forecast_date %in% dates,]

#subset mb and bl to point forecasts
mb_common_dates_point_only <- mb_common_dates[mb_common_dates$type == "point",]
bl_common_dates_point_only <- bl_common_dates[bl_common_dates$type == "point",]

#subset mb and bl to states
mb_common_dates_point_only_states_only <- mb_common_dates_point_only[which(nchar(mb_common_dates_point_only$location) == 2)]
bl_common_dates_point_only_states_only <- bl_common_dates_point_only[which(nchar(bl_common_dates_point_only$location) == 2)]

# remove US and territories 
territories <- c("60","66","78","69","US")
`%notin%` <- Negate(`%in%`)

mb_common_dates_point_only_states_only <- mb_common_dates_point_only_states_only[mb_common_dates_point_only_states_only$location %notin% territories,]
bl_common_dates_point_only_states_only <- bl_common_dates_point_only_states_only[bl_common_dates_point_only_states_only$location %notin% territories,]


# subset to inc death target

mb_common_dates_point_only_states_only_inc_death <- mb_common_dates_point_only_states_only[mb_common_dates_point_only_states_only$target %in% paste0(1:4, " wk ahead inc death"), ]
bl_common_dates_point_only_states_only_inc_death <- bl_common_dates_point_only_states_only[bl_common_dates_point_only_states_only$target %in% paste0(1:4, " wk ahead inc death"),]


#combine dataframes

joined_mb_and_bl <- mb_common_dates_point_only_states_only_inc_death %>% 
  left_join(bl_common_dates_point_only_states_only_inc_death,by=c("type", "forecast_date","target","target_end_date", "location")) %>%
  mutate(mb=value.x,bl=value.y) %>% select(-quantile.x) %>% select(-quantile.y) %>% select(-value.y) %>% select(-value.x)


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

#compute AE
joined_mb_and_bl_truth_complete$mb_ae <- abs(joined_mb_and_bl_truth_complete$mb -joined_mb_and_bl_truth_complete$value)
joined_mb_and_bl_truth_complete$bl_ae <- abs(joined_mb_and_bl_truth_complete$bl -joined_mb_and_bl_truth_complete$value)


# quantile_df

quantile_df <- data.frame(q=seq(0.025,.975,by=.05))
quantile_df$mb_q <- quantile(joined_mb_and_bl_truth_complete$mb_ae,probs=quantile_df$q)
quantile_df$bl_q <- quantile(joined_mb_and_bl_truth_complete$bl_ae,probs=quantile_df$q)

# empirical quantile using sorting instead
quantile_df <- data.frame(q=1:nrow(joined_mb_and_bl_truth_complete))
quantile_df$mb_q <- sort(joined_mb_and_bl_truth_complete$mb_ae)
quantile_df$bl_q <- sort(joined_mb_and_bl_truth_complete$bl_ae)

# theoretical quantile using sorting instead
quantile_df_theoretical <- data.frame(q=c(.8,.9,.95,.975,.99))
quantile_df_theoretical$mb_q <- quantile(joined_mb_and_bl_truth_complete$mb_ae,probs=quantile_df_theoretical$q)
quantile_df_theoretical$bl_q <- quantile(joined_mb_and_bl_truth_complete$bl_ae,probs=quantile_df_theoretical$q)





#### FIGURE 5 a TODO: label quantile values
library(ggplot2)
figure_5_ablation2 <- figure_5_ablation1 + geom_point(data=quantile_df,aes(x=bl_q,y=mb_q,shape="MechBayes Fixed-Detection"),alpha=.2,size=1)  + theme_bw() + xlab("Baseline Quantile of AE") + ylab("MechBayes Quantile of AE") +
  theme(aspect.ratio=1)  + geom_abline(intercept = 0,slope=1,alpha=.4) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[5],y=mb_q[5],col="99%"),size=2,shape=16) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[4],y=mb_q[4],col="97.5%"),size=2,shape=16) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[3],y=mb_q[3],col="95%"),size=2,shape=16) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[2],y=mb_q[2],col="90%"),size=2,shape=16) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[1],y=mb_q[1],col="80%"),size=2,shape=16)  + xlab("Reduced Model Quantile of AE") + coord_cartesian(xlim=c(0,2000),ylim=c(0,2000))

figure_5_ablation2 <- figure_5_ablation2+ theme(legend.title=element_blank())
ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","figure_5_ablation2.png"),figure_5_ablation2,device="png",width = 6,height=6)


