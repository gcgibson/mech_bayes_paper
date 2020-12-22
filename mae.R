library(tidyverse)
library(data.table)
library(lubridate)


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
truth <- read.csv(url("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Deaths.csv"))#read.csv("/Users/gcgibson/covid19-forecast-hub/data-truth/truth-Incident Deaths.csv")
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
colours = c("0.80" = "deepskyblue", "0.90" = "deepskyblue1", "0.95" = "deepskyblue2", "0.975" = "deepskyblue3","0.99"="deepskyblue4","Median" = "maroon","Mean"="maroon4")

figure_5 <- ggplot(quantile_df,aes(x=bl_q,y=mb_q)) + geom_point(size=.5) + theme_bw() + xlab("Baseline Quantile of AE") + ylab("MechBayes Quantile of AE") +
  theme(aspect.ratio=1)  + geom_abline(intercept = 0,slope=1) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[5],y=mb_q[5],col="99%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[4],y=mb_q[4],col="97.5%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[3],y=mb_q[3],col="95%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[2],y=mb_q[2],col="90%"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[1],y=mb_q[1],col="80%"),size=2) + theme(legend.title = element_blank()) + xlab("Baseline Quantile of AE") + ylab("MechBayes Quantile of AE") + coord_cartesian( ylim=c(0,3000),xlim=c(0,3000))  +
  scale_color_manual(values = colours)

  
##### FIGURE 5 B

figure_5_b <- ggplot(quantile_df,aes(x=bl_q,y=mb_q)) + geom_point(size=.5) + theme_bw() + xlab("Baseline Quantile of AE") + ylab("MechBayes Quantile of AE") +
  theme(aspect.ratio=1)  + geom_abline(intercept = 0,slope=1) +
  geom_point(aes(x=median(joined_mb_and_bl_truth_complete$bl_ae),y=median(joined_mb_and_bl_truth_complete$mb_ae),col="Median"),size=2) +
  geom_point(aes(x=mean(joined_mb_and_bl_truth_complete$bl_ae),y=mean(joined_mb_and_bl_truth_complete$mb_ae),col="Mean"),size=2) +
  coord_cartesian( ylim=c(0,200),xlim=c(0,200)) +  theme(legend.title = element_blank())  +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[1],y=mb_q[1],col="0.80"),size=2) +
  geom_point(data=quantile_df_theoretical,aes(x=bl_q[2],y=mb_q[2],col="0.90"),size=2) +
geom_point(data=quantile_df_theoretical,aes(x=bl_q[3],y=mb_q[3],col="0.95"),size=2) +
  scale_color_manual(values = colours) +
  geom_blank(aes(col="0.975"),size=2) +
  geom_blank(aes(col="0.99"),size=2) 
  

  

library(cowplot)
library(gridExtra)
library(grid)
fig_5_total <- cowplot::plot_grid(figure_5,figure_5_b,nrow=2,align="v")
y.grob <- textGrob("MechBayes Quantile of AE", 
                   gp=gpar( col="black", fontsize=12), rot=90)

fig_5_total <- grid.arrange(arrangeGrob(fig_5_total, left = y.grob))


ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","fig_5_total.png"),fig_5_total,device="png",width = 6,height=6)

#### FIGURE 6
#  2) Errors by time. My suggestion is to
  #average over locations. 
  #Then, for each target, plot MAE vs. date. 
  #Use points/lines. There will be up to 8 lines 
  #(MB/baseline x 4 targets). See if all eight of 
  #these can fit on one plot; if not we can tweak or
  #subselect targets.

## TODO: remove US forecasts and territories

fig_6_b <- ggplot(joined_mb_and_bl_truth_complete %>% group_by(target_end_date) %>% summarize(mb_ae=mean(mb_ae,na.rm=T),bl_ae =mean(bl_ae,a.rm=T)),
       aes(x=target_end_date,y=mb_ae,col="MechBayes")) + geom_line() + geom_point()  +
  geom_line(aes(x=target_end_date,y=bl_ae,col="Baseline")) + geom_point(aes(x=target_end_date,y=bl_ae,col="Baseline"))+  theme_bw()  +ylab("MAE") +
  xlab("Target Date") +theme(legend.title=element_blank(),legend.position="bottom")+ ylim(c(0,80))


truth_states_by_week_subset <- truth_states_by_week[truth_states_by_week$target_end_date >= "2020-05-16" & truth_states_by_week$target_end_date  <= "2020-11-07",]
fig_6_df <- truth_states_by_week_subset %>% group_by(target_end_date) %>% summarize(value=sum(value))
fig_6_df$date <- fig_6_df$target_end_date
fig_6_a <- ggplot(fig_6_df,aes(x=target_end_date,y=value)) + geom_line() + geom_point() + theme_bw() + xlab("") + ylab("Total Incident Deaths")

fig_6 <- cowplot::plot_grid(fig_6_a,fig_6_b,align = "v",ncol=1,rel_heights = c(.75,1))
ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","fig_6.png"),fig_6_b + theme(legend.position = "none"),device="png",width = 6,height=6)


### FIG 7

## TODO: replace fips codes with state abbrevs
fig_7_df <- joined_mb_and_bl_truth_complete %>% group_by(location,target) %>% summarize(mb_ae=mean(mb_ae,na.rm=T),bl_ae =mean(bl_ae,a.rm=T))
fips <- read.csv("/Users/gcgibson/covid19-forecast-hub/data-locations/locations.csv")
fig_7_df <- fig_7_df %>% left_join(fips,by="location")
fig_7_df$target <- factor(fig_7_df$target)
levels(fig_7_df$target) <- paste0(1:4," week ahead")
fig_7 <- ggplot(fig_7_df[fig_7_df$location !="US",] %>% group_by(location,abbreviation) %>% summarize(bl_ae = mean(bl_ae),mb_ae = mean(mb_ae)),
       aes(x=bl_ae,y=mb_ae)) + geom_text(aes(label=abbreviation),size=2)  + ylab("MechBayes MAE") + xlab("Baseline MAE") +
  geom_abline(intercept=0,slope=1,alpha=.4) + theme_bw() #+ facet_wrap(~target,scales="free")

ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","fig_7.png"),fig_7,device="png",width = 6,height=6)

#### Target plot
### TODO: remove states and territories
joined_mb_and_bl_truth_complete_long <- joined_mb_and_bl_truth_complete%>%  pivot_longer(cols=c("mb_ae","bl_ae"),names_to=c("model"),values_to=c("model_ae")) %>% select(-value)
joined_mb_and_bl_truth_complete_long$model <- as.factor(joined_mb_and_bl_truth_complete_long$model)
levels(joined_mb_and_bl_truth_complete_long$model) <- c("Baseline","MechBayes")
joined_mb_and_bl_truth_complete_long$target <- factor(joined_mb_and_bl_truth_complete_long$target)
levels(joined_mb_and_bl_truth_complete_long$target) <- paste0(1:4, " week")
fig_8 <- ggplot(joined_mb_and_bl_truth_complete_long %>% group_by(target,forecast_date,model) %>% summarize(model_ae=mean(model_ae)),aes(x=target,y=model_ae,col=model)) +
  geom_boxplot() + theme_bw()  +ylim(c(0,150)) + ylab("MAE") +   theme(legend.position = "none") + xlab("Target")


ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","fig_8.png"),fig_8,device="png",width = 6,height=6)

grobs <- ggplotGrob(fig_6_b)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","legend.png"),legend,width=6,height=6)

multi_plot<- cowplot::plot_grid(fig_7,fig_6_b +theme(legend.position="none"),fig_8,fig_9,align = "v",rows = 2,labels=c("A","B","C","D"))
ggsave(paste0("/Users/gcgibson/mech_bayes_paper/","multi_plot.png"),multi_plot,device="png",width = 6,height=6)

### regression analysis 
library(lme4)
library(lmerTest)

fit <- lmer(log(model_ae+1) ~ target:model + (1 | location),data=joined_mb_and_bl_truth_complete_long)

summary(fit)

#### Check forecast target

ggplot(joined_mb_and_bl_truth_complete_long,aes(x=location,y=forecast_date)) + geom_point() + facet_wrap(~model)
