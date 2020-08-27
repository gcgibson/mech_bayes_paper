library(lme4)
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
         mae=abs_error,error=error,truth=truth)

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

mae_results_by_time_zero_df <- tmp_subset[tmp_subset$timezero >= "2020-05-12" & tmp_subset$timezero <= "2020-08-01",] %>% group_by(timezero,model,unit,target,truth) %>% summarize(mae=mean(mae))

library(lme4)
library(nlme)


#diagnostic_fit <- update(diagnostic_fit, correlation = corAR1())
diagnostic_fit <- lmer(log(mae+1)~ factor(target) + factor(model):factor(target) +  ( 1   | unit),data=mae_results_by_time_zero_df)

summary(diagnostic_fit)
library(sjPlot)
qqnorm(resid(diagnostic_fit))
qqline(resid(diagnostic_fit))
## contrast for two week ahead
coefs <- fixef(diagnostic_fit)
varb <- vcov(diagnostic_fit)
c1 <- as.matrix(c(0,1,0,0,0,1,0,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
two_week_ahead_p <- (1-pt(abs(t1), df = 4343))*2

## contrast for three week ahead
coefs <- fixef(diagnostic_fit)
varb <- vcov(diagnostic_fit)
c1 <- as.matrix(c(0,1,0,0,0,0,1,0))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
three_week_ahead_p <- (1-pt(abs(t1), df = 4343))*2

## contrast for four week ahead
coefs <- fixef(diagnostic_fit)
varb <- vcov(diagnostic_fit)
c1 <- as.matrix(c(0,1,0,0,0,0,0,1))
est1 <- t(c1)%*%coefs
varc1 <- t(c1)%*%varb%*%c1
t1 <- as.numeric(est1/sqrt(varc1))
four_week_ahead_p <- (1-pt(abs(t1), df = 4343))*2

print (c(two_week_ahead_p,three_week_ahead_p,four_week_ahead_p))



mae_results_by_time_zero_df_wide <- mae_results_by_time_zero_df %>% spread(model,mae)
mae_results_by_time_zero_df_wide$rmae <- mae_results_by_time_zero_df_wide$`UMass-MechBayes` - mae_results_by_time_zero_df_wide$`COVIDhub-baseline`
ggplot(mae_results_by_time_zero_df_wide,aes(x=truth,y=rmae)) + geom_point() + facet_grid(~target)
