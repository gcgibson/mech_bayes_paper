library(zoltr) ## devtools::install_github("reichlab/zoltr")
library(tidyverse)
library(lubridate)
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, "gcgibson", "casey!ili")


tmp <- scores(zoltar_connection, "https://www.zoltardata.com/api/project/44/") %>%
  mutate(truth=truth)


mb_prefix <- "/Users/gcgibson/mech_bayes_paper/UMass-MechBayes/"
baseline_prefix <- "/Users/gcgibson/mech_bayes_paper/COVIDhub-baseline/"

baseline_files <- list.files(baseline_prefix)
mb_files <- list.files(mb_prefix)
dates <- c("2020-05-17", "2020-05-24", "2020-05-31", "2020-06-07", "2020-06-14", "2020-06-21",
           "2020-06-28", "2020-07-05", "2020-07-12", "2020-07-19", "2020-07-26", "2020-08-02" ,"2020-08-09" ,
           "2020-08-16")

locations <-   c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", 
  "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", 
  "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
  "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", 
  "49", "50", "51", "53", "54", "55", "56")


coverage_probability_mb <- array(NA,dim = c(length(dates),length(locations),4))
coverage_probability_baseline <-array(NA,dim = c(length(dates),length(locations),4))

date_idx <- 1
for (date_ in dates){
  file_idx_mb <- which(grepl(date_,mb_files))
  mb_file <- read.csv(paste0(mb_prefix,mb_files[file_idx_mb]))
  
  file_idx_baseline <- which(grepl(as.Date(date_)+1,baseline_files))
  baseline_file <- read.csv(paste0(baseline_prefix,baseline_files[file_idx_baseline]))
  location_idx <- 1
  for (location in locations){
    target_idx <- 1
    for (target in 1:4){
      tmp_mb <- mb_file[mb_file$location == location & mb_file$quantile == .025 & mb_file$target == paste0(target," wk ahead inc death"),]
      tmp_mb_lower <- tmp_mb[complete.cases(tmp_mb),]$value
      
      tmp_mb <- mb_file[mb_file$location == location & mb_file$quantile == .975 & mb_file$target == paste0(target," wk ahead inc death"),]
      tmp_mb_upper <- tmp_mb[complete.cases(tmp_mb),]$value
      
      tmp_baseline <- baseline_file[baseline_file$location == location & baseline_file$quantile == .025 & baseline_file$target == paste0(target," wk ahead inc death"),]
      tmp_baseline_lower <- tmp_baseline[complete.cases(tmp_baseline),]$value
      
      tmp_baseline <- baseline_file[baseline_file$location == location & baseline_file$quantile == .975 & baseline_file$target == paste0(target," wk ahead inc death"),]
      tmp_baseline_upper <- tmp_baseline[complete.cases(tmp_baseline),]$value
      
      truth_local <- tmp[tmp$timezero==date_ & tmp$unit == location & tmp$target == paste0(target," wk ahead inc death"),]$truth[1]
      coverage_probability_mb[date_idx,location_idx,target_idx] <- ifelse(truth_local <=tmp_mb_upper & truth_local >= tmp_mb_lower,1,0  )
      coverage_probability_baseline[date_idx,location_idx,target_idx] <- ifelse(truth_local <=tmp_baseline_upper & truth_local >= tmp_baseline_lower,1,0  )
      
      target_idx <- target_idx + 1
    }
    location_idx <- location_idx + 1
  }
  date_idx <- date_idx + 1
}

library(ggplot2)

coverage_probability_df <- data.frame(date = rep(dates,each = 4*length(locations)),
                                         location =rep(rep(locations,each=4),length(dates)),
                                         target = rep(paste0(1:4, " wk ahead inc death"),length(locations)*length(dates)),
                                         bl = c(coverage_probability_baseline),
                                      mb = c(coverage_probability_mb))
library(reshape2)
coverage_probability_df_long <- melt(coverage_probability_df[complete.cases(coverage_probability_df),], id.vars=c("date", "location","target"))
coverage_probability_df_long$model <- coverage_probability_df_long$variable
coverage_probability_df_long$cp <- coverage_probability_df_long$value
levels(coverage_probability_df_long$model) <- c("COVIDhub-baseline","UMass-MechBayes")


cp_results_by_time_zero <- ggplot(coverage_probability_df_long %>% group_by(date,model) %>% summarize(cp=mean(cp)),aes(x=as.Date(date),y=cp,col=model)) + geom_point() + theme_bw() + geom_hline(yintercept = .95)+ ylab("Coverage Probability") + xlab("Date")
ggsave("/Users/gcgibson/mech_bayes_paper/cp_results_by_time_zero.png",cp_results_by_time_zero,device="png",width=7,height=4)


regions_to_show <- c("TX", "FL", "CA", "NY", "NJ", "IL", "PA", "MA", "AZ", "GA", 
  "OH", "MI", "CT", "SC", "MD", "LA", "IN", "TN", "MS", "VA")
fips_csv <- read.csv(url("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv"))
tmp_subset_r <- coverage_probability_df_long %>% group_by(location,model) %>% summarize(cp=mean(cp)) %>% left_join(fips_csv,by='location')
tmp_subset_r <- tmp_subset_r[tmp_subset_r$abbreviation %in% regions_to_show,]

tmp_subset_r$abbreviation <- factor(tmp_subset_r$abbreviation,levels=regions_to_show)
cp_results_by_location <- ggplot(tmp_subset_r,aes(x=abbreviation,y=cp,col=model)) + geom_point() + theme_bw() + ylab("Coverage Probability") + xlab("State") + geom_hline(yintercept = .95)
ggsave("/Users/gcgibson/mech_bayes_paper/cp_results_by_location.png",cp_results_by_location,device="png",width=7,height=4)


cp_results_by_target <- ggplot(coverage_probability_df_long %>% group_by(target,model) %>% summarize(cp=mean(cp)),aes(x=target,y=cp,col=model)) + geom_point() + theme_bw() + ylab("Coverage Probability") + xlab("Target")+ geom_hline(yintercept = .95)
ggsave("/Users/gcgibson/mech_bayes_paper/cp_results_by_target.png",cp_results_by_target,device="png",width=7,height=4)

