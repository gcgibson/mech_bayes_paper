library(stringr)
library(dplyr)
library(RCurl)
#deaths <- read.csv("/Users/gcgibson/Desktop/truth-Incident Deaths.txt")  # Number of students in bed
x <- getURL("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-truth/truth-Cumulative%20Deaths.csv")
y <- read.csv(text = x)
deaths <- y[!grepl("County",y$location_name) , ]



state_deaths <- deaths %>%
  filter(!str_detect(location_name, "County"))
state_deaths$date <- as.Date(state_deaths$date)
state_deaths <- state_deaths %>% group_by(location_name) %>%mutate(incident=c(value[1],diff(value))) %>% ungroup()

state_deaths_subset_idx  <- unique(state_deaths[state_deaths$incident >= 50,]$location_name)

state_deaths_subset <- state_deaths[state_deaths$location_name %in% state_deaths_subset_idx,]
library(ggplot2)


state_deaths_subset_total_deaths <- state_deaths_subset %>% group_by(location) %>% mutate(total_deaths = sum(value)) %>% ungroup()
state_deaths_subset <- left_join(state_deaths_subset,state_deaths_subset_total_deaths)
state_deaths_subset$total_deaths <- as.factor(state_deaths_subset$total_deaths)
state_deaths_subset$total_deaths <- factor(state_deaths_subset$total_deaths,levels = rev(levels(state_deaths_subset$total_deaths)))
state_deaths_subset$location_name <- as.factor(state_deaths_subset$location_name)
state_deaths_subset <- state_deaths_subset[order(state_deaths_subset$total_deaths),]
state_deaths_subset$location_name <- factor(state_deaths_subset$location_name,levels = unique(state_deaths_subset$location_name))
data_plot <- ggplot(state_deaths_subset,aes(x=date,y=incident)) + geom_point(size=.5) + facet_wrap(~location_name,scales="free") + theme_bw() +ylab("Incident Reported COVID-19 Deaths (per day)") + xlab("")
ggsave("/Users/gcgibson/mech_bayes_paper/data_plot.png",data_plot,device="png",width=10,height=10)



library(reticulate)
np <- import("numpy")
files = list.files("/Users/gcgibson/mech_bayes_paper/samples/samples")
fit_plot_df <- array(NA,dim=c(length(files),3,172) )
col_idx <- 1
states = c()
for (file in files){
    print (file)
    npz1 <- np$load(paste0("/Users/gcgibson/mech_bayes_paper/samples/samples/",file),allow_pickle = T)
    tmp <- npz1$f$post_pred_samples[[1]]$dz
    tmp_fcast <- npz1$f$forecast_samples[[1]]$dz_future
    tmp <- cbind(tmp,tmp_fcast)
    tmp_med <- apply(tmp,2,median)
    tmp_f <- function(x)return(quantile(x,probs = .975))
    tmp_f2 <- function(x)return(quantile(x,probs = .025))
    
    tmp_95 <- apply(tmp,2,tmp_f)
    tmp_05 <- apply(tmp,2,tmp_f2)
    
    fit_plot_df[col_idx,1 ,] <- tmp_med
    fit_plot_df[col_idx,2 ,] <- tmp_95
    fit_plot_df[col_idx,3 ,] <- tmp_05
    
    
    col_idx <- col_idx + 1
    states <-c(states,substr(file,1,2))
    
  
}

plot_df <- data.frame(time=rep(seq(as.Date("2020-03-05"),as.Date("2020-08-23"),by="day"),length(states)),
                      median=c(t(fit_plot_df[,1,])),upper=c(t(fit_plot_df[,2,])),
                      lower=c(t(fit_plot_df[,3,])))
plot_df$location <- rep(states,each=172)
plot_df_subset <- plot_df[plot_df$location %in% c("US",state.abb[match(state_deaths_subset_idx,state.name)]), ]
state_deaths_subset_for_addition <- state_deaths_subset#[state_deaths_subset$location_name != "New Jersey",]
state_deaths_subset_for_addition <- state_deaths_subset_for_addition[state_deaths_subset_for_addition$date %in% plot_df_subset$time,]
state_deaths_subset_for_addition$location <- state.abb[match(state_deaths_subset_for_addition$location_name,state.name)]
state_deaths_subset_for_addition[is.na(state_deaths_subset_for_addition$location),]$location <- "US"
state_deaths_subset_for_addition <- state_deaths_subset_for_addition[order(state_deaths_subset_for_addition$total_deaths),]
state_deaths_subset_for_addition$location_name <- factor(state_deaths_subset_for_addition$location_name,levels = unique(state_deaths_subset_for_addition$location_name))

plot_df_subset$location_name <- state.name[match(plot_df_subset$location, state.abb)]
plot_df_subset$location_name  <- factor(plot_df_subset$location_name ,levels=unique(state_deaths_subset_for_addition$location_name))
plot_df_subset[is.na(plot_df_subset$location_name), ]$location_name <- "US"
fit_and_forecast_results <- ggplot(plot_df_subset,aes(x=time,y=median)) + geom_line(col="cornflowerblue") +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.2,size=0,colour="cornflowerblue")+geom_point(data=state_deaths_subset_for_addition,aes(x=date,y=incident),size=.5)  +facet_wrap(~location_name,scales='free') +
  theme_bw() + geom_vline(xintercept = as.Date("2020-07-26"),alpha=.75)
ggsave("/Users/gcgibson/mech_bayes_paper/fit_and_forecast_results.png",fit_and_forecast_results,device="png",width=8,height=8)


plot_df_subset$date <- plot_df_subset$time
coverage_df <- plot_df_subset %>% left_join(state_deaths_subset_for_addition,by=c("date","location"))
coverage_df <- coverage_df %>% group_by(location,date) %>% mutate(cov = ifelse(lower <= incident & incident <= upper,1,0))
sum(coverage_df$cov,na.rm=T)/length(coverage_df$cov)
