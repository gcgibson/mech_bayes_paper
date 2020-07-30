library(stringr)
library(dplyr)
deaths <- read.csv("/Users/gcgibson/Desktop/truth-Incident Deaths.txt")  # Number of students in bed



state_deaths <- deaths %>%
  filter(!str_detect(location_name, "County"))
state_deaths$date <- as.Date(state_deaths$adate)

state_deaths_subset_idx  <- unique(state_deaths[state_deaths$value >= 50,]$location_name)

state_deaths_subset <- state_deaths[state_deaths$location_name %in% state_deaths_subset_idx,]
library(ggplot2)

data_plot <- ggplot(state_deaths_subset,aes(x=date,y=value)) + geom_point(size=.5) + facet_wrap(~location_name,scales="free") + theme_bw()
ggsave("/Users/gcgibson/mech_bayes_paper/data_plot.png",data_plot,device="png",width=10,height=10)



library(reticulate)
np <- import("numpy")
files = list.files("/Users/gcgibson/mech_bayes_paper/samples/samples")
fit_plot_df <- array(NA,dim=c(length(files),3,144) )
col_idx <- 1
states = c()
for (file in files){
    print (file)
    npz1 <- np$load(paste0("/Users/gcgibson/mech_bayes_paper/samples/samples/",file),allow_pickle = T)
    tmp <- npz1$f$post_pred_samples[[1]]$dz
    tmp_med <- apply(tmp,2,median)
    tmp_f <- function(x)return(quantile(x,probs = .95))
    tmp_f2 <- function(x)return(quantile(x,probs = .05))
    
    tmp_95 <- apply(tmp,2,tmp_f)
    tmp_05 <- apply(tmp,2,tmp_f2)
    
    fit_plot_df[col_idx,1 ,] <- tmp_med
    fit_plot_df[col_idx,2 ,] <- tmp_95
    fit_plot_df[col_idx,3 ,] <- tmp_05
    
    
    col_idx <- col_idx + 1
    states <-c(states,substr(file,1,2))
    
  
}

plot_df <- data.frame(time=rep(seq(as.Date("2020-03-05"),as.Date("2020-07-26"),by="day"),length(states)),
                      median=c(t(fit_plot_df[,1,])),upper=c(t(fit_plot_df[,2,])),
                      lower=c(t(fit_plot_df[,3,])))
plot_df$location <- rep(states,each=144)
plot_df_subset <- plot_df[plot_df$location %in% c("US",state.abb[match(state_deaths_subset_idx,state.name)]), ]
state_deaths_subset_for_addition <- state_deaths_subset#[state_deaths_subset$location_name != "New Jersey",]
state_deaths_subset_for_addition <- state_deaths_subset_for_addition[state_deaths_subset_for_addition$date %in% plot_df_subset$time,]
state_deaths_subset_for_addition$location <- state.abb[match(state_deaths_subset_for_addition$location_name,state.name)]
state_deaths_subset_for_addition[is.na(state_deaths_subset_for_addition$location),]$location <- "US"
fit_and_forecast_results <- ggplot(plot_df_subset,aes(x=time,y=median)) + geom_line(col="cornflowerblue") +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.2,size=0,colour="cornflowerblue")+geom_point(data=state_deaths_subset_for_addition,aes(x=date,y=value),size=.5)  +facet_wrap(~location,scales='free') + theme_bw() 
ggsave("/Users/gcgibson/mech_bayes_paper/fit_and_forecast_results.png",fit_and_forecast_results,device="png",width=8,height=8)

