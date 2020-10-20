library(reticulate)
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(lme4) #for lmer & glmer models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(latex2exp)

#library(effects)
library(sjstats) #use for r2 functions

np <- import("numpy")
files = list.files("/Users/gcgibson/mech_bayes_paper/samples/")
#files <- unlist(lapply(files,function(x){return (substr(x,1,2))}))
files <- state_deaths_subset_idx
files <- files[files !="US"]
files <- state.abb[match(files,state.name)]


beta <- matrix(NA,ncol=length(files),nrow=137+28)

beta_upper <- matrix(NA,ncol=length(files),nrow=137+28)
beta_lower <- matrix(NA,ncol=length(files),nrow=137+28 )
library(matrixStats)
col_idx <- 1
states = c()
for (file in files){
  if (file !="DC.npz"){
    print (file)
    npz1 <- np$load(paste0("/Users/gcgibson/mech_bayes_paper/samples/",file,".npz"),allow_pickle = T)
    beta[,col_idx] = colMeans(cbind(npz1$f$mcmc_samples[[1]]$beta,npz1$f$forecast_samples[[1]]$beta))
    beta_upper[,col_idx] = t(rowQuantiles(t(cbind(npz1$f$mcmc_samples[[1]]$beta,npz1$f$forecast_samples[[1]]$beta)),probs = .975))
    beta_lower[,col_idx] = t(rowQuantiles(t(cbind(npz1$f$mcmc_samples[[1]]$beta,npz1$f$forecast_samples[[1]]$beta)),probs = .025))
    
    states <-c(states,substr(file,1,2))
    col_idx <- col_idx + 1
  }
}


data_for_lmer  = data.frame(R0 = c((beta)),
                            R0_upper = c((beta_upper)),
                            R0_lower = c((beta_lower)),
                            t= rep(1:(137+28),length(states)), state=rep(states,each=(137+28)))
#data_for_lmer$state <- as.factor(data_for_lmer$state)
data_for_lmer$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-08-16"),"days"),length(states))
library(openintro)

library(stringr)
regions_to_show <- c("TX", "FL", "CA", "NY", "NJ", "IL", "PA", "MA", "AZ", "GA", 
                     "OH", "MI", "CT", "SC", "MD", "LA", "IN", "TN", "MS", "VA")

data_for_lmer <- data_for_lmer[data_for_lmer$state %in% regions_to_show,]
data_for_lmer$state <- factor(data_for_lmer$state,levels=regions_to_show)
bet_t_plot <- ggplot(data_for_lmer[data_for_lmer$date >= "2020-4-01",], aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state) + theme_bw() + ylab("Beta(t)") + xlab("Date")
bet_t_plot <- bet_t_plot + geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3)
#bet_t_plot <- bet_t_plot +geom_hline(yintercept = 1,linetype=2) +xlab("")
ggsave("/Users/gcgibson/mech_bayes_paper/beta_t_plot.png",bet_t_plot,device="png",width=8,height=6)






det_probs <- matrix(NA,nrow=137+28,ncol=length(files))
det_probs_upper <- matrix(NA,nrow=137+28,ncol=length(files))
det_probs_lower <- matrix(NA,nrow=137+28,ncol=length(files))

counter <- 1
for (file in files){

  rw  <-c()
  rw_upper <- c()
  rw_lower <- c()
  npz1 <- np$load(paste0("/Users/gcgibson/mech_bayes_paper/samples/samples/",file,".npz"),allow_pickle = T)
  det_probs[,counter] = colMeans(cbind(npz1$f$mcmc_samples[[1]]$det_prob,npz1$f$forecast_samples[[1]]$det_prob_future))
  det_probs_upper[,counter] = t(rowQuantiles(t(cbind(npz1$f$mcmc_samples[[1]]$det_prob,npz1$f$forecast_samples[[1]]$det_prob_future)),probs = .975))
  det_probs_lower[,counter] = t(rowQuantiles(t(cbind(npz1$f$mcmc_samples[[1]]$det_prob,npz1$f$forecast_samples[[1]]$det_prob_future)),probs = .025))
 
  
  counter <- counter +1
}
data_for_detection  = data.frame(R0 = as.numeric(c((det_probs))),
                                 R0_upper = as.numeric(c((det_probs_upper))),
                                R0_lower = as.numeric(c((det_probs_lower))),                     
                                 t= rep(1:165,length(states)), state=rep(states,each=165))

#data_for_detection$state <- as.factor(data_for_detection$state)
data_for_detection$state <- factor(data_for_detection$state,levels=regions_to_show)

data_for_detection$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-08-16"),"days"),length(states))
detection_plot <- ggplot(data_for_detection, aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state) + 
  theme_bw() + ylab("Detection Probability") + xlab("Date") #+ scale_y_discrete(breaks=seq(0,1,by=.1))
detection_plot <- detection_plot+ geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3) + xlab("") + 
ylab(TeX("p_{c,t}"))
ggsave("/Users/gcgibson/mech_bayes_paper/detection_plot.png",detection_plot,device="png",width=8,height=6)






## Figure 3
pct_df <- data_for_detection[data_for_detection$state %in% c("TX","NY","CA","FL"),]
pct_df <- pct_df[pct_df$date >= "2020-03-01",]
pct_df$state <- factor(pct_df$state , levels=c("TX","FL","CA","NY"))
  
  
beta_df <- data_for_lmer[data_for_lmer$date >= "2020-03-01" & data_for_lmer$state %in%  c("TX","NY","CA","FL"),]
beta_df$state <- factor(beta_df$state,levels=c("TX","FL","CA","NY"))

fit_and_forecast_results_df <- plot_df_subset[plot_df_subset$location_name %in% head(levels(plot_df_subset$location_name),12),]
fit_and_forecast_results_df <- fit_and_forecast_results_df[fit_and_forecast_results_df$location %in% c("TX","NY","CA","FL"),]
library(cowplot)

detection_plot_local <- ggplot(pct_df, aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state,nrow=1) + 
  theme_bw() + ylab("Detection Probability") + xlab("Date") #+ scale_y_discrete(breaks=seq(0,1,by=.1))
detection_plot_local <- detection_plot_local+ geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3) + xlab("") + 
  ylab(TeX("p_{c,t}"))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
extra_space <- seq(as.Date("2020-07-20"),as.Date("2020-08-16"),by="day")
detection_plot_local <- detection_plot_local + geom_line(data=data.frame(state=rep(c("TX","FL","CA","NY"),each= length(extra_space)),date=rep(extra_space,4),R0=rep(rep(.1,length(extra_space))),4),col='white')
detection_plot_local <- detection_plot_local + geom_vline(xintercept = as.Date("2020-07-18"),alpha=.75)

bet_t_plot_local <- ggplot(beta_df[beta_df$date >= "2020-03-01",], aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state,nrow = 1) + theme_bw() + ylab("Beta(t)") + xlab("Date")
bet_t_plot_local <- bet_t_plot_local + geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
bet_t_plot_local <- bet_t_plot_local + geom_line(data=data.frame(state=rep(c("TX","FL","CA","NY"),each= length(extra_space)),date=rep(extra_space,4),R0=rep(rep(.1,length(extra_space))),4),col='white')
bet_t_plot_local <- bet_t_plot_local + geom_vline(xintercept = as.Date("2020-07-18"),alpha=.75)


fit_and_forecast_results_df$location <- factor(fit_and_forecast_results_df$location,levels=c("CA","FL","NY","TX"))

state_deaths_subset_for_addition_local <- state_deaths_subset_for_addition[state_deaths_subset_for_addition$location %in% c("CA","FL","TX","NY"),]
state_deaths_subset_for_addition_local$location <- factor(state_deaths_subset_for_addition_local$location,levels=c("TX","FL","CA","NY"))

fit_and_forecast_results_plot_local <- ggplot(fit_and_forecast_results_df[fit_and_forecast_results_df$location %in% c("CA","FL","NY","TX") & as.Date(fit_and_forecast_results_df$time) >= as.Date("2020-03-01")  ,],aes(x=time,y=median)) + geom_line(col="cornflowerblue") +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.2,size=0,colour="cornflowerblue")+geom_point(data=state_deaths_subset_for_addition_local[state_deaths_subset_for_addition_local$date >= "2020-03-01",],aes(x=date,y=incident),size=.5)  +facet_wrap(~location,scales='free',nrow=1) +
  theme_bw() + geom_vline(xintercept = as.Date("2020-07-18"),alpha=.75)
fit_and_forecast_results_plot_local <- fit_and_forecast_results_plot_local + xlab("") + ylab("Incident Deaths") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#fit_and_forecast_results_plot_local <- fit_and_forecast_results_plot_local + scale_y_continuous(breaks=c(2,4,6),labels=c("2  ","4  ","6  "))

fit_and_forecast_results_plot_local_log <- ggplot(fit_and_forecast_results_df[fit_and_forecast_results_df$location %in% c("CA","FL","NY","TX") & as.Date(fit_and_forecast_results_df$time) >= as.Date("2020-03-01")  ,],aes(x=time,y=median)) + geom_line(col="cornflowerblue") +
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.2,size=0,colour="cornflowerblue")+geom_point(data=state_deaths_subset_for_addition_local[state_deaths_subset_for_addition_local$date >= "2020-03-01",],aes(x=date,y=incident),size=.5)  +facet_wrap(~location,scales='free',nrow=1) +
  theme_bw() + geom_vline(xintercept = as.Date("2020-07-18"),alpha=.75)
fit_and_forecast_results_plot_local_log <- fit_and_forecast_results_plot_local_log + xlab("") + ylab("Incident Deaths (Log Scale)") + scale_y_continuous(trans='log10')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


library(cowplot)
fig3 <- cowplot::plot_grid(fit_and_forecast_results_plot_local,fit_and_forecast_results_plot_local_log,bet_t_plot_local,detection_plot_local , labels = c('A', 'B','C','D'), rel_heights = c(1.2,1.2,1,1), label_size = 12,nrow=4,align = "v") 
ggsave(file='/Users/gcgibson/mech_bayes_paper/fig3.png',plot=fig3,device = 'png',height = 10,width = 10)

