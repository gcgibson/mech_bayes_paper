library(reticulate)
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(lme4) #for lmer & glmer models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
#library(effects)
library(sjstats) #use for r2 functions

np <- import("numpy")
files = list.files("/Users/gcgibson/covid-intervention-analysis/samples/")
files <- c("US.npz","FL.npz","NY.npz","CA.npz")
beta <- matrix(NA,ncol=length(files),nrow=137 )
col_idx <- 1
states = c()
for (file in files){
  if (file !="DC.npz"){
    print (file)
    npz1 <- np$load(paste0("/Users/gcgibson/covid-intervention-analysis/samples/",file),allow_pickle = T)
    beta[,col_idx] = colMeans(npz1$f$mcmc_samples[[1]]$beta)
    states <-c(states,substr(file,1,2))
    col_idx <- col_idx + 1
  }
}

X <- read.csv(url("https://raw.githubusercontent.com/CEIDatUGA/COVID-19-DATA/master/US/us-state-intervention-data/US_state_intervention_time_series.csv"))

data_for_lmer  = data.frame(R0 = c((beta)), t= rep(1:137,length(states)), state=rep(states,each=137))
data_for_lmer$state <- as.factor(data_for_lmer$state)
data_for_lmer$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-07-19"),"days"),length(states))
library(openintro)

library(stringr)

bet_t_plot <- ggplot(data_for_lmer, aes(x=date,y=R0,group=state,col=state)) + geom_line() + facet_grid(~state) + theme_bw() + ylab("Beta(t)") + xlab("Date")
ggsave("/Users/gcgibson/mech_bayes_paper/beta_t_plot.png",bet_t_plot,device="png",width=8,height=4)

det_probs <- matrix(NA,nrow=137,ncol=4)
counter <- 1
for (file in c("US.txt","CA.txt","NY.txt","FL.txt")){
  npz1 <- read.csv(paste0("/Users/gcgibson/covid-intervention-analysis/summary/",file))
  
  rw  <-c()
  for (line in 10:146){
     row <- (str_split(npz1$mean.......std....median......5.0......95.0......n_eff.....r_hat[line]," "))[[1]]
     row_formatted <- c()
     for (element in row){
       if (element != ""){
        row_formatted <- c(row_formatted,element)
       }
     }
     rw <-  c(rw,row_formatted[2])

     #rw <-c(rw,())
  }
  print (length(rw))
  det_probs[,counter] <- rw
  counter <- counter +1
}
data_for_detection  = data.frame(R0 = as.numeric(c((det_probs))), t= rep(1:137,length(states)), state=rep(states,each=137))
data_for_detection$state <- as.factor(data_for_detection$state)
data_for_detection$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-07-19"),"days"),length(states))
detection_plot <- ggplot(data_for_detection, aes(x=date,y=R0,group=state,col=state)) + geom_line() + facet_grid(~state) + 
  theme_bw() + ylab("Detection Probability") + xlab("Date") #+ scale_y_discrete(breaks=seq(0,1,by=.1))
ggsave("/Users/gcgibson/mech_bayes_paper/detection_plot.png",detection_plot,device="png",width=8,height=4)






## fit plot
library(covid19.analytics)
observed_data <- covid19.analytics::covid19.data(case = "ts-deaths")
US_subset <- observed_data[observed_data$Country.Region=="US",][1,40:ncol(observed_data)]
US_subset <- unlist(c(US_subset))
plot(US_subset)
