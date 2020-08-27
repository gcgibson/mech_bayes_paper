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
files <- state_deaths_subset_idx
files <- files[files !="US"]
files <- state.abb[match(files,state.name)]


beta <- matrix(NA,ncol=length(files),nrow=137 )
beta_upper <- matrix(NA,ncol=length(files),nrow=137 )
beta_lower <- matrix(NA,ncol=length(files),nrow=137 )
library(matrixStats)
col_idx <- 1
states = c()
for (file in files){
  if (file !="DC.npz"){
    print (file)
    npz1 <- np$load(paste0("/Users/gcgibson/covid-intervention-analysis/samples/",file,".npz"),allow_pickle = T)
    beta[,col_idx] = colMeans(npz1$f$mcmc_samples[[1]]$beta)
    beta_upper[,col_idx] = t(rowQuantiles(t(npz1$f$mcmc_samples[[1]]$beta),probs = .975))
    beta_lower[,col_idx] = t(rowQuantiles(t(npz1$f$mcmc_samples[[1]]$beta),probs = .025))
    
    states <-c(states,substr(file,1,2))
    col_idx <- col_idx + 1
  }
}


data_for_lmer  = data.frame(R0 = c((beta)),
                            R0_upper = c((beta_upper)),
                            R0_lower = c((beta_lower)),
                            t= rep(1:137,length(states)), state=rep(states,each=137))
#data_for_lmer$state <- as.factor(data_for_lmer$state)
data_for_lmer$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-07-19"),"days"),length(states))
library(openintro)

library(stringr)
regions_to_show <- c("TX", "FL", "CA", "NY", "NJ", "IL", "PA", "MA", "AZ", "GA", 
                     "OH", "MI", "CT", "SC", "MD", "LA", "IN", "TN", "MS", "VA")

data_for_lmer <- data_for_lmer[data_for_lmer$state %in% regions_to_show,]
data_for_lmer$state <- factor(data_for_lmer$state,levels=regions_to_show)
bet_t_plot <- ggplot(data_for_lmer, aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state) + theme_bw() + ylab("Beta(t)") + xlab("Date")
bet_t_plot <- bet_t_plot + geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3)
bet_t_plot <- bet_t_plot +geom_hline(yintercept = 1,linetype=2) +xlab("")
ggsave("/Users/gcgibson/mech_bayes_paper/beta_t_plot.png",bet_t_plot,device="png",width=8,height=6)






det_probs <- matrix(NA,nrow=137,ncol=length(files))
det_probs_upper <- matrix(NA,nrow=137,ncol=length(files))
det_probs_lower <- matrix(NA,nrow=137,ncol=length(files))

counter <- 1
for (file in files){
  npz1 <- read.csv(paste0("/Users/gcgibson/covid-intervention-analysis/summary/",file,".txt"))
  
  rw  <-c()
  rw_upper <- c()
  rw_lower <- c()
  for (line in 10:146){
     row <- (str_split(npz1$mean.......std....median......5.0......95.0......n_eff.....r_hat[line]," "))[[1]]
     row_formatted <- c()
     for (element in row){
       if (element != ""){
        row_formatted <- c(row_formatted,element)
       }
     }
     rw <-  c(rw,row_formatted[2])
     rw_upper <- c(rw_upper, row_formatted[6])
     rw_lower <- c(rw_lower, row_formatted[5])

     #rw <-c(rw,())
  }
  det_probs[,counter] <- rw
  det_probs_lower[,counter] <- rw_lower
  det_probs_upper[,counter] <- rw_upper
  
  counter <- counter +1
}
data_for_detection  = data.frame(R0 = as.numeric(c((det_probs))),
                                 R0_upper = as.numeric(c((det_probs_upper))),
                                R0_lower = as.numeric(c((det_probs_lower))),                     
                                 t= rep(1:137,length(states)), state=rep(states,each=137))

data_for_detection$state <- as.factor(data_for_detection$state)
data_for_detection$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-07-19"),"days"),length(states))
detection_plot <- ggplot(data_for_detection, aes(x=date,y=R0,group=state)) + geom_line() + facet_wrap(~state) + 
  theme_bw() + ylab("Detection Probability") + xlab("Date") #+ scale_y_discrete(breaks=seq(0,1,by=.1))
detection_plot <- detection_plot+ geom_ribbon(aes(x=date,ymin=R0_lower,ymax=R0_upper),alpha=.3) + xlab("") + 
ylab(TeX("p_{c,t}"))
ggsave("/Users/gcgibson/mech_bayes_paper/detection_plot.png",detection_plot,device="png",width=8,height=6)






## fit plot
library(covid19.analytics)
observed_data <- covid19.analytics::covid19.data(case = "ts-deaths")
US_subset <- observed_data[observed_data$Country.Region=="US",][1,40:ncol(observed_data)]
US_subset <- unlist(c(US_subset))
plot(US_subset)
