library(reticulate)
np <- import("numpy")

### read numpy files

ny_npz <- np$load("2020-10-18/samples/NY.npz",allow_pickle = T)
tx_npz <- np$load("2020-10-18/samples/TX.npz",allow_pickle = T)
ca_npz <- np$load("2020-10-18/samples/CA.npz",allow_pickle = T)
fl_npz <- np$load("2020-10-18/samples/FL.npz",allow_pickle = T)
library(tidyverse)

## get observed data
obs <- read.csv("/Users/gcgibson/covid19-forecast-hub/data-truth/truth-Incident Deaths.csv")


##### IN SAMPLE
ny_in_sample <- ny_npz$f$post_pred_samples[[1]]$dz
ca_in_sample <- ca_npz$f$post_pred_samples[[1]]$dz
fl_in_sample <- fl_npz$f$post_pred_samples[[1]]$dz
tx_in_sample <- tx_npz$f$post_pred_samples[[1]]$dz

dims <- dim(tx_in_sample)
top_panel_df_total <- data.frame(ny=c(t(ny_in_sample)),
                           fl=c(t(fl_in_sample)),
                           tx=c(t(tx_in_sample)),
                           ca=c(t(ca_in_sample)),
                           t = rep(1:dims[2],dims[1]),
                           sample=rep(1:dims[1],each=dims[2]))

top_panel_df <- top_panel_df_total %>% group_by(t) %>% mutate(ny_q_u = quantile(ny,probs=c(.975)),
                                        fl_q_u = quantile(fl,probs=c(.975)),
                                        tx_q_u = quantile(tx,probs=c(.975)),
                                        ca_q_u = quantile(ca,probs=c(.975)),
                                        ny_q_l = quantile(ny,probs=c(.025)),
                                        fl_q_l = quantile(fl,probs=c(.025)),
                                        tx_q_l = quantile(tx,probs=c(.025)),
                                        ca_q_l = quantile(ca,probs=c(.025))) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample)

top_panel_df_long <- top_panel_df %>% pivot_longer(cols=-t)
top_panel_df_long$region <- substr(top_panel_df_long$name,1,2)
top_panel_df_long$quantile <- substr(top_panel_df_long$name,4,6)
tmp<-top_panel_df_long %>% select(-name)%>% distinct() %>% pivot_wider(id_cols=c(t,region), values_from = value,names_from=quantile)



######### OUT OF SAMPLE

ny_out_of_sample <- ny_npz$f$forecast_samples[[1]]$dz_future
ca_out_of_sample <- ca_npz$f$forecast_samples[[1]]$dz_future
fl_out_of_sample <- fl_npz$f$forecast_samples[[1]]$dz_future
tx_out_of_sample <- tx_npz$f$forecast_samples[[1]]$dz_future

dims_out_of_sample <- dim(tx_out_of_sample)
t_future_seq <- dims[2]:(dims[2] -1  +dims_out_of_sample[2])
top_panel_df_out_of_sample_total <- data.frame(ny=c(t(ny_out_of_sample)),
                           fl=c(t(fl_out_of_sample)),
                           tx=c(t(tx_out_of_sample)),
                           ca=c(t(ca_out_of_sample)),
                           t = rep(t_future_seq,dims[1]),
                           sample=rep(1:dims[1],each=length(t_future_seq)))

top_panel_df_out_of_sample <- top_panel_df_out_of_sample_total %>% group_by(t) %>% mutate(ny_q_u = quantile(ny,probs=c(.975)),
                                                        fl_q_u = quantile(fl,probs=c(.975)),
                                                        tx_q_u = quantile(tx,probs=c(.975)),
                                                        ca_q_u = quantile(ca,probs=c(.975)),
                                                        ny_q_l = quantile(ny,probs=c(.025)),
                                                        fl_q_l = quantile(fl,probs=c(.025)),
                                                        tx_q_l = quantile(tx,probs=c(.025)),
                                                        ca_q_l = quantile(ca,probs=c(.025))) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample)

top_panel_df_long_out_of_sample <- top_panel_df_out_of_sample %>% pivot_longer(cols=-t)
top_panel_df_long_out_of_sample$region <- substr(top_panel_df_long_out_of_sample$name,1,2)
top_panel_df_long_out_of_sample$quantile <- substr(top_panel_df_long_out_of_sample$name,4,6)
tmp_out_of_sample<-top_panel_df_long_out_of_sample %>% select(-name)%>% distinct() %>% pivot_wider(id_cols=c(t,region), values_from = value,names_from=quantile)




##### GET MEDIANS

top_panel_df_median_out_of_sample <- top_panel_df_out_of_sample_total %>% group_by(t) %>% mutate(ny_m = median(ny),
                                                        fl_m = median(fl),
                                                        tx_m = median(tx),
                                                        ca_m = median(ca)) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample) %>% distinct()

top_panel_df_median_long_out_of_sample <- top_panel_df_median_out_of_sample%>% pivot_longer(cols=-t,names_to = "region",values_to="median")
top_panel_df_median_long_out_of_sample$region <- substr(top_panel_df_median_long_out_of_sample$region,1,2)
top_panel_df_median_long_out_of_sample$q_u <- NA
top_panel_df_median_long_out_of_sample$q_l <- NA


top_panel_df_median <- top_panel_df_total %>% group_by(t) %>% mutate(ny_m = median(ny),
                                                                     fl_m = median(fl),
                                                                     tx_m = median(tx),
                                                                     ca_m = median(ca)) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample) %>% distinct()

top_panel_df_median_long <- top_panel_df_median%>% pivot_longer(cols=-t,names_to = "region",values_to="median")
top_panel_df_median_long$region <- substr(top_panel_df_median_long$region,1,2)
top_panel_df_median_long$q_u <- NA
top_panel_df_median_long$q_l <- NA
## get dates
tmp_out_of_sample$date <-rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)
tmp$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-10-18"),by="day"),each=4)
top_panel_df_median_long$date <-  rep(seq(as.Date("2020-03-05"),as.Date("2020-10-18"),by="day"),each=4)
top_panel_df_median_long_out_of_sample$date <- rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)

## abbreviations to upper
tmp_out_of_sample$region <- toupper(tmp_out_of_sample$region)
tmp$region <- toupper(tmp$region)
top_panel_df_median_long$region <- toupper(top_panel_df_median_long$region)
top_panel_df_median_long_out_of_sample$region <- toupper(top_panel_df_median_long_out_of_sample$region)

## 

fips <- read.csv("/Users/gcgibson/covid19-forecast-hub/data-locations/locations.csv")
obs_w_fips <- obs %>% left_join(fips)
obs_w_fips$region <- obs_w_fips$abbreviation
obs_w_fips$date <- as.Date(obs_w_fips$date)
obs_w_fips_subset <- obs_w_fips[obs_w_fips$region %in% tmp_out_of_sample$region,]

top_panel <- ggplot() + geom_ribbon(data=tmp_out_of_sample,aes(x=date,ymax=q_u,ymin=q_l,fill="Out of Sample"),alpha=.4) +
  geom_ribbon(data=tmp,aes(x=date,ymax=q_u,ymin=q_l,fill="In Sample"),alpha=.4) +
   geom_line(data=top_panel_df_median_long,aes(x=date,y=median),col='red')+
  geom_line(data=top_panel_df_median_long_out_of_sample,aes(x=date,y=median),col='blue')+
  geom_point(data=obs_w_fips_subset,aes(x=date,y=value),alpha=.8,size=.2)+
   facet_wrap(~region,scales="free",nrow = 1) + theme_bw()  + ylab("Incident Deaths") +theme(legend.title =element_blank()) 

top_panel_log <- ggplot() + geom_ribbon(data=tmp_out_of_sample,aes(x=date,ymax=q_u,ymin=q_l,fill="Out of Sample"),alpha=.4) +
  geom_ribbon(data=tmp,aes(x=date,ymax=q_u,ymin=q_l,fill="In Sample"),alpha=.4) +
  geom_line(data=top_panel_df_median_long,aes(x=date,y=median),col='red')+
  geom_line(data=top_panel_df_median_long_out_of_sample,aes(x=date,y=median),col='blue')+
  geom_point(data=obs_w_fips_subset,aes(x=date,y=value),alpha=.8,size=.2)+  scale_y_continuous(trans='log10')+
  facet_wrap(~region,scales="free",nrow=1) + theme_bw()  + ylab("Incident Deaths (Log Scale)") +theme(legend.title =element_blank()) 

library(cowplot)  
fig_4 <- cowplot::plot_grid(top_panel,top_panel_log,beta_t,last_panel,nrow = 4,align = T)
ggsave("fig_4.png",fig_4,height=8,width=10)  
