library(reticulate)
np <- import("numpy")

### read numpy files

ny_npz <- np$load("2020-10-18/samples/NY.npz",allow_pickle = T)
tx_npz <- np$load("2020-10-18/samples/TX.npz",allow_pickle = T)
ca_npz <- np$load("2020-10-18/samples/CA.npz",allow_pickle = T)
fl_npz <- np$load("2020-10-18/samples/FL.npz",allow_pickle = T)
library(tidyverse)



#### IN SAMPLE
ny_in_sample <- ny_npz$f$mcmc_samples[[1]]$det_prob
ca_in_sample <- ca_npz$f$mcmc_samples[[1]]$det_prob
fl_in_sample <- fl_npz$f$mcmc_samples[[1]]$det_prob
tx_in_sample <- tx_npz$f$mcmc_samples[[1]]$det_prob

dims <- dim(tx_in_sample)
mid_panel_df_total <- data.frame(ny=c(t(ny_in_sample)),
                                 fl=c(t(fl_in_sample)),
                                 tx=c(t(tx_in_sample)),
                                 ca=c(t(ca_in_sample)),
                                 t = rep(1:dims[2],dims[1]),
                                 sample=rep(1:dims[1],each=dims[2]))

mid_panel_df <- mid_panel_df_total %>% group_by(t) %>% mutate(ny_q_u = quantile(ny,probs=c(.975)),
                                                              fl_q_u = quantile(fl,probs=c(.975)),
                                                              tx_q_u = quantile(tx,probs=c(.975)),
                                                              ca_q_u = quantile(ca,probs=c(.975)),
                                                              ny_q_l = quantile(ny,probs=c(.025)),
                                                              fl_q_l = quantile(fl,probs=c(.025)),
                                                              tx_q_l = quantile(tx,probs=c(.025)),
                                                              ca_q_l = quantile(ca,probs=c(.025))) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample)
mid_panel_df_long <- mid_panel_df %>% pivot_longer(cols=-t)
mid_panel_df_long$region <- substr(mid_panel_df_long$name,1,2)
mid_panel_df_long$quantile <- substr(mid_panel_df_long$name,4,6)
tmp_mid <-mid_panel_df_long %>% select(-name)%>% distinct() %>% pivot_wider(id_cols=c(t,region), values_from = value,names_from=quantile)
tmp_mid$region <- toupper(tmp_mid$region)
tmp_mid$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-10-18"),by="day"),each=4)




#### OUT SAMPLE
ny_out_sample <- ny_npz$f$forecast_samples[[1]]$det_prob
ca_out_sample <- ca_npz$f$forecast_samples[[1]]$det_prob
fl_out_sample <- fl_npz$f$forecast_samples[[1]]$det_prob
tx_out_sample <- tx_npz$f$forecast_samples[[1]]$det_prob

dims <- dim(tx_out_sample)
mid_panel_df_total_out <- data.frame(ny=c(t(ny_out_sample)),
                                     fl=c(t(fl_out_sample)),
                                     tx=c(t(tx_out_sample)),
                                     ca=c(t(ca_out_sample)),
                                     t = rep(1:dims[2],dims[1]),
                                     sample=rep(1:dims[1],each=dims[2]))

mid_panel_df_out <- mid_panel_df_total_out %>% group_by(t) %>% mutate(ny_q_u = quantile(ny,probs=c(.975)),
                                                                      fl_q_u = quantile(fl,probs=c(.975)),
                                                                      tx_q_u = quantile(tx,probs=c(.975)),
                                                                      ca_q_u = quantile(ca,probs=c(.975)),
                                                                      ny_q_l = quantile(ny,probs=c(.025)),
                                                                      fl_q_l = quantile(fl,probs=c(.025)),
                                                                      tx_q_l = quantile(tx,probs=c(.025)),
                                                                      ca_q_l = quantile(ca,probs=c(.025))) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample)

mid_panel_df_long_out <- mid_panel_df_out %>% pivot_longer(cols=-t)
mid_panel_df_long_out$region <- substr(mid_panel_df_long_out$name,1,2)
mid_panel_df_long_out$quantile <- substr(mid_panel_df_long_out$name,4,6)
tmp_mid_out <-mid_panel_df_long_out %>% select(-name)%>% distinct() %>% pivot_wider(id_cols=c(t,region), values_from = value,names_from=quantile)
tmp_mid_out$region <- toupper(tmp_mid_out$region)
tmp_mid_out$date <- rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)


### MEDIANS

mid_panel_df_median_out_of_sample <- mid_panel_df_total_out %>% group_by(t) %>% mutate(ny_m = median(ny),
                                                                                       fl_m = median(fl),
                                                                                       tx_m = median(tx),
                                                                                       ca_m = median(ca)) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample) %>% distinct()

mid_panel_df_median_long_out_of_sample <- mid_panel_df_median_out_of_sample%>% pivot_longer(cols=-t,names_to = "region",values_to="median")
mid_panel_df_median_long_out_of_sample$region <- substr(mid_panel_df_median_long_out_of_sample$region,1,2)
mid_panel_df_median_long_out_of_sample$q_u <- NA
mid_panel_df_median_long_out_of_sample$q_l <- NA
mid_panel_df_median_long_out_of_sample$region <- toupper(mid_panel_df_median_long_out_of_sample$region)
mid_panel_df_median_long_out_of_sample$date <- rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)


####
mid_panel_df_median_in_sample <- mid_panel_df_total %>% group_by(t) %>% mutate(ny_m = median(ny),
                                                                               fl_m = median(fl),
                                                                               tx_m = median(tx),
                                                                               ca_m = median(ca)) %>% select(-ny) %>% select(-fl) %>%
  select(-ca) %>% select(-tx) %>% select(-sample) %>% distinct()

mid_panel_df_median_long_in_sample <- mid_panel_df_median_in_sample%>% pivot_longer(cols=-t,names_to = "region",values_to="median")
mid_panel_df_median_long_in_sample$region <- substr(mid_panel_df_median_long_in_sample$region,1,2)
mid_panel_df_median_long_in_sample$q_u <- NA
mid_panel_df_median_long_in_sample$q_l <- NA
mid_panel_df_median_long_in_sample$region <- toupper(mid_panel_df_median_long_in_sample$region)
mid_panel_df_median_long_in_sample$date <- rep(seq(as.Date("2020-03-05"),as.Date("2020-10-18"),by="day"),each=4)





last_panel <- ggplot() + geom_ribbon(data=tmp_mid,aes(x=date,ymax=q_u,ymin=q_l,fill="In Sample"),alpha=.5) +
  geom_ribbon(data=tmp_mid_out,aes(x=date,ymax=q_u,ymin=q_l,fill="Out of Sample"),alpha=.5) +
  geom_line(data=mid_panel_df_median_long_out_of_sample,aes(x=date,y=median),col='blue')+
  geom_line(data=mid_panel_df_median_long_in_sample,aes(x=date,y=median),col='red')+
  facet_wrap(~region,scales='free',nrow=1) + theme_bw() + ylab(expression(p[c][","][t]))
