library(reticulate)
np <- import("numpy")

### read numpy files

ny_npz <- np$load("2020-09-06/samples/NY.npz",allow_pickle = T)
tx_npz <- np$load("2020-09-06/samples/TX.npz",allow_pickle = T)
ca_npz <- np$load("2020-09-06/samples/CA.npz",allow_pickle = T)
fl_npz <- np$load("2020-09-06/samples/FL.npz",allow_pickle = T)
library(tidyverse)

## get observed data
obs <- covidHubUtils::load_truth("JHU", "inc death") %>%
  dplyr::rename(true_value = value) %>%
  dplyr::select(-model)


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
##
tmp_out_of_sample$date <-rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)
tmp$date <- rep(seq(as.Date("2020-03-04"),as.Date("2020-10-18"),by="day"),each=4)
top_panel_df_median_long$date <-  rep(seq(as.Date("2020-03-04"),as.Date("2020-10-18"),by="day"),each=4)
top_panel_df_median_long_out_of_sample$date <- rep(seq(as.Date("2020-10-19"),as.Date("2020-11-15"),by="day"),each=4)


ggplot() + geom_ribbon(data=tmp_out_of_sample,aes(x=t,ymax=q_u,ymin=q_l,fill="Out of Sample"),alpha=.4) +
  geom_ribbon(data=tmp,aes(x=t,ymax=q_u,ymin=q_l,fill="In Sample"),alpha=.4) +
   geom_line(data=top_panel_df_median_long,aes(x=t,y=median),col='red')+
  geom_line(data=top_panel_df_median_long_out_of_sample,aes(x=t,y=median),col='blue')+
   facet_wrap(~region,scales="free") + theme_bw()  

       
