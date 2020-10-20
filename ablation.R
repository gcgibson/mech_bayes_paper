ablation_res <- read.csv("/Users/gcgibson/mech_bayes_paper/summary-states.csv")
ablation_res$mae <- ablation_res$MAE
library(ggplot2)
library(ggthemes)
### 

ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(6),]
ablation_res_subset$model <- as.factor(ablation_res_subset$model)
levels(ablation_res_subset$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")
ablation_res_subset$forecast_date <- as.Date(ablation_res_subset$forecast_date)
  
ablation_1 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
         summarize(se = sqrt(var(mae)),mae_t=mean(mae)),aes(x=as.Date(forecast_date),y=mae_t,col=model)) + geom_point(size=1.5) +theme_bw() + ylab("MAE") + xlab("Date")  + scale_color_brewer(palette="Dark2") + ylim(0,800) + theme(legend.position = "none")  
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_1.png",ablation_1,device="png",width=4,height=6)


ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(13),]
ablation_res_subset$model <- as.factor(ablation_res_subset$model)
levels(ablation_res_subset$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")
ablation_res_subset$forecast_date <- as.Date(ablation_res_subset$forecast_date)


ablation_res_subset$model <- as.factor(ablation_res_subset$model)
levels(ablation_res_subset$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")
ablation_res_subset$forecast_date <- as.Date(ablation_res_subset$forecast_date)


ablation_2 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + 
  theme_bw()+ ylab("MAE") + xlab("Date") +scale_color_brewer(palette="Dark2") + ylim(0,800)
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_2.png",ablation_2,device="png",width=6,height=6)




ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(20),]
ablation_res_subset$model <- as.factor(ablation_res_subset$model)
levels(ablation_res_subset$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")
ablation_res_subset$forecast_date <- as.Date(ablation_res_subset$forecast_date)


ablation_3 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + 
  theme_bw()+ ylab("MAE") + xlab("Date") +scale_color_brewer(palette="Dark2") + ylim(0,800)+ theme(legend.position = "none") 
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_3.png",ablation_3,device="png",width=4,height=6)




ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(27),]
ablation_res_subset$model <- as.factor(ablation_res_subset$model)
levels(ablation_res_subset$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")
ablation_res_subset$forecast_date <- as.Date(ablation_res_subset$forecast_date)



ablation_4 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5)  +
  theme_bw()+scale_color_brewer(palette="Dark2") + ylim(0,800)+ ylab("MAE") + xlab("Date")  
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_4.png",ablation_4,device="png",width=6,height=6)



ablation_calibration <- ggplot(ablation_res %>% group_by(forecast_date,model) %>% 
        summarize(ls=mean(LS)),aes(x=forecast_date,y=ls,col=model)) + geom_point(size=1.5) + theme_bw() +
  scale_color_brewer(palette="Dark2") + ylim(0,800)
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_calibration.png",ablation_calibration,device="png",width=8,height=4)
