ablation_res <- read.csv("/Users/gcgibson/mech_bayes_paper/summary-states.csv")
ablation_res$mae <- ablation_res$MAE
library(ggplot2)
library(ggthemes)

ablation_res_formatted <- ablation_res[ablation_res$horizon %in% c(6,13,20,27),]

ablation_res_formatted$horizon <- factor(ablation_res_formatted$horizon)
levels(ablation_res_formatted$horizon) <- paste0(1:4, " wk ahead inc death")

ablation_res_formatted$model <- factor(ablation_res_formatted$model)
levels(ablation_res_formatted$model) <- c("Case/Death Time-Varying","Case/Death Fixed","Death Fixed")


ablation_res_formatted %>% group_by(model) %>% summarize(mae=mean((mae)))



ablation_1 <- ggplot(ablation_res_formatted,aes(x=date,y=MAE+1)) + theme_bw() + 
 scale_y_continuous(trans='log10')+ geom_boxplot(aes(col=model)) + ylab("MAE + 1 (Log Scale)") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("/Users/gcgibson/mech_bayes_paper/ablation_1.png",ablation_1,device="png",width=6,height=4)





ablation_2 <- ggplot(ablation_res_formatted,aes(x=horizon,y=MAE+1)) + theme_bw() + 
  scale_y_continuous(trans='log10')+ geom_boxplot(aes(col=model)) + ylab("MAE + 1 (Log Scale)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Target")


ggsave("/Users/gcgibson/mech_bayes_paper/ablation_2.png",ablation_2,device="png",width=6,height=4)




states_to_plot <- c("TX","FL","CA","NY","NJ","IL","PA","MA","AZ","GA","OH","MI","CT","SC","MD","LA","IN","TN","MS","VA")
region_lot_df <-ablation_res_formatted[ablation_res_formatted$place %in% states_to_plot,]
region_lot_df$place <- factor(region_lot_df$place,levels=states_to_plot)


ablation_3 <- ggplot(region_lot_df,aes(x=place,y=MAE+1)) + theme_bw() + 
  scale_y_continuous(trans='log10')+ geom_boxplot(aes(col=model)) + ylab("MAE + 1 (Log Scale)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Target")



ggsave("/Users/gcgibson/mech_bayes_paper/ablation_3.png",ablation_3,device="png",width=8,height=4)















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



