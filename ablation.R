ablation_res <- read.csv("/Users/gcgibson/mech_bayes_paper/summary-states.csv")
ablation_res$mae <- ablation_res$MAE
library(ggplot2)
ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(6),]

ablation_1 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
         summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + theme_bw()
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_1.png",ablation_1,device="png",width=6,height=6)


ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(13),]

ablation_2 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + theme_bw()
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_2.png",ablation_2,device="png",width=6,height=6)




ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(20),]

ablation_3 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + theme_bw()
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_3.png",ablation_3,device="png",width=6,height=6)




ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(27),]

ablation_4 <- ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
                       summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point(size=1.5) + theme_bw()
ggsave("/Users/gcgibson/mech_bayes_paper/ablation_4.png",ablation_4,device="png",width=6,height=6)




