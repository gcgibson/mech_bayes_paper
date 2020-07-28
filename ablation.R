ablation_res <- read.csv("/Users/gcgibson/mech_bayes_paper/summary-states.csv")
ablation_res$mae <- ablation_res$MAE
library(ggplot2)
ablation_res_subset <- ablation_res[ablation_res$horizon %in% c(6),]

ggplot(ablation_res_subset %>% group_by(forecast_date,model) %>% 
         summarize(mae=mean(mae)),aes(x=forecast_date,y=mae,col=model)) + geom_point() + theme_bw()
