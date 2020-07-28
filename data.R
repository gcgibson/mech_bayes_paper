library(stringr)
library(dplyr)
deaths <- read.csv("/Users/gcgibson/Desktop/truth-Incident Deaths.txt")  # Number of students in bed



state_deaths <- deaths %>%
  filter(!str_detect(location_name, "County"))

library(ggplot2)

data_plot <- ggplot(state_deaths,aes(x=date,y=value)) + geom_point(size=.5) + facet_wrap(~location_name,scales="free")
ggsave("/Users/gcgibson/mech_bayes_paper/data_plot.png",data_plot,device="png",width=10,height=10)
