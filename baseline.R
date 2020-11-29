library(magrittr)
obs <- covidHubUtils::load_truth("JHU", "inc death") %>%
  dplyr::rename(true_value = value) %>%
  dplyr::select(-model)
dates <- seq(as.Date("2020-05-11"),as.Date("2020-10-19"),by="week")

mb_forecasts <- covidHubUtils::load_forecasts(models = c("UMass-MechBayes"),
                                           forecast_date = dates-1, 
                                           types = "point")





baseline_forecasts <- covidHubUtils::load_forecasts(models = c("COVIDhub-baseline"),
                                              forecast_date = dates, 
                                              types = "point")


ggplot(baseline_forecasts[baseline_forecasts$type == "point",],aes(x=location,y=forecast_date)) + geom_point()
