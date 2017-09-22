require(dplyr)
require(prophet)

predict_units_prophet <- function(data = data_clean, period = 1,
                            save_plot=F){
# Input:
#     data - data for predict
#     period - number of days for forecast
#     save_plot - T of F for saving plot of train and predict data
# Output:
#     result=list(data, forecast, plot, units, plot_components), where
#         data - historical (train) data
#         forecast - predict of model
#         plot - plot of train and predict data
#         units - units for next month
#         plot_components - trend and seasonal plots
    
  #prepare data
  data_train <- data %>% 
    group_by(WEEK_NO) %>% 
    summarise(y = sum(UNITS)) %>% mutate(ds = as.Date("2014/01/09", format="%Y/%m/%d") + WEEK_NO * 7) %>% 
    select(-WEEK_NO)
  
  #train and predict model
  m <- prophet(data_train, yearly.seasonality = T, weekly.seasonality = T)
  future <- make_future_dataframe(m, periods = period, freq = 'w')
  forecast <- predict(m, future)
  
  #save plot predict and historical data
  plot_train_forecast <- plot(m, forecast, uncertainty = T, ylabel = 'y_test')
  plot_components <- prophet_plot_components(m, forecast)
  
  return(list(data=data_train, 
              forecast=forecast, 
              plot=plot_train_forecast, 
              units=tail(forecast, 1)$yhat,
              plot_components=plot_components))
}

#testing predict_prophet

# res <- predict_units_prophet(data_excelent %>% filter(UPC == 1111009497))
# res$data
# res$forecast
# res$plot
# res$units


################For all product prophet

# list_product <- data_clean$UPC %>% unique() %>% sort()
# 
# for (arg in list_product){
#   predict_units_prophet(data_clean, arg)
# }
