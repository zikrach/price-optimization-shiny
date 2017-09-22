require(ggplot2)
require(dplyr)
require(data.table)
require(Matrix)
require(lme4)

max_with_zero <- function(x){
  #Input:
  # x - numeric
  #Output:  max(x, 0) 
  
  if (x > 0){
    return(x)
  } else {
    return(0)
  }
}

units_to_factor <- function(x, data){
  #Input:
  # x - numeric
  # data - data for one product
  #Output:  factor level of x 
  
  qnt <- quantile(data$UNITS,seq(0,1,1/3))
  if ((x > qnt)[4]){
    return(3)
  } else if((x <= qnt)[1]){
    return(1)
  } else{
    return(cut(x, unique(qnt), 
             include.lowest=TRUE, labels = F))}
}

# data <- data_good %>% filter(UPC == 1111009507)
# data$UNITS
# units_to_factor(50, data)


profit_unit <- function(coef, price, factor_spend, feature, display, vars_predictors){
#Input:
  # coef - coefficient fromm linear model
  # price - price for product
  # factor_spend - level of factor units
  # feature - FUATURE
  # display - DISPLAY
  # vars_predictors - True or False if DISPLAY and FEATURE we use for model
#Output:  number units for price (predicted from lm)

  if (sum(vars_predictors) == 2){
    unit_pred <- coef[factor_spend, 1] + coef[factor_spend, 2] * price + coef[factor_spend, 3] * display + coef[factor_spend, 4] * feature
  } else if(sum(vars_predictors) == 1 & vars_predictors[1]){
    unit_pred <- coef[factor_spend, 1] + coef[factor_spend, 2] * price + coef[factor_spend, 3] * display
  } else if(sum(vars_predictors) == 1 & vars_predictors[2]){
    unit_pred <- coef[factor_spend, 1] + coef[factor_spend, 2] * price + coef[factor_spend, 3] * feature
  } else {
    unit_pred <- coef[factor_spend, 1] + coef[factor_spend, 2] * price
  }

  return(unit_pred)
}


profit <- function(coef, price, factor_spend, feature, display, vars_predictors){
  #Input:
  # coef - coefficient fromm linear model
  # price - price for product
  # factor_spend - level of factor units
  # feature - FUATURE
  # display - DISPLAY
  # vars_predictors - True or False if DISPLAY and FEATURE we use for model
  #Output:
  # spend for number units (predicted from lm) and price
  
  unit_pred <- profit_unit(coef, price, factor_spend, feature, display, vars_predictors)
  return(unit_pred * price)
}



optimize_one_product_lmer <- function(data, factor_spend, 
                                      feature, display){
  # Input:
  #     data - data for optimize
  #     factor_spend - level of factor UNITS for optimize
  #     feature - FUATURE
  #     display - DISPLAY
  # Output:
  #     result=list(data_hist, data_lm, opt), where
  #         data_hist - historical (train) data
  #         opt - optimize result
  #         model - linear model
  #         plot - scatter plot with line from model 
  #         vars_predictors - True or False if DISPLAY and FEATURE we use for model
  #         good_factor_units - True or False if correlation between UNITS and BASE_PRICE is positive for all levels of unit_factor
  #         profit_plot - plotly for profit based on price

  
  #prepare data
  data_hist <- data %>% 
    mutate(avg_price = (PRICE - BASE_PRICE) / BASE_PRICE) 
  
  data_hist_clean <- data_hist %>% select(-VISITS, -HHS, -SPEND, -PRICE)
  
  #select predictors
  vars <- sapply(data_hist_clean, function(x){
    result <- x %>% unique() %>% length()
    result > 1
  })
  vars_predictors <- sapply(c('DISPLAY', 'FEATURE'), function(x){
    result <- data_hist_clean[[x]] %>% unique() %>% length()
    result > 1
  })
  
  train <- data_hist_clean[, vars]
  
  #fit and predict linear model
  
  if (sum(vars_predictors) == 2){
    fit2 <- lmer(UNITS ~ BASE_PRICE + DISPLAY + FEATURE  + (0 + BASE_PRICE|units_factor), train)
  } else if(sum(vars_predictors) == 1 & vars_predictors[1]){
    fit2 <- lmer(UNITS ~ BASE_PRICE + DISPLAY  + (0 + BASE_PRICE|units_factor), train)
  } else if(sum(vars_predictors) == 1 & vars_predictors[2]){
    fit2 <- lmer(UNITS ~ BASE_PRICE + FEATURE  + (0 + BASE_PRICE|units_factor), train)
  } else {
    fit2 <- lmer(UNITS ~ BASE_PRICE + (0 + BASE_PRICE|units_factor), train)
  }

  train$preds_fit2 <- predict(fit2) 
  coef_model <- coefficients(fit2)$units_factor
  
  #plot linear model
  data_plot_lm <- train %>% filter(units_factor == factor_spend) %>% select(UNITS, BASE_PRICE) %>% 
    mutate(preds = coef_model[factor_spend, 1] + BASE_PRICE * coef_model[factor_spend, 2])
  
  plot_lm <- plot_ly(data_plot_lm, x = ~BASE_PRICE, y = ~UNITS, type = 'scatter', mode = "markers") %>% 
    add_trace(data_plot_lm, x = ~BASE_PRICE, y = ~preds, type = "scatter", mode = "lines", 
              line = list(width = 2)) %>% 
    layout(xaxis = list(title = "Price"),
           yaxis = list (title = "Units"),
           showlegend = FALSE)
  
  

  #optimize price for maximal profit
  opt <- optimize(profit, coef=coef_model, factor_spend=factor_spend,
                  feature=feature, display=display, vars_predictors=vars_predictors,
                  interval = c(quantile(train$BASE_PRICE, .1), 
                               quantile(train$BASE_PRICE, .9)), maximum = T)
  opt$opt_units <- profit_unit(coef_model, opt$maximum, factor_spend, 
                               feature, display, vars_predictors)
  
  #prepare data for plot profit dependent on price
  profit_x <- seq(min(train$BASE_PRICE) * 0.1, max(train$BASE_PRICE) * 50, 0.01)
  profit_y <- sapply(profit_x, 
                     function(x){
                       profit(coef=coef_model, price = x, factor_spend=factor_spend,
                              feature=feature, display=display, vars_predictors=vars_predictors)
                     })
  
  data_plot <- data.frame(x=profit_x, y=profit_y) %>% filter(y > 0)

  opt_point <- list(
    x = opt$maximum,
    y = opt$objective,
    text = "Optimal point",
    xref = "x",
    yref = "y",
    #showarrow = TRUE,
    #arrowhead = 7,
    ax = 30,
    ay = -30
  )
  
  #plot profit depended on price via plotly
  profit_plot <- plot_ly(data_plot, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>% 
    add_trace(x = c(opt$maximum, opt$maximum), y= c(0, opt$objective), 
              mode = "lines", line=list(dash='dot', width=2, color='blue')) %>% 
    add_trace(x = c(0, opt$maximum), y= c(opt$objective, opt$objective), 
              mode = "lines", line=list(dash='dot', width=2, color='blue')) %>% 
    layout(annotations = opt_point,
           xaxis = list(title = "Price"),
           yaxis = list (title = "Profit"),
           showlegend = FALSE,
           shapes = list(
             list(type = "rect",
                  fillcolor = "lightgrey", line = list(color = "lightgrey"), opacity = 0.3,
                  x0 = quantile(train$BASE_PRICE, .1), x1 = quantile(train$BASE_PRICE, .9), 
                  xref = "x",
                  y0 = 0, y1 = max(data_plot$y) * 1.2, yref = "y")
             ))
  
  
  return(list(data_hist=train,
              opt=opt,
              model=fit2,
              plot=plot_lm,
              vars_predictors=vars_predictors,
              good_factor_units=coefficients(fit2)$units_factor$BASE_PRICE < 0,
              profit_plot=profit_plot))
}