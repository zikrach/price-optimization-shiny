#----------------------------------------
#Reactive expressions with data subsets
#----------------------------------------
#Subset of data for selected product
product_data <- reactive({
  
  data_excelent %>% subset(UPC == input$po_input_product) %>% arrange(WEEK_NO)
  
})

#Time vector for data subset
visualize_data <- reactive({
  
  data.frame(Week = seq(as.Date("2014/01/09", format="%Y/%m/%d"), by = "w", length.out = 156)[product_data()$WEEK_NO + 1],
             Price = product_data()$BASE_PRICE, 
             Demand = product_data()$UNITS,
             stringsAsFactors = FALSE)
  
})

#----------------------------------------
#Rendering of input elements
#----------------------------------------

output$po_render_price <- renderUI({
  
  sliderInput(inputId = "po_input_price", 
              label = "Price", 
              min = (min(product_data()$BASE_PRICE) / 2), 
              max = (max(product_data()$BASE_PRICE) * 2), 
              value = product_data()$BASE_PRICE[nrow(product_data())],
              step =0.01)
  
})

#----------------------------------------
#VISUALIZATIONS
#----------------------------------------
#Product descriptions
#Description
output$po_box_description <- renderInfoBox({
  
  infoBox(title = "Description", 
          value = as.character(product_data()$DESCRIPTION)[1], 
          icon = icon("file-text")
  )
  
})

#Category
output$po_box_category <- renderInfoBox({
  
  infoBox(title = "Category", 
          value = paste0(as.character(product_data()$CATEGORY)[1], " - ", as.character(product_data()$SUB_CATEGORY)[1]),
          color = "purple",
          icon = icon("object-group")
  )
  
})

#Predicted demand
output$po_box_predicted_demand <- renderValueBox({
  
  valueBox(round(predicted_demand() %>% max_with_zero(), 0), 
           "Expected demand on next week",
          color = "light-blue",
          icon = icon("line-chart")
  )
  
})

#Predicted demand level
output$po_box_predicted_demand_level <- renderInfoBox({
  
  cat <- optimums()$predicted_level
  
  infoBox(title = "Predicted demand level on next week", 
          value = ifelse(cat == 1, "Low", ifelse(cat == 2, "Medium", "High")),
          color = ifelse(cat == 1, "light-blue", ifelse(cat == 2, "blue", "navy")),
          fill = TRUE, 
          icon = icon("bar-chart-o")
  )
  
})

#Demand elasticity
output$po_box_elasticity <- renderInfoBox({
  
  b <- summary(optimums()$optimums$model)$coefficients
  b <- abs(b[which(rownames(b) == "BASE_PRICE"), 1])

  infoBox(title = "Demand", 
          value = ifelse(b < 1, "Inelastic", "Elastic"),
          color = "purple",
          fill = TRUE, 
          icon = icon("bar-chart-o")
  )
  
})

#Current price
output$po_box_current_price <- renderValueBox({
  
  valueBox(input$po_input_price, 
           "Current price",
           color = "light-blue",
           icon = icon("dollar")
  )
  
})

#Current profit
output$po_box_current_profit <- renderValueBox({
  
  valueBox(round(input$po_input_price * max_with_zero(predicted_demand()), 2), 
           "Next week profit based on current price",
           color = "light-blue",
           icon = icon("money")
  )
  
})

#Optimum price
output$po_box_optimum_price <- renderValueBox({
  
  valueBox(round(optimums()$optimums$opt$maximum, 2), 
           "Optimum price",
           color = "olive",
           icon = icon("dollar")
  )
  
})

output$po_box_optimum_demand <- renderValueBox({
  
  valueBox(round(optimums()$optimums$opt$opt_units, 0), 
           "Optimum number of units",
           color = "olive",
           icon = icon("line-chart")
  )
  
})

#Current profit
output$po_box_optimum_profit <- renderValueBox({
  
  valueBox(round(optimums()$optimums$opt$objective, 2), 
           "Next week profit based on optimum price",
           color = "olive",
           icon = icon("money")
  )
  
})

#----------------------------------------
#Historical price
output$price_history <- renderDygraph({
  
  dygraph(as.xts(visualize_data()$Price, order.by = visualize_data()$Week), 
          ylab = "$", 
          group = "history") %>%
    dyOptions(colors = "black")
  
})

#Historical demand
output$demand_history <- renderDygraph({
  
  dygraph(as.xts(visualize_data()$Demand, order.by = visualize_data()$Week), 
          ylab = "Count", 
          group = "history") %>%
    dyOptions(colors = "black")
  
})

#Demands levels visualization
output$demand_levels <- renderDygraph({
  
  periods_steps <- data.table(Demand = visualize_data()$Demand, 
                              Week = visualize_data()$Week, 
                              Demand_level = units_to_factor(x = product_data()$UNITS, data = product_data()))
  
  dygraph(as.xts(periods_steps$Demand_level, order.by = periods_steps$Week),
          ylab = "Demand Level", 
          group = "history") %>%
    dyOptions(stepPlot = TRUE, fillGraph = TRUE)
  
})

#Profit - price
output$po_profit_plot <- renderPlotly({
  
  optimums()$optimums$profit_plot
  
})

#Demand - price
output$po_demand_vs_price_plot <- renderPlotly({
  
  optimums()$optimums$plot
  
})


#----------------------------------------
#Rendering of input elements
#----------------------------------------
#Slider to select price

output$po_render_price <- renderUI({
  
  sliderInput(inputId = "po_input_price", 
              label = "Price", 
              min = (min(product_data()$BASE_PRICE) / 2), 
              max = (max(product_data()$BASE_PRICE) * 2), 
              value = product_data()$BASE_PRICE[nrow(product_data())],
              step =0.01)
  
})

#----------------------------------------
#Elemens updating & disabling
#----------------------------------------
#Update checkboxes
observeEvent(input$po_input_product, {
  
  updateCheckboxInput(session, "po_input_display", value = FALSE)
  updateCheckboxInput(session, "po_input_letter", value = FALSE)
  
  if (optimums()$optimums$vars_predictors[1]) {
    shinyjs::enable("po_input_display")
  } else {
    shinyjs::disable("po_input_display")
  }
  
  if (optimums()$optimums$vars_predictors[2]) {
    shinyjs::enable("po_input_letter")
  } else {
    shinyjs::disable("po_input_letter")
  }
  
  
})


#----------------------------------------
#Calculations
#----------------------------------------
#Predicted demand
optimums <- reactive({
  
  withProgress(message = "", value = 0, style="old", {
    
    ts_prediction <- predict_units_prophet(product_data())$units
    ts_units_category <- units_to_factor(x = ts_prediction, data = product_data())
    
  })
  
  #Optimize price
  return(list(optimums = optimize_one_product_lmer(data = product_data(), factor_spend = ts_units_category, 
                                                   feature = input$po_input_letter, 
                                                   display = input$po_input_display), 
              ts_units_category = ts_units_category,
              predicted_level = units_to_factor(x = ts_prediction, data = product_data())))
  
})

predicted_demand <- reactive({
  
  withProgress(message = "", value = 0, style="old", {
    
    req(!is.null(input$po_input_price))
    
    df <- data.frame(BASE_PRICE = input$po_input_price, 
                     FEATURE = input$po_input_letter, 
                     DISPLAY = input$po_input_display,
                     units_factor = as.character(optimums()$ts_units_category))
    
    predict(optimums()$optimums$model, df)
    
  })
  
})