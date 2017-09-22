#Libraries
require(shiny)
require(shinyjs)
require(shinydashboard) #version 0.5.3. Last version caused UI bug on startup
require(plotly)
require(DT)
require(dygraphs)
require(xts)

require(data.table)
require(dplyr)
require(plotly)
require(ggplot2)
require(prophet)
require(lme4)

#Load data
load("data/data.RData")

#Source functions
source("functions/function.R")
source("functions/prophet.R")