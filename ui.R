#Function to add nice collapse in menu sidebar
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

#------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(id="sbmenu",
              useShinyjs(),
              #Price optimization
              convertMenuItem(
                menuItem(text = "Price optimization", tabName = "price_optimization", icon = icon("money"),
                         
                         selectInput(inputId = "po_input_product", 
                                     label = "Select product ID",
                                     choices = excelent_prods),
                         
                         uiOutput("po_render_price"),
                         
                         div(
                           h4("Marketing options", id = "custom_header"),
                           checkboxInput(inputId = "po_input_display", label = "Display on TV in store"),
                           checkboxInput(inputId = "po_input_letter", label = "Print in promo-letters")
                         )
                         
                ), "price_optimization"
              )
  )
)

#------------------------------------------------------
body <- dashboardBody(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css")
  ),
  
  tabItems(
    tabItem(tabName = "price_optimization",
            
            box(title = NULL,
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                fluidRow(
                  valueBoxOutput("po_box_description", width = 6),
                  valueBoxOutput("po_box_category", width = 6)
                ),
                fluidRow(
                  valueBoxOutput("po_box_predicted_demand_level", width = 6),
                  valueBoxOutput("po_box_elasticity", width = 6)
                ),
                
                fluidRow(
                  valueBoxOutput("po_box_current_price", width = 4),
                  valueBoxOutput("po_box_predicted_demand", width = 4),
                  valueBoxOutput("po_box_current_profit", width = 4)
                ),
                fluidRow(
                  valueBoxOutput("po_box_optimum_price", width = 4),
                  valueBoxOutput("po_box_optimum_demand", width = 4),
                  valueBoxOutput("po_box_optimum_profit", width = 4)
                )
            ),
            
            fluidRow(
              box(title = "Historical data",
                  status = "primary",
                  solidHeader = FALSE, 
                  width = 12,
                  box(title = "Price",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      dygraphOutput("price_history")
                  ),
                  box(title = "Demand",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      verticalLayout( 
                        dygraphOutput("demand_history", height = 300),
                        dygraphOutput("demand_levels", height = 100)
                      )
                      
                  )
              )
            ),
            
            fluidRow(
              box(title = "Profit vs. price",
                  status = "primary",
                  solidHeader = FALSE, 
                  width = 6,
                  plotlyOutput("po_profit_plot")
              ),
              box(title = "Demand vs. price",
                  status = "primary",
                  solidHeader = FALSE, 
                  width = 6,
                  plotlyOutput("po_demand_vs_price_plot")
              )
            )
            
            
    )
  )
)

dashboardPage(
  skin = "black",
  dashboardHeader(title = "Analytics Prototype"),
  sidebar,
  body
)