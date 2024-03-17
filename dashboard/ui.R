library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)


ui <- dashboardPage(skin = "black",
                    
                    # Dashboard Title
                    header = dashboardHeader(title = "Dashboard"),
                    
                    # Sidebar
                    sidebar = dashboardSidebar(
                      sidebarMenu(
                        menuItem(" Home", tabName = "home", icon = icon("home")),
                        menuItem(" Data", tabName = "data", icon = icon("list"))
                      )
                    ),
                    
                    # Body of the dashboard 
                    body = dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "home", 
                                fluidRow(
                                  box(title = "Total Rows", status = "primary"),
                                  box(title = "Total Columns", status = "primary")
                                ),
                                fluidRow(column(width = 6, 
                                                selectInput("dataTable", label = h3("Select Table"), 
                                                            choices = c("Category Table", "Product Table","Ads Table",
                                                                        "Customer Table", "Order Detail Table",
                                                                        "Supplier Table", "Transaction Table"), 
                                                            selected = "category_table")
                                )), 
                                fluidRow(column(width = 12, 
                                                DT::dataTableOutput("dataTables")))
                        ),
                        
                        tabItem(tabName = "data", 
                                fluidRow(
                                  column(h1("Data Page"), width =12),
                                ),
                                fluidRow(
                                  column(
                                    width = 12, 
                                    DT::dataTableOutput("demo_data")
                                  )
                                )
                        )
                      )
                      
                      
                      
                    )
                    
                    
                    
)

















