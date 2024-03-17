#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(title="IB9HP0 Dashboard Demo"), 
    dashboardSidebar(  
    
        sidebarMenu(
            menuItem("Data", tabName = "diamondsdata", icon = icon("dashboard")),
            menuItem("Plots", tabName = "diamondsplots", icon = icon("th")),
            menuItem("Controls", tabName = "diamondplotswithcontrols", icon = icon("filter"))
    
    )), 
    dashboardBody(    
        tabItems(
            # First tab content
            tabItem(tabName = "diamondsdata",
                    fluidRow( h1("Dataset") ), 
                    fluidRow(
                        column(width = 12,
                            DT::dataTableOutput("diamondsdatatable")
                        )
                    )
                    
            ),
            
            # Second tab content
            tabItem(tabName = "diamondsplots",
                    fluidRow(column(width=12, h1("Plots"))), 
                    fluidRow(
                        column(width = 6, 
                                    box(title="Price Histogram",
                                        width=12,
                                        plotOutput("pricehist"))),
                        column(width = 6, 
                                    box(title="Price Histogram Plotly", 
                                        width = 12,
                                        plotlyOutput("pricehistplotly")))
                        
                        )
            ), 
            tabItem(tabName = "diamondplotswithcontrols", 
                    fluidRow(column(width = 12, h1("Plots with controls"))), 
                    fluidRow(column(width = 6, 
                             selectInput("diamondcut", label = h3("Select Cut"), 
                                         choices = list("Fair" = "Fair", "Good"="Good", 
                                                        "Very Good"="Very Good", 
                                                        "Premium"="Premium", 
                                                        "Ideal"="Ideal" 
                                                        ), 
                                         selected = "Ideal")
                             )), 
                    fluidRow(column(width = 12, 
                                    plotlyOutput("pricehistplotlywithfilter")))
            )
        )
    )
)