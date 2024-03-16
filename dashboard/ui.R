
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(title="IB9HP0 Dashboard Demo"), 
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