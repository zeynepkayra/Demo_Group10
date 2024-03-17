#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
server <- function(input,output){
    output$diamondsdatatable = DT::renderDataTable(
        diamonds
        
    )
    
    output$pricehist <- renderPlot({
        ggplot(diamonds, aes(y=price,x=carat))+geom_smooth()
        }
    )
    # We can use the plotly package
    output$pricehistplotly <- renderPlotly({
        output <-  ggplot(diamonds, aes(y=price,x=carat))+geom_smooth()
        ggplotly(output)
      }
    )
    
    output$pricehistplotlywithfilter <- renderPlotly({
        to_filter <- input$diamondcut
        ploth <- ggplot(subset(diamonds,cut==to_filter),aes(x=price))+geom_histogram()
        ggplotly(ploth)
    })
    
    
}