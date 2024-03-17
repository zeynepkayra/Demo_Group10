library(shiny)
library(ggplot2)
library(plotly)

schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")
table_name <- dbListTables(schema_db)
tables <- list()
for (name in table_name){
  tables[[name]] <- dbReadTable(schema_db, name)
}


server <- function(input,output){
 
  output$demo_data <- DT::renderDataTable({
    product_table
  })
  
  output$dataTables <- DT::renderDataTable({
    switch(input$dataTable,
           "Category Table" = category,
           "Product Table" = product,
           "Ads Table" = ad,
           "Customer Table" = customer,
           "Order Detail Table" = order_detail,
           "Supplier Table" = Suppliers,
           "Transaction Table" = transaction)
  
    
  })
  
  
}

