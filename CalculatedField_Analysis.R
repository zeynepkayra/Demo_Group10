library(dplyr)
library(fakir)
library(charlatan)
library(generator)
library(tidyr)
library(RSQLite)

schema_db <- dbConnect(RSQLite::SQLite(), "ECommerce.db")

create_price_view <- '

CREATE VIEW IF NOT EXISTS final_price_of_order AS select order_detail_id, subtotal, discount, subtotal*(1-discount*0.01) as final_price from 
(select A.order_detail_id, count(A.product_id) as no_of_products, sum(A.total_price_for_each_product) as subtotal, O.discount from 
(select J.order_detail_id, J.product_id, P.price, J.quantity, (P.price* J.quantity) as total_price_for_each_product from joint_order J join product p where J.product_id = P.product_id) A 
join order_detail O on A.order_detail_id = O.order_detail_id group by A.order_detail_id); 


'

dbExecute(schema_db, create_price_view)

# Query the view
view_query <- 'SELECT * FROM final_price_of_order'
result <- dbGetQuery(schema_db, view_query)

dbDisconnect(schema_db)



