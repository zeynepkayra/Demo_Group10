library(dplyr)
library(fakir)
library(charlatan)
library(generator)
library(tidyr)
library(RSQLite)

schema_db <- dbConnect(RSQLite::SQLite(), "ECommerce.db")

create_price_view <- '
CREATE VIEW IF NOT EXISTS final_price_of_order AS
SELECT 
    order_detail_id,
    total_price,
    dis,
    (total_price * (1 - (dis * 0.01))) AS discounted_price
FROM (
    SELECT 
        (joint_order.quantity * product.price) AS total_price,
        CAST(SUBSTR(CAST(order_detail.discount AS TEXT), 1, 2) AS INTEGER) AS dis, 
        order_detail.order_detail_id
    FROM 
        product
    JOIN 
        joint_order ON product.product_id = joint_order.product_id
    JOIN 
        order_detail ON order_detail.order_detail_id = joint_order.order_detail_id
) AS subquery
GROUP BY order_detail_id;
'

dbExecute(schema_db, create_price_view)

# Query the view
view_query <- 'SELECT * FROM final_price_of_order'
result <- dbGetQuery(schema_db, view_query)

dbDisconnect(schema_db)



