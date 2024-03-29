
#title: "Second Phase Data Load"
#output: html_document
#date: "2024-03-01"



library(dplyr)
library(fakir)
library(charlatan)
library(generator)
library(tidyr)
library(RSQLite)

schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")

# Some of the remaining data is generated and then uploaded to the database after validation of data

# 1.Customers Table

# ID
numbers_colon <- sample(110000:210000, 5, replace = FALSE)

customer <- as_tibble(numbers_colon)
names(customer)[names(customer) == "value"] <- "customer_id"

# Name
person_provider <- PersonProvider$new()

random_first_names <- replicate(5, person_provider$first_name(), )
random_last_names <- replicate(5, person_provider$last_name(), )
random_full_names <- paste(random_first_names, random_last_names)

customer$name <- random_full_names
customer$name_modified <- gsub(" ", "_", customer$name)
customer$email <- paste0(customer$name_modified, "@mail.com")
customer <- customer %>%
  separate(name, into = c("First_Name", "Last_Name"), sep = " ")
customer <- customer %>% select(-name_modified)

# 1000 fake phone numbers
lower_limit <- 4400000000000
upper_limit <- 4499999999999
fake_mobile_numbers <- runif(5, min = 0, max = 1) * (upper_limit - lower_limit) + lower_limit

customer$mobile_no <- paste("+", as.character(round(fake_mobile_numbers)), sep = "")

# Credit Card Number
customer$credit_card_no <- ch_credit_card_number(n = 5)

# Country
customer$country <- "UK"

# Street Names
street_elements <- c("Maple", "Main", "Oak", "Elm", "Cedar", "High", "Park", "Station", "Green", "Hill", 'Oxford', 'Liverpool', 'Westwood', 'Scarman', 'Gibbet Hill', 'Stoneleigh', 'Earlsdon', 'Lynchgate', 'Centenary', 'New', 'Moore', 'Abberton', 'Davenport', 'Cryfield', 'Lillington', 'Starley', 'Renown', 'Lakewood', 'Glasgow', 'Warwick', 'Stratford', 'Leighton', 'Chelsea')
fake_street_names <- paste(sample(street_elements, 5, replace = TRUE))

customer$street_name <- fake_street_names

# House Number
house_no <- sample(1:75, 5, replace = TRUE)
customer$house_no <- house_no

# UK-style postcodes
fake_postcodes <- paste0(sample(LETTERS, 5, replace = TRUE), sample(LETTERS, 5, replace = TRUE), sample(0:9, 5, replace = TRUE), " ", sample(0:9, 5, replace = TRUE), sample(LETTERS, 5, replace = TRUE), sample(LETTERS, 5, replace = TRUE))
customer$postcode <- fake_postcodes

# Adjust the columns to fit database
customer <- customer[, c(1, 4, 2, 3, 5, 6, 7, 8, 9, 10)]

# ------------------

# Extracting product_id using SQL query
create_product_id_query <- 'SELECT product_id FROM product;
'
dbExecute(schema_db, create_product_id_query)
product_idd <- dbGetQuery(schema_db, create_product_id_query)
product_idd <- as_tibble(product_idd)

# ------------------

# 7.Transactions Table


# ID
transaction_id <- sample(610001:710000, 10, replace = FALSE)
transaction <- as_tibble(transaction_id)
names(transaction)[names(transaction) == "value"] <- 'transaction_id'

# Payment Method
method <- c('Credit Card', 'Transfer', 'Pay at Door', 'PayPal', 'Debit Card', 'Voucher')
types <- paste(sample(method, 10, replace = TRUE))
transaction$payment_method <- types

# ------------------

# 6.Order Details Table


# ID
order_details_id <- sample(510001:610000, 10, replace = FALSE)
order_detail <- as_tibble(order_details_id)
names(order_detail)[names(order_detail) == "value"] <- 'order_detail_id'

# Delivery Date
start_date <- as.Date("2024-03-20")
end_date <- as.Date("2024-08-20") 
date_sequence <- seq.Date(start_date, end_date, by = "1 day")

dates <- sample(date_sequence, 10, replace = TRUE)
order_detail$delivery_date <- as.character(dates)

# Discount
lower_percentage <- seq(from = 5, to = 45, by = 5)
lower_percentages <- sample(lower_percentage, 7, replace = TRUE)
upper_percentage <- seq(from = 50, to = 75, by = 5)
upper_percentages <- sample(upper_percentage, 3, replace = TRUE)
percentages <- c(lower_percentages, upper_percentages)
order_detail$discount <- paste(percentages, '%', sep = '')

# order_detail_id
orderid_list <- c(order_detail$order_detail_id)
transaction$order_detail_id <- sample(orderid_list, 10, replace = FALSE)

# transaction_id added as a foreign key

order_detail <- merge(x = order_detail, y = transaction, by = 'order_detail_id') %>% select(-payment_method)

# ------------------

order_detail <- order_detail[, c(1, 4, 2, 3)]
transaction <- transaction[, c(1, 3, 2)]

# # Data Integrity and Quality Check

# Formatting Email column in Customers Table
customer$email <- gsub("'", "", customer$email)

# Before inserting, we need to make a check for duplicates by making comparison with the existing database table values

duplicate_check_customer <- 'SELECT customer_id, email, mobile_no, credit_card_no from customer;'
dbExecute(schema_db, duplicate_check_customer)
customer_dup <- dbGetQuery(schema_db, duplicate_check_customer)
customer_dup  <- as_tibble(customer_dup)

duplicate_check_order <- 'SELECT order_detail_id from order_detail;'
dbExecute(schema_db, duplicate_check_order)
order_dup <- dbGetQuery(schema_db, duplicate_check_order)
order_dup  <- as_tibble(order_dup)

duplicate_check_trans <- 'SELECT transaction_id from "transaction";'
dbExecute(schema_db, duplicate_check_trans)
trans_dup <- dbGetQuery(schema_db, duplicate_check_trans)
trans_dup  <- as_tibble(trans_dup)

# Remove any duplicate row/entry
unique_customer <- anti_join(customer, customer_dup, by = 'customer_id')
unique_customer2 <- anti_join(unique_customer, customer_dup, by = 'email')
unique_customer3 <- anti_join(unique_customer2, customer_dup, by = 'credit_card_no')
unique_customer_fin <- anti_join(unique_customer3, customer_dup, by = 'mobile_no')
unique_order <- anti_join(order_detail, order_dup, by = 'order_detail_id')
unique_trans <- anti_join(transaction, trans_dup, by = 'transaction_id')
unique_trans <- semi_join(transaction, unique_order, by = 'transaction_id')

# Keeping record of invalid entries if any
invalid_orders <- anti_join(order_detail, unique_order, by = 'order_detail_id')
invalid_transactions <- anti_join(transaction, unique_trans, by = 'transaction_id')
invalid_customers <- anti_join(customer, unique_customer_fin, by = 'customer_id')

ord <- paste0('The number of invalid order entries ', nrow(invalid_orders))
tra <- paste0('The number of invalid transaction entries are ', nrow(invalid_transactions))
cus <- paste0('The number of invalid customer entries are ', nrow(invalid_customers))

all_messages <- c(ord, tra, cus)
write.table(all_messages, "Invalid_Entries.txt", sep = "\n", row.names = FALSE, col.names = FALSE)


# Joint table for relation 'Order' including order_detail_id, customer_id, and product_id

my_function <- function(n) {
  x_t <- data.frame(order_detail_id = integer(),
                    customer_id = integer(),
                    product_id = integer(),
                    quantity = integer())
  
  for (i in 1:n) {
    ch_order_detail_id <- unique_order$order_detail_id[i]
    ch_customer_id <- sample(unique_customer_fin$customer_id, 1)
    no_of_product <- as.integer(rexp(1, rate = 1/5))
    y <- product_idd
    
    for (j in 1:no_of_product) {
      ch_product_id <- sample(y$product_id, 1, replace = FALSE) 
      quantity <- sample(1:5, 1)
      
      # Append to data frame
      x_t <- rbind(x_t, data.frame(order_detail_id = ch_order_detail_id,
                                   customer_id = ch_customer_id,
                                   product_id = ch_product_id,
                                   quantity = quantity))
    }
  }
  
  return(as_tibble(x_t))
}
new_orders <- my_function(10)

# Rename the columns of joint_order table
joint_order <- distinct(new_orders, order_detail_id, customer_id, product_id, .keep_all = TRUE)
names(joint_order)[names(joint_order) == "order_detail$order_detail_id"] <- 'order_detail_id'
names(joint_order)[names(joint_order) == "customer$customer_id"] <- 'customer_id'
names(joint_order)[names(joint_order) == "product$product_id"] <- 'product_id'

# ------------------


## Inserting fake data into schema 


my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
dbWriteTable(my_db, "transaction", unique_trans, append= TRUE)
dbWriteTable(my_db, "order_detail", unique_order, append= TRUE)
dbWriteTable(my_db, "customer", unique_customer_fin, append= TRUE)

# Joint tables
dbWriteTable(my_db, "joint_order", joint_order, append= TRUE)

# ------------------

# Calculation of Total Price of Orders after Discounts are applied
create_price_view <- '

CREATE VIEW IF NOT EXISTS final_price_of_order AS SELECT order_detail_id, subtotal, discount, subtotal*(1-discount*0.01) AS final_price FROM 
(SELECT A.order_detail_id, COUNT(A.product_id) AS no_of_products, SUM(A.total_price_for_each_product) AS subtotal, O.discount FROM
(SELECT J.order_detail_id, J.product_id, P.price, J.quantity, (P.price* J.quantity) AS total_price_for_each_product FROM joint_order J 
JOIN 
product p WHERE J.product_id = P.product_id) A 
JOIN 
order_detail O ON A.order_detail_id = O.order_detail_id GROUP BY A.order_detail_id);

'
dbExecute(schema_db, create_price_view)

# Exhibit the view and calculated field(Total Price of Order)
view_query <- 'SELECT * FROM final_price_of_order'
view_result <- dbGetQuery(schema_db, view_query)

dbDisconnect(schema_db)