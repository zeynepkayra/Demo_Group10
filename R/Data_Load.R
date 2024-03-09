
#title: "Physical_Schema"
#output: html_document
#date: "2024-03-01"



knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(fakir)
library(charlatan)
library(generator)
library(tidyr)




library(RSQLite)
schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")



# 1.Customers Table

set.seed(4)

# ID
numbers_colon <- sample(110000:210000, 1000, replace = FALSE)

customer <- as_tibble(numbers_colon)
names(customer)[names(customer) == "value"] <- "customer_id"

# Name
person_provider <- PersonProvider$new()

random_first_names <- replicate(1000, person_provider$first_name(), )
random_last_names <- replicate(1000, person_provider$last_name(), )
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
fake_mobile_numbers <- runif(1000, min = 0, max = 1) * (upper_limit - lower_limit) + lower_limit

customer$mobile_no <- paste("+", as.character(round(fake_mobile_numbers)), sep = "")


# Credit Card Number
customer$credit_card_no <- ch_credit_card_number(n = 1000)

# Country
customer$country <- "UK"


# Street Names
street_elements <- c("Maple", "Main", "Oak", "Elm", "Cedar", "High", "Park", "Station", "Green", "Hill", 'Oxford', 'Liverpool', 'Westwood', 'Scarman', 'Gibbet Hill', 'Stoneleigh', 'Earlsdon', 'Lynchgate', 'Centenary', 'New', 'Moore', 'Abberton', 'Davenport', 'Cryfield', 'Lillington', 'Starley', 'Renown', 'Lakewood', 'Glasgow', 'Warwick', 'Stratford', 'Leighton', 'Chelsea')
fake_street_names <- paste(sample(street_elements, 1000, replace = TRUE))

customer$street_name <- fake_street_names

# House Number
house_no <- sample(1:75, 1000, replace = TRUE)
customer$house_no <- house_no

# UK-style postcodes
fake_postcodes <- paste0(sample(LETTERS, 1000, replace = TRUE), sample(LETTERS, 1000, replace = TRUE), sample(0:9, 1000, replace = TRUE), " ", sample(0:9, 1000, replace = TRUE), sample(LETTERS, 1000, replace = TRUE), sample(LETTERS, 1000, replace = TRUE))
customer$postcode <- fake_postcodes

# Adjust the columns to fit database
customer <- customer[, c(1, 4, 2, 3, 5, 6, 7, 8, 9, 10)]

# ------------------

# 2.Suppliers Table

set.seed(5)

# ID
supplier_id <- sample(210001:310000, 30, replace = FALSE)
Suppliers <- as_tibble(supplier_id)
names(Suppliers)[names(Suppliers) == "value"] <- "Supplier_ID"

# Name
supplier_names <- c("Astonville Electronics", 'GlobalTech', 'Estelle', 'Quantin', 'Nexius', 'Oxley', 'Black Horse', 'Zirkon', 'United', 'Velocity Technologies', 'Crescent', 'Starley Mechanics', 'Xander Gray', 'Foster', 'Swifters', 'Red Rabbit', 'Welness House', 'Gary Martin', 'Dirty Duck', 'PJ', 'HealthX', 'Rugby', 'North Warwick Foundation', 'Smart Beta', 'Future World', 'Leicester', 'EcoHealth', 'Venus', 'Boots', 'Maxtra')
abbrev <- c('Group', 'Co.', 'Services')

fake_supplier_names <- replicate(30, paste(sample(supplier_names, 1), sample(abbrev, 1, replace = TRUE), sep = " "))
Suppliers$Supplier_Name <- fake_supplier_names

# Rating
rating <- rgamma(30, shape = 5, rate = 1)
rating <- pmin(rating, 5)
Suppliers$Rating <- round(rating, 1)

# Country
supplier_country <- c("UK", "Ireland")
country_elements <- paste(sample(supplier_country, 30, replace = TRUE))
Suppliers$Country <- country_elements

# Street Name
supplier_streets <- c("Maple", "Main", "Oak", "Elm", "Cedar", "High", "Park", "Station", "Green", "Hill", 'Oxford', 'Liverpool', 'Westwood', 'Scarman', 'Gibbet Hill', 'Stoneleigh', 'Earlsdon', 'Lynchgate', 'Centenary', 'New', 'Moore', 'Abberton', 'Davenport', 'Cryfield', 'Lillington', 'Starley', 'Renown', 'Lakewood', 'Glasgow', 'Warwick', 'Stratford', 'Leighton', 'Chelsea', 'Dublin', 'Galway')
fake_streets <- paste(sample(supplier_streets, 30, replace = TRUE))

Suppliers$Street_Name <- fake_streets

# House No
house_noo <- sample(1:75, 30, replace = TRUE)
Suppliers$House_No <- house_noo

# Postcode
fake_postcodess <- paste0(sample(LETTERS, 30, replace = TRUE), sample(LETTERS, 30, replace = TRUE), sample(0:9, 30, replace = TRUE), " ", sample(0:9, 30, replace = TRUE), sample(LETTERS, 30, replace = TRUE), sample(LETTERS, 30, replace = TRUE))
Suppliers$Postcode <- fake_postcodess

# ------------------

# 3.Products Table

set.seed(6)

product<- fake_products(1000)
category_column <- c(product$category)
product <- product %>% select(-color, -body_location, -sent_from, -category)
names(product)[names(product) == "name"] <- "product_name"
names(product)[names(product) == "price"] <- "price"
names(product)[names(product) == "id"] <- "product_id"
names(product)[names(product) == "brand"] <- "brand"

# Brand
split_brand <- function(x) {
  separated <- separate(data.frame(x), x, into = c("Name1", "Name2"), sep = "[-,]", remove = FALSE, fill = "right")
  separated$Name2 <- ifelse(is.na(separated$Name2), separated$Name1, separated$Name2)
  return(separated)
}
split_data <- split_brand(product$brand)

product <- cbind(product, split_data)
product <- product %>% select(-brand, -x, -Name2)
names(product)[names(product) == "Name1"] <- "brand"

# Rating
ratingg <- rgamma(1000, shape = 5, rate = 1)
ratingg <- pmin(ratingg, 5)
product$rating <- round(ratingg, 1)

# Availability
availability <- c("True", "False")
availability <- paste(sample(availability, 1000, replace = TRUE))
product$availability <- availability
# Supplier id
supplierid_list <- c(Suppliers$Supplier_ID)
product$supplier_id <- sample(supplierid_list, 1000, replace = TRUE)

# Adjust the columns to fit database
product <- product[, c(3, 1, 4, 2, 5, 6, 7)]

# ------------------

# 4.Ads Table

set.seed(7)

# ID
ad_id <- sample(310001:410000, 1000, replace = FALSE)

ad <- as_tibble(ad_id)
names(ad)[names(ad) == "value"] <- "ad_id"

# Duration
duration <- runif(1000, min = 0, max = 1) * 2
ad$duration <- round(duration,2)

# Cost
cost <- runif(1000, min = 0, max = 1) * 50
ad$cost <- round(cost,2)

# product_id
productid_list <- c(product$product_id)
ad$product_id <- sample(productid_list, 1000, replace = FALSE)

# ------------------

# 5.Category Table

set.seed(8)

# ID
category_id <- sample(410001:510000, 8, replace = FALSE)
category <- as_tibble(category_id)
names(category)[names(category) == "value"] <- "category_id"

# Name
namess <- category_column
category$category_name <- unique(namess)


# ------------------

# 7.Transactions Table

set.seed(10)

# ID
transaction_id <- sample(610001:710000, 1000, replace = FALSE)
transaction <- as_tibble(transaction_id)
names(transaction)[names(transaction) == "value"] <- 'transaction_id'

# Payment Method
method <- c('Credit Card', 'Transfer', 'Pay at Door', 'PayPal', 'Debit Card', 'Voucher')
types <- paste(sample(method, 1000, replace = TRUE))
transaction$payment_method <- types

# Amount
price <- rnorm(1000, mean = 505, sd = 60)
rounded_prices <- round(price, 2)
transaction$amount <- rounded_prices


# ------------------



# 6.Order Details Table

set.seed(9)

# ID
order_details_id <- sample(510001:610000, 1000, replace = FALSE)
order_detail <- as_tibble(order_details_id)
names(order_detail)[names(order_detail) == "value"] <- 'order_detail_id'

# Quantity
quantity <- rgamma(1000, shape = 0.5, rate = 0.1) + 1
quantity <- pmin(quantity, 10)

rounded_quantity <- round(quantity)

order_detail$quantity <- rounded_quantity

# Total Price
price <- rnorm(1000, mean = 505, sd = 60)
rounded_prices <- round(price, 2)
order_detail$total_price <- rounded_prices

# Delivery Date
start_date <- as.Date("2024-03-20")
end_date <- as.Date("2024-08-20") 
date_sequence <- seq.Date(start_date, end_date, by = "1 day")

dates <- sample(date_sequence, 1000, replace = TRUE)
order_detail$delivery_date <- dates

# Discount
lower_percentage <- seq(from = 5, to = 45, by = 5)
lower_percentages <- sample(lower_percentage, 750, replace = TRUE)
upper_percentage <- seq(from = 50, to = 75, by = 5)
upper_percentages <- sample(upper_percentage, 250, replace = TRUE)
percentages <- c(lower_percentages, upper_percentages)
order_detail$discount <- paste(percentages, '%', sep = '')

# order_detail_id
orderid_list <- c(order_detail$order_detail_id)
transaction$order_detail_id <- sample(orderid_list, 1000, replace = FALSE)

# transaction_id added as a foreign key

order_detail <- merge(x = order_detail, y = transaction, by = 'order_detail_id') %>% select(-payment_method, -amount)

# ------------------


# Joint table for relation 'In' including category_id and product_id as foreign keys
cat_id <- c(category$category_id)
categories <- sample(cat_id, 1000, replace = TRUE)
prod_id <- c(product$product_id)
joint_in <- tibble(prod_id, categories)

# Joint table for relation 'Order' including order_detail_id, customer_id, and product_id
joint_order <- tibble(order_detail$order_detail_id, customer$customer_id, product$product_id)

names(joint_in)[names(joint_in) == "prod_id"] <- 'product_id'
names(joint_in)[names(joint_in) == "categories"] <- 'category_id'
names(joint_order)[names(joint_order) == "order_detail$order_detail_id"] <- 'order_detail_id'
names(joint_order)[names(joint_order) == "customer$customer_id"] <- 'customer_id'
names(joint_order)[names(joint_order) == "product$product_id"] <- 'product_id'


# ------------------


# # Data Integrity and Quality Check


# Testing for any duplication of primary keys or unique values

duplicate_info <- character()

unique_columns <- list(
  "Customer_ID" = customer$customer_id,
  "Email" = customer$email,
  "Mobile_No" = customer$mobile_no,
  "Supplier_ID" = Suppliers$Supplier_ID,
  "Category_ID" = category$category_id,
  "OrderDetail_ID" = order_detail$order_detail_id,
  "ad_id" = ad$ad_id,
  "product_id" = product$product_id,
  "Transaction_ID" = transaction$transaction_id
)

for (col in names(unique_columns)) {
  has_duplicates <- any(duplicated(unique_columns[[col]]))
  
  if (has_duplicates) {
    duplicate_info <- c(duplicate_info, paste("Column", col, "has duplicate values"))
  }
}

if (length(duplicate_info) > 0) {
  cat("Columns with duplicate values:\n")
  cat(duplicate_info, sep = "\n")
} else {
  print("No primary keys and unique values have duplicate values.")
}



# Formatting Email column in Customers Table
customer$email <- gsub("'", "", customer$email)


## inserting fake data into schema 


# Insert fake data into the supplier table


my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
dbWriteTable(my_db, "supplier", Suppliers, overwrite = TRUE)
dbWriteTable(my_db, "product",product, overwrite= TRUE)
dbWriteTable(my_db, "ad", ad, overwrite= TRUE)
dbWriteTable(my_db, "transaction", transaction, overwrite= TRUE)
dbWriteTable(my_db, "order_detail", order_detail, overwrite= TRUE)
dbWriteTable(my_db, "customer", customer, overwrite= TRUE)
dbWriteTable(my_db, "category", category, overwrite= TRUE)

# Joint tables
dbWriteTable(my_db, "joint_in", joint_in, overwrite= TRUE)
dbWriteTable(my_db, "joint_order", joint_order, overwrite= TRUE)

dbDisconnect(schema_db)