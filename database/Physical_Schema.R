#title: "Physical_Schema"
#output: html_document
#date: "2024-03-01"

knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(fakir)
library(charlatan)
library(generator)

library(RSQLite)

schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")

create_supplier <- "
  CREATE TABLE supplier (
    supplier_id INTEGER NOT NULL,
    supplier_name VARCHAR(100),
    rating DOUBLE,
    country VARCHAR(55),
    street_name VARCHAR(100),
    house_no INTEGER,
    postcode VARCHAR(9),
    PRIMARY KEY (supplier_id)
  );
"

# Execute the CREATE TABLE statement
dbExecute(schema_db, create_supplier)


create_product <- '
CREATE TABLE product (
  product_id INTEGER NOT NULL,
  product_name VARCHAR(200),
  brand VARCHAR(150),
  price DOUBLE,
  rating DOUBLE,
  availability BOOLEAN,
  supplier_id INTEGER NOT NULL,
  PRIMARY KEY (product_id),
  FOREIGN KEY (supplier_id) REFERENCES supplier(supplier_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_product)

create_customer <- '
CREATE TABLE customer (
  customer_id INTEGER NOT NULL,
  email VARCHAR(150) UNIQUE,
  first_name VARCHAR(150),
  last_name VARCHAR(150),
  mobile_no CHAR(15) UNIQUE,
  credit_card_no CHAR(20),
  country VARCHAR(55),
  street_name VARCHAR(100),
  house_no INTEGER,
  postcode VARCHAR(9),
  PRIMARY KEY (customer_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_customer)


create_ad <- '
CREATE TABLE ad (
  ad_id INTEGER NOT NULL,
  duration INTEGER,
  cost DOUBLE,
  product_id INTEGER,
  PRIMARY KEY (ad_id),
  FOREIGN KEY (product_id) REFERENCES product(product_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_ad)


create_category <- '
CREATE TABLE category (
  category_id INTEGER NOT NULL,
  category_name VARCHAR(150),
  PRIMARY KEY (category_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_category)


create_joint_in <- '
CREATE TABLE joint_in (
  product_id INTEGER NOT NULL,
  category_id INTEGER NOT NULL,
  FOREIGN KEY (product_id) REFERENCES product(product_id)
  FOREIGN KEY (category_id) REFERENCES category(category_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_joint_in)

create_transaction <- '
CREATE TABLE "transaction" (
  transaction_id INTEGER NOT NULL,
  payment_method VARCHAR(30),
  amount DOUBLE,
  order_detail_id INTEGER,
  PRIMARY KEY (transaction_id),
  FOREIGN KEY (order_detail_id) REFERENCES order_detail(order_detail_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_transaction)


create_order_detail <- '
CREATE TABLE order_detail (
  order_detail_id INTEGER NOT NULL,
  quantity INTEGER,
  total_price DOUBLE,
  delivery_date DATE,
  discount DOUBLE,
  transaction_id INTEGER NOT NULL,
  PRIMARY KEY (order_detail_id),
  FOREIGN KEY (transaction_id) REFERENCES "transaction"(transaction_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_order_detail)


create_joint_order <- '
CREATE TABLE joint_order (
  order_detail_id INTEGER NOT NULL,
  customer_id INTEGER NOT NULL,
  product_id INTEGER NOT NULL,
  FOREIGN KEY (order_detail_id) REFERENCES order_detail(order_detail_id)
  FOREIGN KEY (customer_id) REFERENCES customer(customer_id)
  FOREIGN KEY (product_id) REFERENCES product(product_id)
);
'
# Execute the CREATE TABLE statement
dbExecute(schema_db, create_joint_order)


result <- dbGetQuery(schema_db, "SELECT order_detail_id, delivery_date FROM order_detail")
result$delivery_date <- as.Date(result$delivery_date)
print(result)

for (i in 1:nrow(result)) {
  order_detail_id <- result$order_detail_id[i]
  delivery_date <- as.character(result$delivery_date[i])
  
  update_query <- sprintf("
    UPDATE order_detail
    SET delivery_date = '%s'
    WHERE order_detail_id = %d
  ", delivery_date, order_detail_id)
  
  # Execute the update query
  dbExecute(schema_db, update_query)
}

dbDisconnect(schema_db)






