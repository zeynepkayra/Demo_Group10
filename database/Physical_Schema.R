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

# Connecting to database
schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")


# Creating tables

create_supplier <- '
  CREATE TABLE IF NOT EXISTS "supplier" (
    "supplier_id" INTEGER PRIMARY KEY,
    "supplier_name" VARCHAR(100),
    "rating" DOUBLE,
    "country" VARCHAR(55),
    "street_name" VARCHAR(100),
    "house_no" INTEGER,
    "postcode" VARCHAR(9)
  );
'

dbExecute(schema_db, create_supplier)


create_product <- '
CREATE TABLE IF NOT EXISTS "product" (
  "product_id" INTEGER PRIMARY KEY,
  "supplier_id" INTEGER,
  "product_name" VARCHAR(200),
  "brand" VARCHAR(150),
  "price" DOUBLE,
  "rating" DOUBLE,
  "availability" TEXT,
  FOREIGN KEY ("supplier_id") REFERENCES supplier("supplier_id")
);
'

dbExecute(schema_db, create_product)

create_customer <- '
CREATE TABLE IF NOT EXISTS "customer" (
  "customer_id" INTEGER PRIMARY KEY,
  "email" VARCHAR(150) UNIQUE,
  "first_name" VARCHAR(150),
  "last_name" VARCHAR(150),
  "mobile_no" CHAR(15) UNIQUE,
  "credit_card_no" CHAR(20) UNIQUE,
  "country" VARCHAR(55),
  "street_name" VARCHAR(100),
  "house_no" INTEGER,
  "postcode" VARCHAR(9)
);
'

dbExecute(schema_db, create_customer)


create_ad <- '
CREATE TABLE IF NOT EXISTS "ad" (
  "ad_id" INTEGER PRIMARY KEY,
  "product_id" INTEGER,
  "duration" INTEGER,
  "cost" DOUBLE,
  FOREIGN KEY ("product_id") REFERENCES product("product_id")
);
'

dbExecute(schema_db, create_ad)


create_category <- '
CREATE TABLE IF NOT EXISTS "category" (
  "category_id" INTEGER PRIMARY KEY,
  "category_name" VARCHAR(150)
);
'

dbExecute(schema_db, create_category)


create_joint_in <- '
CREATE TABLE IF NOT EXISTS "joint_in" (
  "product_id" INTEGER,
  "category_id" INTEGER,
  FOREIGN KEY ("product_id") REFERENCES product("product_id"),
  FOREIGN KEY ("category_id") REFERENCES category("category_id")
);
'

dbExecute(schema_db, create_joint_in)

create_transaction <- '
CREATE TABLE IF NOT EXISTS "transaction" (
  "transaction_id" INTEGER PRIMARY KEY,
  "order_detail_id" INTEGER,
  "payment_method" VARCHAR(30),
  FOREIGN KEY ("order_detail_id") REFERENCES order_detail("order_detail_id")
);
'

dbExecute(schema_db, create_transaction)


create_order_detail <- '
CREATE TABLE IF NOT EXISTS "order_detail" (
  "order_detail_id" INTEGER PRIMARY KEY,
  "transaction_id" INTEGER NOT NULL,
  "delivery_date" DATE,
  "discount" DOUBLE,
  FOREIGN KEY ("transaction_id") REFERENCES "transaction"("transaction_id")
);
'

dbExecute(schema_db, create_order_detail)


create_joint_order <- '
CREATE TABLE IF NOT EXISTS "joint_order" (
  "order_detail_id" INTEGER,
  "customer_id" INTEGER,
  "product_id" INTEGER,
  "quantity" INTEGER,
  FOREIGN KEY ("order_detail_id") REFERENCES order_detail("order_detail_id"),
  FOREIGN KEY ("customer_id") REFERENCES customer("customer_id"),
  FOREIGN KEY ("product_id") REFERENCES product("product_id")
);
'


dbExecute(schema_db, create_joint_order)

dbDisconnect(schema_db)






