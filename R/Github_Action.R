library(DBI)
library(dplyr)
library(RSQLite)

# Running data validation

# 1. Customer Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
customer <- dbReadTable(my_db, "customer")

# Customer ID
validate_customer_id <- function(customer_id) {
  ifelse(grepl("^\\d{6}$", customer_id), TRUE, FALSE)
}

# Email format 
validate_email <- function(email) {
  email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  ifelse(grepl(email_pattern, email), TRUE, FALSE)
}

# Mobile number format
validate_phone <- function(phone) {
  phone_pattern <- "^\\+?[1-9]\\d{1,14}$"
  ifelse(grepl(phone_pattern, phone), TRUE, FALSE)
}


# UK postcode format
validate_uk_postcode <- function(postcode) {
  postcode_pattern <- "^([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9][A-Za-z]?))))\\s?[0-9][A-Za-z]{2})$"
  ifelse(grepl(postcode_pattern, postcode), TRUE, FALSE)
}


# Last Name format (only letters)
validate_last_name <- function(name) {
  ifelse(grepl("^[A-Za-z]+(['-][A-Za-z]+)*$", name), TRUE, FALSE)
}

# First Name format (only letters)
validate_first_name <- function(name) {
  ifelse(grepl("^[A-Za-z]+(['-][A-Za-z]+)*$", name), TRUE, FALSE)
}

# Credit card number (only check the length)
validate_credit_card_no <- function(credit_card_no) {
  valid_length <- nchar(as.character(credit_card_no)) >= 13 & nchar(as.character(credit_card_no)) <= 19
  return(valid_length)
}


# Street name (without number)
validate_street_name <- function(street_name) {
  ifelse(grepl("^[A-Za-z0-9]+([ '-][A-Za-z0-9]+)*$", street_name), TRUE, FALSE)
}

# House No
validate_house_no <- function(house_no) {
  ifelse(grepl("^\\d+[A-Za-z]?$", house_no), TRUE, FALSE)
}

# Country (only UK)
validate_country_uk <- function(country) {
  ifelse(toupper(country) == "UK", TRUE, FALSE)
}


# calculate invalid data
invalid_counts_customer <- data.frame(
  invalid_customer_id = sum(!sapply(customer$customer_id, validate_customer_id)),
  invalid_emails = sum(!sapply(customer$email, validate_email)),
  invalid_first_names = sum(!sapply(customer$First_Name, validate_first_name)),
  invalid_last_names = sum(!sapply(customer$Last_Name, validate_last_name)),
  invalid_phones = sum(!sapply(customer$mobile_no, validate_phone)),
  invalid_credit_card_nos = sum(!sapply(customer$credit_card_no, validate_credit_card_no)),
  invalid_countries_uk = sum(!sapply(customer$country, validate_country_uk)),
  invalid_street_names = sum(!sapply(customer$street_name, validate_street_name)),
  invalid_house_nos = sum(!sapply(customer$house_no, validate_house_no)),
  invalid_postcodes = sum(!sapply(customer$postcode, validate_uk_postcode))
)

print(invalid_counts_customer)


dbDisconnect(my_db)


# 2. Supplier Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
supplier <- dbReadTable(my_db, "supplier")

# Supplier ID
validate_supplier_id <- function(Supplier_ID) {
  ifelse(grepl("^\\d{6}$", Supplier_ID), TRUE, FALSE)
}

# Supplier Name
validate_supplier_name <- function(Supplier_Name) {
  name_pattern <- "^[A-Za-z0-9]+([ '.-][A-Za-z0-9]+)*$"
  ifelse(grepl(name_pattern, Supplier_Name), TRUE, FALSE)
}

# Rating
validate_rating <- function(Rating) {
  if(!is.numeric(Rating)) {
    return(rep(FALSE, length(Rating)))  
  }
  
  result <- (Rating >= 0.0 & Rating <= 5.0) & (floor(Rating * 10) == (Rating * 10))
  return(result)
}

# Country
validate_country <- function(Country) {
  country_pattern <- "^[A-Za-z ]+$"
  ifelse(grepl(country_pattern, Country), TRUE, FALSE)
}


# Street Name
validate_street_name <- function(Street_Name) {
  ifelse(grepl("^[A-Za-z0-9]+([ '-][A-Za-z0-9]+)*$", Street_Name), TRUE, FALSE)
}

# House No
validate_house_no <- function(House_No) {
  ifelse(grepl("^\\d+[A-Za-z]?$", House_No), TRUE, FALSE)
}

# Postcode
validate_postcode <- function(Postcode) {
  postcode_pattern <- "^([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9][A-Za-z]?))))\\s?[0-9][A-Za-z]{2})$"
  ifelse(grepl(postcode_pattern, Postcode), TRUE, FALSE)
}


# calculate invalid data
invalid_counts_supplier <- data.frame(
  invalid_supplier_ids = sum(!sapply(supplier$Supplier_ID, validate_supplier_id)),
  invalid_supplier_names = sum(!sapply(supplier$Supplier_Name, validate_supplier_name)),
  invalid_ratings = sum(!sapply(supplier$Rating, validate_rating)),
  invalid_countries = sum(!sapply(supplier$Country, validate_country)),
  invalid_street_names = sum(!sapply(supplier$Street_Name, validate_street_name)),
  invalid_house_nos = sum(!sapply(supplier$House_No, validate_house_no)),
  invalid_postcodes = sum(!sapply(supplier$Postcode, validate_postcode))
)


print(invalid_counts_supplier)

dbDisconnect(my_db)


# 3. Ad Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
ad <- dbReadTable(my_db, "ad")

# ad ID
validate_ad_id <- function(ad_id) {
  ifelse(grepl("^\\d{6}$", ad_id), TRUE, FALSE)  
}

# product ID
validate_product_id <- function(product_id) {
  ifelse(product_id >= 1 & product_id <= 1000, TRUE, FALSE)
}

# duration
validate_duration <- function(duration) {
  is.numeric(duration)
}

# cost
validate_cost <- function(cost) {
  is.numeric(cost)
}


# calculate invalid data
invalid_counts_ad <- data.frame(
  invalid_ad_ids = sum(!sapply(ad$ad_id, validate_ad_id)),
  invalid_product_ids = sum(!sapply(ad$product_id, validate_product_id)),
  invalid_durations = sum(!sapply(ad$duration, validate_duration)),
  invalid_costs = sum(!sapply(ad$cost, validate_cost))
)

print(invalid_counts_ad)

dbDisconnect(my_db)

# 4. Category Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
category <- dbReadTable(my_db, "category")

# Category ID
validate_category_id <- function(category_id) {
  ifelse(grepl("^\\d{6}$", category_id), TRUE, FALSE)
}

# Category Name 
validate_category_name <- function(category_name) {
  ifelse(grepl("^[A-Za-z ]+$", category_name), TRUE, FALSE)
}

# calculate invalid data
invalid_counts_category <- data.frame(
  invalid_category_ids = sum(!sapply(category$Category_ID, validate_category_id)),
  invalid_category_names = sum(!sapply(category$Category_Name, validate_category_name))
)

print(invalid_counts_category)

dbDisconnect(my_db)


# 5. Transaction Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
transaction <- dbReadTable(my_db, "transaction")

# Transaction ID
validate_transaction_id <- function(transaction_id) {
  ifelse(grepl("^\\d{6}$", transaction_id), TRUE, FALSE)
}

# Order detail ID
validate_order_detail_id <- function(order_detail_id) {
  ifelse(grepl("^\\d{6}$", order_detail_id), TRUE, FALSE)
}

# Payment method
validate_payment_method <- function(payment_method) {
  ifelse(grepl("^[A-Za-z ]+$", payment_method), TRUE, FALSE)
}

# calculate invalid data
invalid_counts_transaction <- data.frame(
  invalid_transaction_ids = sum(!sapply(transaction$Transaction_ID, validate_transaction_id)),
  invalid_order_detail_ids = sum(!sapply(transaction$Order_Detail_ID, validate_order_detail_id)),
  invalid_payment_methods = sum(!sapply(transaction$Payment_Method, validate_payment_method))
)

print(invalid_counts_transaction)

dbDisconnect(my_db)



# 6. Order Details Table

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
order_detail <- dbReadTable(my_db, "order_detail")

# Order detail ID
validate_order_detail_id <- function(order_detail_id) {
  ifelse(grepl("^\\d{6}$", order_detail_id), TRUE, FALSE)
}

# transaction ID
validate_transaction_id <- function(transaction_id) {
  ifelse(grepl("^\\d{6}$", transaction_id), TRUE, FALSE)
}

# delivery date
validate_delivery_date <- function(delivery_date) {
  tryCatch(!is.na(as.Date(delivery_date, format = "%Y-%m-%d")), 
           error = function(e) FALSE)
}

# discount
validate_discount <- function(discount) {
  if(!is.numeric(discount)) {
    return(FALSE)
  }
  ifelse(discount >= 0 & discount <= 100, TRUE, FALSE)
}

# calculate invalid data
invalid_counts_order_detail <- data.frame(
  invalid_order_detail_ids = sum(!sapply(order_detail$Order_Detail_ID, validate_order_detail_id)),
  invalid_transaction_ids = sum(!sapply(order_detail$Transaction_ID, validate_transaction_id)),
  invalid_delivery_dates = sum(!sapply(order_detail$Delivery_Date, validate_delivery_date)),
  invalid_discounts = sum(!sapply(order_detail$Discount, validate_discount))
)

print(invalid_counts_order_detail)

dbDisconnect(my_db)



