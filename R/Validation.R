library(DBI)
library(dplyr)
library(RSQLite)

# Running data validation

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")

# 1. Customer Table

customer <- dbReadTable(my_db, "customer")


# 1. Customer Table

# Customer ID (6-digit)
validate_customer_id <- function(customer_id) {
  ifelse(grepl("^\\d{6}$", customer_id), TRUE, FALSE)
}

# Email format (xxx@xxx.xxx)
validate_email <- function(email) {
  email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.]+\\.com$"
  ifelse(grepl(email_pattern, email), TRUE, FALSE)
}

# Mobile number format (+xxxxxxxxxxxxxx)
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
  ifelse(grepl("^[A-Za-z]+([ '-][A-Za-z]+)*$", street_name), TRUE, FALSE)
}

# House No (number+letter)
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

# duplicates 


customer_duplicates <- dbGetQuery(my_db, "SELECT customer_id, count(*) from customer group by customer_id having count(*) > 1")
 print(paste("No:of duplicates for customer table is ",nrow(customer_duplicates)))




# 2. Supplier Table

supplier <- dbReadTable(my_db, "supplier")

# Supplier ID (6-digit)
validate_supplier_id <- function(Supplier_ID) {
  ifelse(grepl("^\\d{6}$", Supplier_ID), TRUE, FALSE)
}

# Supplier Name (only letters)
validate_supplier_name <- function(Supplier_Name) {
  name_pattern <- "^[A-Za-z]+([ '.-][A-Za-z]+)*\\.?$"  
  ifelse(grepl(name_pattern, Supplier_Name), TRUE, FALSE)
}



# Rating (0.0-5.0)

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
  ifelse(grepl("^[A-Za-z]+([ '-][A-Za-z]+)*$", Street_Name), TRUE, FALSE)
}

# House No (number+letter)
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

# duplicates 

supplier_duplicates <- dbGetQuery(my_db, "SELECT supplier_id, count(*) from supplier group by supplier_id having count(*) > 1")
print(paste("No:of duplicates for supplier table is ",nrow(supplier_duplicates)))


# 3. Ad Table

ad <- dbReadTable(my_db, "ad")


# ad ID (6-digit)
validate_ad_id <- function(ad_id) {
  ifelse(grepl("^\\d{6}$", ad_id), TRUE, FALSE)  
}

# product ID (1-1000)
validate_product_id <- function(product_id) {
  ifelse(product_id >= 1 & product_id <= 1000, TRUE, FALSE)
}

# duration (ad duration can be 0 to 10 minutes)
validate_duration <- function(duration) {
  if(!is.numeric(duration)) {
    return(FALSE)
  }
  ifelse(duration >= 0 && duration <= 10 && floor(duration * 100) == (duration * 100), TRUE, FALSE)
}


# cost 
validate_cost <- function(cost) {
  if(!is.numeric(cost)) {
    return(FALSE)
  }
  ifelse(cost >= 0 && cost <= 1000, TRUE, FALSE)
}

# duration (numeric)
validate_duration <- function(duration) {
  is.numeric(duration)
}

# cost (numeric)
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


# duplicates 

ad_duplicates <- dbGetQuery(my_db, "SELECT ad_id, count(*) from ad group by ad_id having count(*) > 1")
print(paste("No:of duplicates for ad table is ",nrow(ad_duplicates)))



# 4. Category Table

category <- dbReadTable(my_db, "category")

# Category ID (6-digit)
validate_category_id <- function(category_id) {
  ifelse(grepl("^\\d{6}$", category_id), TRUE, FALSE)
}

# Category Name (only letters)
validate_category_name <- function(category_name) {
  ifelse(grepl("^[A-Za-z ]+$", category_name), TRUE, FALSE)
}

# calculate invalid data
invalid_counts_category <- data.frame(
  invalid_category_ids = sum(!sapply(category$Category_ID, validate_category_id)),
  invalid_category_names = sum(!sapply(category$Category_Name, validate_category_name))
)

print(invalid_counts_category)


# duplicates 

category_duplicates <- dbGetQuery(my_db, "SELECT category_id, count(*) from category group by category_id having count(*) > 1")
print(paste("No:of duplicates for category table is ",nrow(category_duplicates)))



# 5. Transaction Table

transaction <- dbReadTable(my_db, "transaction")

# Transaction ID (6-digit)
validate_transaction_id <- function(transaction_id) {
  ifelse(grepl("^\\d{6}$", transaction_id), TRUE, FALSE)
}

# Order detail ID (6-digit)
validate_order_detail_id <- function(order_detail_id) {
  ifelse(grepl("^\\d{6}$", order_detail_id), TRUE, FALSE)
}

# Payment method (only letters)
validate_payment_method <- function(payment_method) {
  ifelse(grepl("^[A-Za-z ]+$", payment_method), TRUE, FALSE)
}

# checking payment methods other than listed and setting it to others
pay_method  <- "
  UPDATE 'transaction' SET payment_method = 'Others' where payment_method not in ('Transfer', 'PayPal','Credit Card','Debit Card','Pay at Door', 'Voucher');
"
dbExecute(my_db, pay_method)

# calculate invalid data
invalid_counts_transaction <- data.frame(
  invalid_transaction_ids = sum(!sapply(transaction$Transaction_ID, validate_transaction_id)),
  invalid_order_detail_ids = sum(!sapply(transaction$Order_Detail_ID, validate_order_detail_id)),
  invalid_payment_methods = sum(!sapply(transaction$Payment_Method, validate_payment_method))
)

print(invalid_counts_transaction)


# duplicates 

transaction_duplicates <- dbGetQuery(my_db, "SELECT transaction_id, count(*) from 'transaction' group by transaction_id having count(*) > 1")
print(paste("No:of duplicates for transaction table is ",nrow(transaction_duplicates)))



# 6. Order Details Table

order_detail <- dbReadTable(my_db, "order_detail")

# Order detail ID (6-digit)
validate_order_detail_id <- function(order_detail_id) {
  ifelse(grepl("^\\d{6}$", order_detail_id), TRUE, FALSE)
}

# transaction ID (6-digit)
validate_transaction_id <- function(transaction_id) {
  ifelse(grepl("^\\d{6}$", transaction_id), TRUE, FALSE)
}

# delivery date
validate_delivery_date <- function(delivery_date) {
  tryCatch(!is.na(as.Date(delivery_date, format = "%Y-%m-%d")), 
           error = function(e) FALSE)
}

# discount (xx%)
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

# duplicates 

order_detail_duplicates <- dbGetQuery(my_db, "SELECT order_detail_id, count(*) from order_detail group by order_detail_id having count(*) > 1")
print(paste("No:of duplicates for order_detail table is ",nrow(order_detail_duplicates)))


#7. Products table

product <- dbReadTable(my_db, "product")

# Validate price

validate_price <- function(price){
  ifelse(price <= 0, FALSE , TRUE )
}

validate_availability <-  function(availability) {
  if (!(availability %in% c("yes", "no"))) {
    return(TRUE)  
  } else {
    return(FALSE)
  }
}


validate_rating_pr <- function(rating) {
  if(!is.numeric(rating)) {
    return(rep(FALSE, length(rating)))  
  }
  
  result <- (rating >= 0.0 & rating <= 5.0) & (floor(rating * 10) == (rating * 10))
  return(result)
}

validate_prname <- function(product_name){
  if (is.null(product_name) | product_name == ""){
    print("product name is null")
    return (FALSE)
  }
  else {
    return(TRUE)
  }
}

validate_brand <- function(brand){
  if(is.null(brand) | brand == ""){
    print("product brand is null")
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

# calculate invalid data
invalid_counts_product <- data.frame(
  invalid_price = sum(!sapply(product$price, validate_price)),
  invalid_rating_pr = sum(!sapply(product$rating, validate_rating_pr)),
  invalid_availability = sum(!sapply(product$availability, validate_availability)),
  invalid_prname = sum(!sapply(product$product_name, validate_prname)),
  invalid_brand = sum(!sapply(product$brand, validate_brand))
)

print(invalid_counts_product)

# duplicates 

product_duplicates <- dbGetQuery(my_db, "SELECT product_id, count(*) from product group by product_id having count(*) > 1")
print(paste("No:of duplicates for product table is ",nrow(product_duplicates)))

#Referential Integrity

# Product Table

sup_id_query <- "UPDATE Product SET supplier_id = -1 WHERE supplier_id NOT IN (SELECT supplier_id FROM supplier);"
dbExecute(my_db, sup_id_query)

# Ad Table

pr_id_query <- "UPDATE Ad SET product_id = -1 WHERE product_id NOT IN (SELECT product_id FROM product);"
dbExecute(my_db, pr_id_query)

# Transaction Table

orderdet_id_query <- "UPDATE 'transaction' SET order_detail_id = -1 WHERE order_detail_id NOT IN (SELECT order_detail_id FROM order_detail);"
dbExecute(my_db, orderdet_id_query)

# Order Detail Table

tr_id_query <- "UPDATE order_detail SET transaction_id = -1 WHERE transaction_id NOT IN (SELECT transaction_id FROM 'transaction');"
dbExecute(my_db, tr_id_query)

# Order Table

or_id <- "UPDATE joint_order SET order_detail_id = -1 WHERE order_detail_id NOT IN (SELECT order_detail_id FROM order_detail);"
dbExecute(my_db, or_id)
cu_id <- "UPDATE joint_order SET customer_id = -1 WHERE customer_id NOT IN (SELECT customer_id FROM customer);"
dbExecute(my_db, cu_id)
pr_id <- "UPDATE joint_order SET product_id = -1 WHERE product_id NOT IN (SELECT product_id FROM product);"
dbExecute(my_db, pr_id)

# In table 

pr_id_in <- "UPDATE joint_in SET product_id = -1 WHERE product_id NOT IN (SELECT product_id FROM product);"
dbExecute(my_db, pr_id_in)
cat_id_in <- "UPDATE joint_in SET category_id = -1 WHERE category_id NOT IN (SELECT category_id FROM category);"
dbExecute(my_db, cat_id_in)



dbDisconnect(my_db)