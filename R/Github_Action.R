library(DBI)
library(dplyr)

# 1.	Running data validation
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")
customer <- dbReadTable(my_db, "customer")

# Validate Email format 
validate_email <- function(email) {
  email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  ifelse(grepl(email_pattern, email), TRUE, FALSE)
}


# Validate Mobile number format
validate_phone <- function(phone) {
  phone_pattern <- "^\\+?[1-9]\\d{1,14}$"
  ifelse(grepl(phone_pattern, phone), TRUE, FALSE)
}


# Validate UK postcode format
validate_uk_postcode <- function(postcode) {
  postcode_pattern <- "^([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9][A-Za-z]?))))\\s?[0-9][A-Za-z]{2})$"
  ifelse(grepl(postcode_pattern, postcode), TRUE, FALSE)
}


# apply to customer table
customer <- customer %>%
  mutate(valid_email = validate_email(email),
         valid_phone = validate_phone(mobile_no),
         valid_postcode = validate_uk_postcode(postcode))

dbDisconnect(my_db)


# 2.	Updating the database with new data

library(DBI)
library(RSQLite)

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")


# Update product information
update_product_info <- function(my_db, product_id, new_price, new_availability) {
  dbExecute(my_db, "UPDATE product SET price = ?, availability = ? WHERE product_id = ?", 
            params = list(new_price, new_availability, product_id))
}
update_product_info(my_db, '1', 110, 'True')


# Update customer information
update_customer_info <- function(my_db, customer_id, new_email, new_mobile_no) {
  dbExecute(my_db, "UPDATE customer SET email = ? WHERE customer_id = ?", 
            params = list(new_email, customer_id))
}
update_customer_info(my_db, '176122', 'Deanna_Parisian_H@mail.com')


dbDisconnect(my_db)

