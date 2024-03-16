library(DBI)
library(dplyr)
library(RSQLite)

# Running data validation

# Connect to database
my_db <- RSQLite::dbConnect(RSQLite::SQLite(),"ECommerce.db")

# 1. Customer table
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


# apply to customer table
customer <- customer %>%
  mutate(validate_customer_id = validate_customer_id(customer_id),
         valid_email = validate_email(email),
         validate_first_name = validate_first_name(First_Name),
         validate_last_name = validate_last_name(Last_Name),
         valid_phone = validate_phone(mobile_no),
         validate_credit_card_no = validate_credit_card_no(credit_card_no),
         validate_country_uk = validate_country_uk(country),
         validate_street_name = validate_street_name(street_name),
         validate_house_no = validate_house_no(house_no),
         valid_postcode = validate_uk_postcode(postcode),
  )


# update database
dbWriteTable(my_db, "customer", customer, overwrite = TRUE, row.names = FALSE)



# calculate invalid data
invalid_counts <- customer %>%
  summarize(
    invalid_customer_id = sum(!validate_customer_id),
    invalid_emails = sum(!valid_email),
    invalid_first_name = sum(!validate_first_name),
    invalid_last_name = sum(!validate_last_name),
    invalid_phones = sum(!valid_phone),
    invalid_credit_card_no = sum(!validate_credit_card_no),
    invalid_country_uk = sum(!validate_country_uk),
    invalid_street_name = sum(!validate_street_name),
    invalid_house_no = sum(!validate_house_no),
    invalid_postcodes = sum(!valid_postcode),
  )

print(invalid_counts)


dbDisconnect(my_db)




