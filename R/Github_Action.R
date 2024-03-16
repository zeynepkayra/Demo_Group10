library(DBI)
library(dplyr)
library(RSQLite)

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


# update database
dbWriteTable(my_db, "customer", customer, overwrite = TRUE, row.names = FALSE)



# calculate invalid data
invalid_counts <- customer %>%
  summarize(
    invalid_emails = sum(!valid_email),
    invalid_phones = sum(!valid_phone),
    invalid_postcodes = sum(!valid_postcode)
  )

print(invalid_counts)


dbDisconnect(my_db)