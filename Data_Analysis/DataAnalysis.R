## -----------------------------------------------------------------------
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)


## -----------------------------------------------------------------------
schema_db <- RSQLite::dbConnect(RSQLite::SQLite(), "ECommerce.db")


## -----------------------------------------------------------------------
# Read data
table_name <- dbListTables(schema_db)
tables <- list()
for (name in table_name){
  tables[[name]] <- dbReadTable(schema_db, name)
}


## -----------------------------------------------------------------------
joint_in_table <- tables[["joint_in"]]
joint_order_table <- tables[["joint_order"]]


## -----------------------------------------------------------------------
# Payment methods
transaction_table <- tables[["transaction"]]
payment_plot <- ggplot(transaction_table, aes(x = payment_method))+
  geom_bar(fill = 'indianred3') +
  labs(x = "Payment Method", y = "Count")

# Save the plot
ggsave("Data_Analysis/payment.jpeg", plot = payment_plot, width = 10, height = 6, dpi = 300)



## -----------------------------------------------------------------------
# Best sellers for each category
product_table <- tables[["product"]]
category_table <- tables[["category"]]

bsec <- product_table %>% 
  left_join(joint_in_table, by = "product_id") %>%
  left_join(category_table, by = "category_id") %>%
  left_join(joint_order_table, by = "product_id")

best_sellers <- bsec %>%
  group_by(category_name, product_name) %>%
  summarise(total_quantity = sum(quantity), .groups = 'drop') %>%
  group_by(category_name) %>%
  arrange(desc(total_quantity)) %>%
  slice(1) %>% 
  ungroup()

bests <- ggplot(best_sellers) +
  geom_bar(aes(x = product_name, y = total_quantity, fill = category_name), 
           stat = "identity") + 
  labs(x = "Products", y = "Quantity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("Data_Analysis/bestsellers.jpeg", plot = bests, width = 10, height = 6, dpi = 300)



## -----------------------------------------------------------------------
# Which category is the most popular

category_table <- tables[["category"]]
product_table <- tables[["product"]]
customer_table <- tables[["customer"]]
joint_in_table <- tables[["joint_in"]]

# Step 1: Join the product and category tables using the joint table
product_category_join <- joint_in_table%>%
  inner_join(product_table, by = "product_id") %>%
  inner_join(category_table, by = "category_id")

# Step 2: Aggregate the data to count occurrences of each category
category_counts <- product_category_join %>%
  group_by(category_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Step 3: Print or visualize the results to identify the most popular category
most_popular_category <- category_counts$category_name[1]
print(paste("The most popular category is:", most_popular_category))

# Writing it into the csv file

write.csv(most_popular_category ,"most_popular_category.csv", row.names = FALSE)


top_cat <- ggplot(category_counts, aes(x = reorder(category_name, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Category", y = "Number of Products", title = "Popularity of Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Save the plot
ggsave("Data_Analysis/top_category.jpeg", plot = top_cat, width = 10, height = 6, dpi = 300)


## -----------------------------------------------------------------------
# Final price of orders distribution with respect to payment method
tot_price <- 'SELECT final_price, order_detail_id FROM final_price_of_order'
view_tot <- dbGetQuery(schema_db, tot_price)
view_tot <- as_tibble(view_tot)

transaction_table <- tables[["transaction"]]
transaction_table <- left_join(transaction_table, view_tot, by = 'order_detail_id')
basket <- ggplot(transaction_table, aes(final_price, fill = payment_method)) +
  geom_histogram(binwidth = 2000) +
  labs(title = 'Distribution of Orders based on Payment Method', x = 'Price', y = 'Frequency') 

# Save the plot
ggsave("Data_Analysis/basket_price.jpeg", plot = basket, width = 10, height = 6, dpi = 300)


## -----------------------------------------------------------------------
# Top 20 customers 

# Join the joint_order table with the customer_table to include customer names
customer_order_join <- inner_join(joint_order_table, customer_table, by = "customer_id")

# Aggregate the data to count the number of orders per customer
customer_order_counts <- customer_order_join %>%
  group_by(customer_id, First_Name , Last_Name) %>%
  summarise(order_count = n(), .groups = "drop") %>%
  arrange(desc(order_count), First_Name , Last_Name)

# Select the top 20 customers with the highest order counts
top_20_customers <- head(customer_order_counts, 20)

# Print or visualize the results
print(top_20_customers)

# Optionally, you can save the top 20 customers to a CSV file
write.csv(top_20_customers, "Data_Analysis/top_20_customers.csv", row.names = FALSE)





## -----------------------------------------------------------------------
# which product has the highest rating in each category

# Join the product_table to include product names
product_rating_join <- inner_join(product_table,product_category_join, by = "product_id", suffix = c("_product", "_joint"))

# Group the data by category and find the product with the highest rating in each category
highest_rating_per_category <- product_rating_join %>%
  group_by(category_id) %>%
  slice(which.max(rating_product)) %>%
  arrange(category_id)

# Print or visualize the results
print(highest_rating_per_category)

# Optionally, you can save the results to a CSV file
write.csv(highest_rating_per_category, "highest_rating_per_category.csv", row.names = FALSE)



