# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(corrplot)
library(Amelia)

data<-read.csv("C:/Users/Admin/Downloads/appleStore_1.csv")

str(data)

sum(is.na(data))

missmap(data, main = "Missing values vs Observed")

data$cont_rating <- as.numeric(gsub("\\+", "", data$cont_rating))

# Distribution of app prices
ggplot(data, aes(x = price)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of App Prices", x = "Price", y = "Count")

ggplot(data, aes(x = user_rating)) + 
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of User Ratings", x = "User Rating", y = "Count")

# Total number of ratings by genre
total_ratings_by_genre <- data %>%
  group_by(prime_genre) %>%
  summarise(total_ratings = sum(rating_count_tot))
print(total_ratings_by_genre)

# Top 10 most expensive apps
top_10_expensive_apps <- data %>%
  arrange(desc(price)) %>%
  head(10)
print(top_10_expensive_apps)

# Group by genre and calculate average user rating and price
genre_summary <- data %>%
  group_by(prime_genre) %>%
  summarise(avg_rating = mean(user_rating, na.rm = TRUE),
            avg_price = mean(price, na.rm = TRUE),
            app_count = n()) %>%
  arrange(desc(avg_rating))

ggplot(genre_summary, aes(x = reorder(prime_genre, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average User Rating by Genre", x = "Genre", y = "Average Rating")

# Count of Apps by Currency
ggplot(data, aes(x = currency)) +
  geom_bar(fill = "purple") +
  labs(title = "Count of Apps by Currency", x = "Currency", y = "Count")

# Count of Apps by Content Rating
ggplot(data, aes(x = cont_rating)) +
  geom_bar(fill = "cyan") +
  labs(title = "Count of Apps by Content Rating", x = "Content Rating", y = "Count")

# Count of Apps by Prime Genre
ggplot(data, aes(x = prime_genre)) +
  geom_bar(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Count of Apps by Prime Genre", x = "Prime Genre", y = "Count")

# Correlation matrix of numeric variables
numeric_vars <- data %>% select_if(is.numeric)
corr_matrix <- cor(numeric_vars)
corrplot(corr_matrix, method = "color")

# Create a new feature - log of price to handle skewness
data$log_price <- log1p(data$price)

# Boxplot for detecting outliers in User Rating
ggplot(data, aes(y = user_rating)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot of User Rating", y = "User Rating")

# Identifying outliers using the IQR method for User Rating
Q1 <- quantile(data$user_rating, 0.25)
Q3 <- quantile(data$user_rating, 0.75)
IQR <- Q3 - Q1
user_rating_outliers <- data[data$user_rating < (Q1 - 1.5 * IQR) | data$user_rating > (Q3 + 1.5 * IQR), ]

# Display outliers
print(nrow(user_rating_outliers))
head(user_rating_outliers)

# Repeat for other numerical variables, e.g., price
ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Price", y = "Price")

# Identifying outliers using the IQR method for Price
Q1_price <- quantile(data$price, 0.25)
Q3_price <- quantile(data$price, 0.75)
IQR_price <- Q3_price - Q1_price
price_outliers <- data[data$price < (Q1_price - 1.5 * IQR_price) | data$price > (Q3_price + 1.5 * IQR_price), ]

# Display outliers
print(nrow(price_outliers))
head(price_outliers)

