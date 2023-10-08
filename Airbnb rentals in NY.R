# Load packages
library(tidyverse)
library(lubridate)

# Import data
data <- read.csv(file.choose())
head(data)

# Display the structure of data
str(data)
dim(data)

# Check for missing values
sum(is.na(data))
data <- na.omit(data)

# Check for duplicates
sum(duplicated(data))

# Descriptive Statistics
summary(data)

# 1.What are the most popular neighborhoods in New York City for Airbnb rentals?
ggplot(data,aes(neighbourhood_group))+
  geom_bar()

data %>%
  count(neighbourhood) %>%
  arrange(desc(n)) %>%
  head(10)

# 2.What types of properties are most commonly listed on Airbnb in New York City?
data %>%
  count(room_type) %>%
  arrange(desc(n))

# 3.How do prices vary by neighborhood and property type?
data %>%
  group_by(neighbourhood, room_type) %>%
  summarize(avg_price = mean(price)) %>%
  arrange(desc(avg_price))

# 4.Compare the number of Airbnb rental listings across different neighborhoods and room types
data %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = neighbourhood_group, y = n, fill = room_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Neighborhood_group", y = "Number of Listings", title = "Number of Airbnb Rental Listings by Neighborhood and Room Type") +
  theme_bw()

# Select only numerical variables
numerical_vars <- data %>% 
  select_if(is.numeric)

# Compute correlation matrix
cor_matrix <- cor(numerical_vars)

# Print correlation matrix
print(cor_matrix)

# Load ggcorrplot package
library(ggcorrplot)

# Create correlation plot
ggcorrplot(cor_matrix, type = "lower")