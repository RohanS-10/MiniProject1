library(ggplot2)
library(tidyverse)

# Read the dataset
br_data <- read.csv("br2.csv")

# Scatter plot: Price vs. Square Footage
scatter_plot_sqft_price = ggplot(br_data, aes(x = sqft, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot: Price vs. Square Footage",
       x = "Square Footage",
       y = "Price")

# Box plot: Bedrooms
box_plot_bedrooms_price = ggplot(br_data, aes(x = as.factor(bedrooms), y = price)) +
  geom_boxplot() +
  labs(title = "Box Plot: Price vs. Bedrooms",
       x = "Bedrooms",
       y = "Price")

# Bar plot: Count of houses with and without a pool
bar_plot_pool_count = ggplot(br_data, aes(x = pool, fill = factor(pool))) +
  geom_bar() +
  labs(title = "Bar Plot: Count of Houses with and without a Pool",
       x = "Pool",
       y = "Count") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red"),
                    labels = c("With Pool", "Without Pool"))

# Histogram: Distribution of Age
histogram_age = ggplot(br_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram: Distribution of Age",
       x = "Age",
       y = "Frequency")

# Correlation matrix heatmap
cor_matrix <- cor(br_data[, c("price", "sqft", "bedrooms", "baths", "Age")]) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  gather(key = "Var2", value = "value", -Var1)

heatmap_cor_matrix = ggplot(data = cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Matrix Heatmap",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Correlation")


#STATISTICAL ANALYSIS


#T-Tests

#Bedrooms and Price


# Bedrooms and Price ANOVA
anova_bedrooms_price <- aov(price ~ as.factor(bedrooms), data = br_data)
summary(anova_bedrooms_price)

# Baths and Price ANOVA
anova_baths_price <- aov(price ~ as.factor(baths), data = br_data)
summary(anova_baths_price)




#Correlation Analysis


cor_matrix <- cor(br_data[, c("price", "sqft", "bedrooms", "baths", "Age")])
cor_matrix


#Linear Regression Model with Bedrooms and Baths


model <- lm(price ~ bedrooms + baths, data = br_data)
summary(model)



# Scatterplot with regression line for Bedrooms
ggplot(br_data, aes(x = bedrooms, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression Model: Price vs Bedrooms",
       x = "Bedrooms",
       y = "Price")

# Scatterplot with regression line for Baths
ggplot(br_data, aes(x = baths, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Linear Regression Model: Price vs Baths",
       x = "Baths",
       y = "Price")




# Save all plots
ggsave("scatter_plot_sqft_price.png", plot = scatter_plot_sqft_price)
ggsave("box_plot_bedrooms_price.png", plot = box_plot_bedrooms_price)
ggsave("bar_plot_pool_count.png", plot = bar_plot_pool_count)
ggsave("histogram_age.png", plot = histogram_age)
ggsave("heatmap_cor_matrix.png", plot = heatmap_cor_matrix)
