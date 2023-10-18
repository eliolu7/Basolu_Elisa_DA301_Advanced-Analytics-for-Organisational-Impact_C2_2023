## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.
# Import necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("forcats")
install.packages("magrittr")
install.packages("knitr")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(forcats)
library(knitr)
# Instructions
# 1. Load and explore the data.
turtle_sales <- read.csv("/Users/elisa/Desktop/Course 3/LSE_DA301_assignment_files 2/turtle_sales.csv")

##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating a subset
turtle_sales_subset <- subset(turtle_sales, select = -c(Ranking, Year, Genre, Publisher))

# Summary of the new data frame
summary(turtle_sales_subset)

# Calculate Other Sales
turtle_sales$Other_Sales <- turtle_sales$Global_Sales - (turtle_sales$NA_Sales + turtle_sales$EU_Sales)

# Add Other Sales to the subset:
turtle_sales_subset$Other_Sales <- turtle_sales$Other_Sales

#view the new subset
head(turtle_sales_subset)


#Grouping by products to determine the impact of sales on them.
library(dplyr)
sales_by_product <- turtle_sales_subset %>%
  group_by(Product) %>%
  summarise(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_Other_Sales = sum(Other_Sales),
    Total_Global_Sales = sum(Global_Sales),
    Average_NA_Sales = mean(NA_Sales),
    Average_EU_Sales = mean(EU_Sales),
    Average_Other_Sales = mean(Other_Sales),
    Average_Global_Sales = mean(Global_Sales)
  )

# 2. Create plots to review and determine insights into data set.

#Create a pie chart to view the distribution of sales by region with %
# Sum up the total sales for each region
total_EU_sales <- sum(turtle_sales_subset$EU_Sales)
total_NA_sales <- sum(turtle_sales_subset$NA_Sales)
total_Other_sales <- sum(turtle_sales_subset$Other_Sales)

# Create a data frame with the total sales values
sales_data <- data.frame(
  Region = c("EU", "NA", "Others"),
  Total_Sales = c(total_EU_sales, total_NA_sales, total_Other_sales)
)

# Calculate the percentages
sales_data$Percentage <- (sales_data$Total_Sales / sum(sales_data$Total_Sales)) * 100

# Plot a pie chart with percentage labels
ggplot(sales_data, aes(x = "", y = Total_Sales, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Total Sales by Region") +
  geom_text(aes(y = cumsum(Total_Sales) - (0.5 * Total_Sales), 
                label = paste0(round(Percentage, 1), "%")), size = 4)

# View the descriptive statistics.
summary(turtle_sales_subset)

# 2. Review plots to determine insights into the data set.
## 2a) Scatterplots
# Create scatterplots.
# Scatterplot of NA Sales vs EU Sales with trend line
ggplot(turtle_sales, aes(x = NA_Sales, y = EU_Sales)) + 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  labs(title = "Relationship between NA Sales and EU Sales", 
       x = "NA Sales (in millions)", 
       y = "EU Sales (in millions)") +
  theme_minimal()

## 2b) Histograms
# Create histograms.
ggplot(turtle_sales_subset, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title="Histogram of Global Sales")

#Topsellers
high_sales_threshold <- quantile(turtle_sales_subset$Global_Sales, 0.90)
top_sellers <- subset(turtle_sales_subset, Global_Sales > high_sales_threshold)


## 2c) Boxplots
# Create boxplots.
# Boxplots for distribution of Global Sales by product
ggplot(turtle_sales_subset, aes(x = reorder(Product, -Global_Sales), y = Global_Sales)) +
  geom_boxplot(fill = "lightblue") +
  labs(title="Distribution of Global Sales by Product", x="Product", y="Global Sales (in millions)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################

# 3. Observations and insights

## The sales data for Turtle Games showcases the distribution of sales across North America (NA), Europe (EU), and other regions. North America dominates with 47.2% of the total sales, trailed by Europe and other markets. A deeper dive reveals:
##NA Sales: With a spectrum from 0 to 34.02 million units, NA exhibits an average sales volume of 2.516 million units, and a median of 1.82 million units.
##EU Sales: European sales fluctuate between 0 and 23.8 million units. The region's average sales sit at 1.644 million units, and its median at 1.17 million units.
##Global Sales: Depicting worldwide sales, the global figure ranges from 0.01 to 67.85 million units, averaging 5.335 million units and settling at a median of 4.32 million units.
##Other Sales: Encompassing regions outside NA and EU, sales vary from 0 to 10.03 million units, averaging 1.175 million units with a median of 0.61 million units.
##Notably, North America consistently records higher sales compared to Europe. The presence of high maximum sales figures alongside modest median values suggests the influence of outlier games with extremely high sales. Histogram analysis confirms this, showing a concentration of games on the graph's left, with outliers to the right.
##A scatterplot establishes a positive correlation between EU and NA sales, signifying that hits in one region often resonate in the other, pointing to shared gamer preferences or universally appealing games.




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(turtle_sales_subset)

# Check output: Determine the min, max, and mean values.
summary(turtle_sales_subset)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
grouped_data <- turtle_sales_subset %>%
  group_by(Product) %>%
  summarise(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_Global_Sales = sum(Global_Sales),
    Total_Other_Sales = sum(Other_Sales)
  )

# View the data frame.
View(grouped_data)

# Explore the data frame.
summary(grouped_data)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# Scatter plot with trend line for Total NA Sales vs Total EU Sales
ggplot(grouped_data, aes(x = Total_NA_Sales, y = Total_EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Adding the linear regression trend line
  labs(title = "Scatter Plot of Total NA Sales vs Total EU Sales with Trend Line",
       x = "Total NA Sales",
       y = "Total EU Sales") +
  theme_minimal()

# Create histograms.
ggplot(grouped_data, aes(x=Total_Global_Sales)) + geom_histogram()

# Create boxplots.
ggplot(grouped_data, aes(y=Total_Global_Sales)) + geom_boxplot()

###############################################################################

# 3. Determine the normality of the data set.
## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(grouped_data$Total_Global_Sales)
qqline(grouped_data$Total_Global_Sales)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(grouped_data$Total_Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(grouped_data$Total_Global_Sales)
kurtosis(grouped_data$Total_Global_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(grouped_data$Total_NA_Sales, grouped_data$Total_EU_Sales)

###############################################################################

# 5. Observations and insights
##Data Distribution: The Q-Q plot revealed the data didn't follow a perfect normal distribution, implying potential skewness or kurtosis.
##Normality Tests: The Shapiro-Wilk test confirmed non-normality in the Global Sales data (p-value < 2.2e-16). This could be due to outliers, skewness, or data transformations.
##Skewness and Kurtosis: Positive skewness (3.066769) indicated the tail on the right side of the distribution was longer, meaning there were some games with exceptionally high sales. High kurtosis (17.79072) suggested there were more extreme values than expected.
##Correlation: North American and European sales were significantly positively correlated (0.6209317). This means when sales in North America increase, European sales tend to increase as well.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# Calculate correlation between NA_Sales and EU_Sales
correlation_coefficient_NA_EU <- cor(turtle_sales$NA_Sales, turtle_sales$EU_Sales)
print(paste("Correlation between NA_Sales and EU_Sales:", correlation_coefficient_NA_EU))

# Create a linear regression model with EU_Sales as the dependent variable 
# and NA_Sales as the independent variable
simple_lm_NA_EU <- lm(EU_Sales ~ NA_Sales, data = turtle_sales)
summary(simple_lm_NA_EU)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns
numeric_data <- select(turtle_sales, NA_Sales, EU_Sales, Global_Sales)

# Build the multiple linear regression model, using Global_Sales as the dependent variable
model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = numeric_data)
summary(model)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Given coefficients from the regression model
coeff_intercept <- 0.22175
coeff_NA_Sales <- 1.15543
coeff_EU_Sales <- 1.34197

# Given NA_Sales and EU_Sales values
NA_Sales_values <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_values <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Predicting Global_Sales using the model
predicted_Global_Sales <- coeff_intercept + coeff_NA_Sales * NA_Sales_values + coeff_EU_Sales * EU_Sales_values

# Printing the predicted Global_Sales
print(predicted_Global_Sales)

# Predicting Global_Sales using the provided model coefficients
predicted_Global_Sales <- coeff_intercept + coeff_NA_Sales * NA_Sales_values + coeff_EU_Sales * EU_Sales_values

# Extracting the observed Global_Sales for those specific NA_Sales and EU_Sales values
observed_Global_Sales <- numeric(length(NA_Sales_values))

for (i in 1:length(NA_Sales_values)) {
  observed_Global_Sales[i] <- turtle_sales$Global_Sales[which(turtle_sales$NA_Sales == NA_Sales_values[i] & 
                                                                turtle_sales$EU_Sales == EU_Sales_values[i])]
}

# Comparing predicted to observed values
comparison_df <- data.frame(NA_Sales = NA_Sales_values,
                            EU_Sales = EU_Sales_values,
                            Predicted_Global_Sales = predicted_Global_Sales,
                            Observed_Global_Sales = observed_Global_Sales)

print(comparison_df)

###############################################################################

# 5. Observations and insights
##Correlation & Significance: There's a strong positive relationship between North American and European sales. Both regions significantly predict global sales, accounting for about 96.85% of the variance (Adjusted R-squared: 0.9685).
##Redundancy: Using North American and European sales to predict global sales can be redundant since global sales is a sum of different regions' sales. However, it helps understand their weighted influence on the global market.
##Predictive Accuracy: The model's predictions are close to the observed values, indicating its reliability. However considering the impact of other sales can enhance accuracy.


###############################################################################
###############################################################################




