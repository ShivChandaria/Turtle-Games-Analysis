## Turtle Games Customer Review Analysis

#Turtle Games is looking to improve overall sales performance by understanding customer trends. They are looking to understand:
#  - how customers accumulate loyalty points
#  - how groups within the customer base can be used to target specific market segments 
#  - how social data (e.g. customer reviews) can be used to inform marketing campaigns
#  - the impact that each product has on sales
#  - how reliable the data is (e.g. normal distribution, skewness, or kurtosis)
#  - what the relationship(s) is/are (if any) between North American, European, and global sales.

#This R Script  will answer the last three of their questions
-

# Prepare your workstation

# Install the tidyverse library.
install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse)

# Import the sales data set.
sales <- read.csv(file.choose(), header=T)

# Print the data frame.
sales

# View the data frame.
View(sales)

# Summarise the data set.
summary(sales)

#Get rid of Ranking, Year, Genre, and Publisher Columns
sales2 <- select(sales, -Ranking, -Genre, -Publisher, -Year)

# Print the data frame.
sales2

# Summarise the data set.
summary(sales2)


#Creating a new dataframe where sales are grouped by Platform
salesplatform <- select(sales2, -Product)

platformgrouped <- salesplatform %>% group_by(Platform) %>% summarise(across(everything(),sum, na.rm=TRUE))
View(platformgrouped)

#Creating Barplot with NA Sales and Platform 
NAPlatform<-ggplot(data=platformgrouped, aes(x=Platform, y=NA_Sales)) +
  geom_bar(stat="identity")
NAPlatform

#Creating Barplot with EU Sales and Platform
EUPlatform<-ggplot(data=platformgrouped, aes(x=Platform, y=EU_Sales)) +
  geom_bar(stat="identity")

EUPlatform

# Measure the mean, median, min and max of global sales
mean(sales2$Global_Sales)
median(sales2$Global_Sales)
min (sales2$Global_Sales)
max (sales2$Global_Sales)


# Measure the mean, median, min and max of NA sales
mean(sales2$NA_Sales)
median(sales2$NA_Sales)
min (sales2$NA_Sales)
max (sales2$NA_Sales)

# Measure the mean, median, min and max of EU sales
mean(sales2$EU_Sales)
median(sales2$EU_Sales)
min (sales2$EU_Sales)
max (sales2$EU_Sales)

# Import the tidyverse of dplyr for the group_by function.
library(dplyr)

#Creating a dataframe with only numerical values
sales3 <- select(sales2, -Platform)

#Creating a dataframe grouped by Product_ID where Global, NA and EU sales are summed
salesgrouped <- sales3 %>% group_by(Product) %>% summarise(across(everything(),sum, na.rm=TRUE))
View(salesgrouped)

# Measure normality in Global Sales values using a QQPlot
# Q-Q plot:
qqnorm(salesgrouped$Global_Sales)
# Add a reference line:
qqline(salesgrouped$Global_Sales, col='red')
#In the QQ plot the data points do not follow a straight line, suggesting evidence against normality

# Measure normality in EU Sales values using a QQPlot
# Q-Q plot:
qqnorm(salesgrouped$EU_Sales)
# Add a reference line:
qqline(salesgrouped$EU_Sales, col='red')
#In the QQ plot the data points do not follow a straight line, suggesting evidence against normality

# Measure normality in NA Sales values using a QQ Plot
# Q-Q plot:
qqnorm(salesgrouped$NA_Sales)
# Add a reference line:
qqline(salesgrouped$NA_Sales, col='red')
#In the QQ plot the data points do not follow a straight line, suggesting evidence against normality


# Determining Normality - Shapiro-Wilk test for Global Sales
shapiro.test((salesgrouped$Global_Sales))
#P value is < 0.05 suggesting evidence against normality

# Determining Normality - Shapiro-Wilk test for NA Sales
shapiro.test((salesgrouped$NA_Sales))
#P value is < 0.05 suggesting evidence against normality

# Determining Normality - Shapiro-Wilk test for EU Sales
shapiro.test((salesgrouped$EU_Sales))
#P value is < 0.05 suggesting evidence against normality

# Install the moments package and load the library.
install.packages('moments') 
library(moments)

#Determining Skewness and Kurtosis for Global Sales
skewness(salesgrouped$Global_Sales) 
#Result of 3.07 suggest data is Positively skewwed (right skew)
kurtosis(salesgrouped$Global_Sales)
#Result of 17.79 suggest data is leptokurtic (or heavy-tailed) distribution

#Determining Skewness and Kurtosis for NA Sales
skewness(salesgrouped$NA_Sales) 
#Result of 3.05 suggest data is Positively skewwed (right skew)
kurtosis(salesgrouped$NA_Sales)
#Result of 15.60 suggest data is leptokurtic (or heavy-tailed) distribution

#Determining Skewness and Kurtosis for EU Sales
skewness(salesgrouped$EU_Sales) 
#Result of 2.89 suggest data is Positively skewwed (right skew)
kurtosis(salesgrouped$EU_Sales)
#Result of 16.23 suggest data is leptokurtic (or heavy-tailed) distribution


cor(salesgrouped$NA_Sales, salesgrouped$EU_Sales)
#Positive Relation

#NA and Global Simple regression model
plot(salesgrouped$NA_Sales,salesgrouped$Global_Sales)
NAmodel <- lm(Global_Sales ~ NA_Sales, data = salesgrouped)
NAmodel
summary(NAmodel)
abline(coefficients(NAmodel))

#NA and EU Simple regression model
plot(salesgrouped$NA_Sales,salesgrouped$EU_Sales)
modelb <- lm(EU_Sales ~ NA_Sales, data = salesgrouped)
modelb
summary(modelb)
#An r-squared value of 0.39 suggestsvery weak linear relation.


#Global, EU, and NA Multiple regression model
modelc = lm(Global_Sales~EU_Sales+NA_Sales,
            data=salesgrouped)

summary(modelc)
#Has a strong adjusted r squaraed value of 0.97

#Creating a newdataframe for predictions with model c
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
predictors <- data.frame(EU_Sales,NA_Sales)
predictors

#Making predictions with modelc and new prediction variables
predict(modelc, newdata = predictors)
