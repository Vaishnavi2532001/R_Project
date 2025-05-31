# GOALS 
# This project presents an exploration of car sales data 2022-2023
# Objectives of this analysis are:
# Gain insights from data to Analyse it thoroughly.
# Clean and Prepare data for further analysis.
# Identify trends and patterns in data. 
# Use visualizations to emphasize Sales trends by Company, model and over time. 

## DATA

mod.1 = data.frame(Car.Sales.xlsx...car_data)
summary(mod.1)
library(dplyr)

# DATA INFORMATION :- 
# There are 16 columns and 23906 rows overall.
# The name and datatype of each column - most values are Character in this data set.
# Some Columns should be removed which are less important.
# There is no missing data.  

mod.1= dplyr::select(Car.Sales.xlsx...car_data, -Phone, -Car_id, -Customer.Name )
summary(mod.1)
is.na.data.frame(mod.1)
count = table(Car.Sales.xlsx...car_data$Company)
count
barplot(sort(count)) ## Tells company count through bar graph.
# Annual Income:- 

summary(mod.1$Annual.Income)
sd(mod.1$Annual.Income)

#The average annual income is 830,840.
#The Income Range from minimum of 10,080 to maximum 11,200,000.  
#The average income is notably high, indicating a potential skew towards specific individuals.
#The large standard deviation of 720,006 suggests considerable variation in income levels.
#Such data have a higher chance of unequal distribution of data and notify the 
#the presence of outilers.
# Annual.Income needs further analysis.
# Analyse the Annual Income to detect the presence of outliers  

boxplot(mod.1$Annual.Income)
plot(density(mod.1$Annual.Income))

# From mean and Standard Deviation we are getting that outliers are there.
# Data Cleaning (Removing Outliers)

iqr= IQR(mod.1$Annual.Income)
iqr
up <- quantile(mod.1$Annual.Income, 0.63) + 1.5*iqr
up
low <- quantile(mod.1$Annual.Income, 0.25)- 1.5*iqr
low
eliminate <- subset(mod.1, mod.1$Annual.Income >low & mod.1$Annual.Income < up)
summary(eliminate)
mod.1_updated =eliminate
summary(mod.1_updated)
boxplot(mod.1_updated$Annual.Income)
## REMOVED OUTLIERS 

#PRICE:-

summary(mod.1_updated$Price....)

#The average car price is 28,090.
#The price ranges from a minimum of 1,200 to a maximum of 85,800.
#The price column shows a wide range of car prices, 
#with the majority of cars priced around the median of $23,000, indicating a mix of mid-range and high-end vehicles.

# EDA (Exploratory Data Analysis)
# As we have clean the data, now its time for EDA
# Next topics for analysis:

# DEALER PERFORMANCE ANALYSIS:
# This section focuses on evaluating the performance of different car dealers based on key financial metrics such as total revenue and average car prices.
# The results will highlight which dealers stand out in terms of revenue and pricing, allowing for a deeper exploration of factors contributing to their performance.

# Dealer Revenue
#Group by dealer_name and sum the price to calculate total revenue for each dealer

# The dealer with the highest total revenue is Rabun Used Car Sales with 
# a revenue of 35.45 million, closely followed by Progressive Shippers 
# Cooperative Association No and U-Haul CO, which have revenues of 34.63 
# million and 33.81 million respectively. These dealers likely have a large 
# volume of sales, contributing to their high revenue figures.

## Average car price by Dealer:- 
Average.price = mod.1 %>%
  group_by(Dealer_Name) %>%
  summarize(Average.price= mean(Price.... ))
print(Average.price, n=28)

# Sorted in Descending order
Average.price = arrange(Average.price, desc(Average.price))
print(Average.price , n= 28)
#TOP 10 DEALERS WIT RESPECT TO AVERAGE:- 
top_10_dealers = top_n_values <- Average.price %>%
  group_by(Dealer_Name) %>%
  top_n(10, Average.price)
print(top_10_dealers)


# The average car prices shows that U-Haul CO offers the highest 
# average price per car at $28,603.61, while Classic Chevy and Rabun Used Car Sales follow
# closely behind. This suggests that these dealers may focus on selling higher-end or 
# more expensive vehicles, contributing to their revenue despite potentially lower sales volume


#Revenue vs. Price: Interestingly, some dealers like U-Haul CO rank high in both total revenue 
#and average price, while others such as Progressive Shippers Cooperative Association Not perform
#well in terms of revenue and offer relatively lower average prices. This may indicate different 
#sales strategies, with some dealers focusing on volume and others on premium pricing.

## COMPANIES ANALYSIS:- 
# In this section we will analyse car companies based on crucial metrics like overall sales of each company
# Total revenue and average car prices.

# By our analyse we can conclude which company will dominate the market, most popular car model and can conclude customers 
#preference.

# calculate overall revenue of companies:- 

company_revenue <- mod.1 %>%
  group_by(Company) %>%
  summarize(total_revenue = sum(Price....))
print(company_revenue)

company_revenue = over_all_revenue

# revenue sorted in descending order.

over_all_revenue <- arrange(company_revenue, desc(total_revenue))
print(over_all_revenue, n = 30)
 
#Top 10 companies that dominate market with respect to revenue.

top_10_companies = top_n_values <- over_all_revenue  %>% 
  group_by(Company) %>%
  top_n(10,  total_revenue)
print(top_10_companies )

# In terms of revenue Chevrolet dominate the market with 47655265 total_sales_value
#showing that it not only sells well but also commands higher prices for its vehicles.

# Ford ranks second with 47,231,583 in revenue, reflecting its strong market presence and 
# diverse vehicle lineup. Dodge follows closely behind with 44,124,996, demonstrating its 
# ability to generate significant revenue through a combination of solid sales volume and competitive pricing.


## Total sales_Count of companies:-

x = table(mod.1$Company)
total_sales_count <- as.data.frame(x)
class(total_sales_count)

# Total sales count sorted in descending order:-

arrange_total_sales_count<- total_sales_count[order(total_sales_count$Freq, decreasing = TRUE),]
print(arrange_total_sales_count)

# In terms of sales count also Chevrolet dominate the market with 1819 units, presenting the strong consumer preference and brand loyalty.
# Dodge and Ford follow closely behind with sales volumes of 1,671 and 1,614 units, respectively, suggesting effective marketing strategies and popular model offerings.

## This correlation between sales volume and revenue underscores the financial strength of 
#  these brands and their ability to maintain profitability through effective pricing strategies.

## Calculate Average car price for each company


Average_car_price = mod.1 %>%
  group_by(Company) %>%
  summarize(car_prize= mean(Price.... ))
print(Average_car_price, n=40)

# Sorted in Descending order
Average.carprice = arrange(Average_car_price, desc(car_prize))
print(Average.carprice , n= 40)

## Top 10 companies with highest average car prize

top_Averagecars = top_n_values <- Average.carprice %>%
  group_by(Company) %>%
  top_n(10, car_prize)
print(top_Averagecars)

## Company Cadillac has highest average car prize of 40,972. indicating its position as a 
# luxury brand catering to a more affluent customer base.
# Other companies like Saab and Lexus also serves luxury followed by Cadillac with 36516 and 34025 average prize.
# The emphasis on higher average prices suggests that these companies may prioritize quality and luxury over sheer 
# sales volume, targeting niche markets.

#Calculate the model for each company based on sales per unit


popular_models <- mod.1 %>%
  group_by(Company, Model) %>%
  summarise(sales_volume = n()) %>%
  ungroup()
print(popular_models, n= 155)
# Sort the models in Descending order
popular_models <- arrange(popular_models,desc(sales_volume))
print(popular_models, n= 155)

# Calculated the most popular models with respect to companies:- 


most_popular_models = popular_models %>%
  group_by(Company) %>%
  slice_max(order_by = sales_volume, n = 1, with_ties = FALSE) %>% 
  ungroup() 
print(most_popular_models, n=30)
# Sorted in descending 
most_popular_models = arrange(most_popular_models, desc(sales_volume))
print(most_popular_models, n=30)

## Most popular model According to Average_prize

average_price = mod.1 %>%
  group_by(Company,Model)  %>%
  summarise(Average_car= mean(Price....))
print(average_price)
average_price<- arrange(average_price, desc(Average_car))
print(average_price, n= 155)

attach(mod.1)
most_popular_models <- most_popular_models %>% 
  group_by(Company,Model)  %>%
  merge(average_price)   
print(most_popular_models)

# Sorted data in Descending order according to Sales_Volume:-

most_popular_models <- arrange(most_popular_models,  desc(sales_volume ))
print(most_popular_models)

# The analysis shows the model Diamante from Company Mitsubishi with average car 
# prize 22268.30 and total sales of 418 units, is the highest selling model because of its minimum 
# average prize has became the  first choice of the customers. 

# Prizm and Silhouette are also  close behind the  Diamante  with 411 sales unit 
# These Companies shows different strategies with their premium features and price range to the top the market.

# Sorted data According to Average_car: 
most_popular_models <- arrange(most_popular_models,  desc(Average_car  ))
print(most_popular_models)

# Result showcase that model Tacoma of company Toyota with highest Average Price of44576.74 and 179 sales unit shows that the company 
# serves legacy and their robust features, followed by model Continental & Eldorado of company Lincoln & Cadillac with 42612.00 & 41919.86
# with 206 and 232 sales unit. The prices reflect the strategies of different brands, where premium models are priced higher to 
# reflect their quality and features.


# Analysis According to Average_car price shows drastic difference in the range and preferences of consumer
# As per their life style some chose luxury and some choose comfort. 

# CUSTOMERS DEMEOGRAPICS ANALYSIS:-

# Analysis worth exploring the variables like gender, demography etc.
#Examining these aspects provides insights into consumer preferences, identifies trends among different demographic groups, and explores 
# how these factors contribute to the popularity of specific car brands and models.

# Calculate sales volume by gender

gender_wise_sales =  mod.1 %>% 
  group_by(Gender) %>%
  summarise(sales_volume = n()) %>%
  ungroup()
print(gender_wise_sales)

## Bar plot for Gender wise sales:-

library(ggplot2)

ggplot(gender_wise_sales, aes(x = Gender, y = sales_volume, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales Volume by Gender",
       x = "Gender",
       y = "Sales Volume") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



## Company wise sales with respect to gender:-  

Company_wise_gender = mod.1 %>% 
  group_by(Company, Gender) %>%
  summarise(sales_volume = n()) %>%
  ungroup()
print(Company_wise_gender)
 # print(sales_volume)
 
Company_wise_gender = as.matrix(Company_wise_gender)
class(Company_wise_gender)

## Bar plot for Company_wise_gender sales:

ggplot(Company_wise_gender, aes(x = Company, y = sales_volume , fill = Gender)) +
  geom_col(position = "dodge") + # Use dodge to show bars side-by-side
  labs(title = "Company wise gender",
       x = "Company ",
       y = "sales_volume",
       fill = "Gender") +
  theme_minimal() 

## The gender wise sales analysis indicates a significant gender difference with 18798 male 
# buyers as compare to female with 5108 sales volume. It represent that car market is male oriented 
# and shows companies to target marketing efforts aimed at female consumer to increase their 
# representation in car sales.

# When examining the performance of different car brands across these demographics, it is clear
# that certain brands resonate more strongly with male buyers. For instance, Chevrolet and Dodge 
# lead with sales figures of 1,362 and 1,246 units respectively, demonstrating their strong appeal 
# in the market. Additionally, brands like Ford and Mercedes-Benz also show significant sales, with
# 1,181 and 926 units sold, respectively.

# In contrast, brands such as Infinite and Jaguar have relatively lower sales figures, with only 150 
# and 145 units sold, indicating they may be less popular among the current consumer base or may cater to a  market.


#The data suggests that car manufacturers should consider the varying preferences across demographics.
#Strategies aimed at enhancing female engagement, such as marketing campaigns and focusing on vehicle
#attributes that resonate with women (like safety, eco-friendliness, and technology features), could help 
#improve sales among female consumers.


## Calculate sales volume by region

Sales_by_region = mod.1 %>%
  group_by(Dealer_Region) %>%
  summarise(sales_volume = n()) %>%
  ungroup()
print(Sales_by_region)

# Sorted in descending order:-

Sales_by_region = arrange(Sales_by_region, desc(sales_volume))
print(Sales_by_region)

# Bar plot for  Region wise sales:-

ggplot(Sales_by_region, aes(x = Dealer_Region, y = sales_volume , fill =sales_volume )) +
  geom_col(position = "dodge") + # Use dodge to show bars side-by-side
  labs(title = "Region wise Sales",
       x = "Dealer_Region ",
       y = "sales_volume",
       fill = "sales_volume") +
  theme_minimal() 

# According to the analysis Austin is a region having highest sales of 4135. it shows strong market 
# presence and high demand of vehicles.

# On the 2nd and 3rd place comes Janesville and Scottsdale with 3821 and 3433 sales volume
# these cities also have high demand of vehicles and  good presence of market.
# Other cities like  Aurora, Greenville, Middletown and Pasco indicates less sales                                           
# The overall distribution highlights that while some regions demonstrate robust automotive sales, 
#others may require targeted marketing strategies to stimulate growth and improve sales performance.





## REGIONAL ANALYSIS:- 
# In REGIONAL ANALYSIS we will analyse car sales across different geographical distribution.

# Calculate sales volume with respect to regions

regional_sales<- mod.1 %>%
  group_by(Company,Dealer_Region)  %>%
  summarise(sales_volume = n()) %>%
  mutate(Dealer_Region = as.character(Dealer_Region)) %>%
  ungroup()
#print(class(regional_sales))
print(head(regional_sales))

regional_sales_step2 <- regional_sales %>%
  mutate(Dealer_Region= as.character(Dealer_Region)) %>%
  arrange(Dealer_Region,desc(sales_volume))
print(regional_sales_step2,n=210)

#  Top 3 companies per region:- 

# top_3_regions <-  regional_sales_step2  %>%
  
#top_3_companies = head(n= 3,regional_sales$Company)
#print(top_3_companies)

# Bottom 3 region:-
#bottom_3_company = tail(n = 3,regional_sales$Company)
#print(bottom_3_company)

## Analysis according to region and gender with respect to sales volume

genderwise_regions_sales <- mod.1 %>%
  group_by(Gender,Dealer_Region) %>%
  summarise(sales_volume = n())
print(genderwise_regions_sales)

# Calculate total sales of each region
total_salesper_region <- regional_sales %>%
  group_by(Dealer_Region) %>%
  summarise(total_sales = sum(sales_volume, na.rm = TRUE))

print(total_salesper_region)

# Calculate region wise percentage:-

regionwise_percentage <- genderwise_regions_sales$sales_volume/total_salesper_region$total_sales*100
gender_distribution

# Region wise Sales with respect to gender:-

ggplot(genderwise_regions_sales, aes(x = sales_volume , y = Dealer_Region , fill = Gender)) +
  geom_col(position = "dodge") + # Use dodge to show bars side-by-side
  labs(title = "Region wise Sales",
       x = "sales_volume ",
       y = "Dealer_Region",
       fill = "Gender") +
  theme_minimal() 

# Created data frame to show data:-

over_all_sales_gender.region <- data.frame(genderwise_regions_sales,regionwise_percentage)
print(over_all_sales_gender.region)

# The above analysis shows that car market or automobile market is dominated by male with 79 to 77% sales.
# This indicates a trend of male being primary consumers of vehicles.

# On the other hand females are consistent but lower 22 - 20% of sales in all regions.
# This indicate poor strategies by dealer and have great opportunity to  develop the market 
# with female consumers in all region.


# Calculate region wise annual income

region_wies_annualincome <- mod.1 %>%
  group_by(Dealer_Region) %>%
  summarise( Annual.Income = mean(Annual.Income)) %>%
  ungroup()
print(region_wies_annualincome)
# Sorted in descending order
region_wies_annualincome <- arrange(region_wies_annualincome, desc(Annual.Income))
print(region_wies_annualincome)
region_wies_annualincome <- as.matrix(region_wies_annualincome)

class(Annual.Income)
Annual.Income <- as.numeric(Annual.Income)
class(Annual.Income)

# bar plot to visualize Average Income by Region

ggplot(region_wies_annualincome, aes(x = as.numeric(Annual.Income) , y = Dealer_Region, fill = Annual.Income )) +
  geom_col(position = "dodge" , fill = "skyblue") +
  labs(title = "Average Income by Region",
       x = "Annual.Income ",
       y = "Dealer_Region",
      ) ++
  theme_minimal() 

## Calculate average car prize by region:-
 average_carprize_region  <- mod.1 %>%
   group_by(Dealer_Region) %>%
   summarise(averagecar_price= mean(Price....))
print(average_carprize_region)

average_carprize_region = arrange(average_carprize_region, desc(averagecar_price))
print(average_carprize_region)

# bar plot to visualize Average car price by Region
ggplot(average_carprize_region, aes(x = as.numeric(averagecar_price) , y = Dealer_Region, fill = Annual.Income )) +
  geom_col(position = "dodge" , fill = aes("yellow","red","skyblue","green","pink","orange","purple") ) +
  labs(title = "Average car price by Region",
       x = "Average car price ",
       y = "Dealer_Region",
  ) +
  theme_minimal() 
# print all outputs:-

print(region_wies_annualincome)
print(average_carprize_region)

# According to our analysis income if each and every 
# region is on same scale. With Pasco on top with 853975.3 and 
# Scottsdale on bottom with 805682.7 annual income.

# Analysis according to region wise average car price of 28342 in Austin and 27833 in
# Janesville shows much more less variation in car prices. And have over all same price 
# range in all regions.


# COLOUR AND BODY TYPE ANALYSIS:-  

# In this section we will analyse colors and body types of cars which attracts the most customers and 
# Dominate the market. This will help us to broadly understand market trends.

colour  = unique(mod.1$Color)
colour_count = table(mod.1$Color)

colour_count <- data.frame(colour_count)
colour_count
colour_count <- arrange(colour_count, desc(Freq))
print(colour_count)


# bar plot to visualize color :- 

costum_pallet  <- c("black", "#F5F5F5", "red")
ggplot(colour_count, aes( x =Var1 ,  y = Freq )) +
  geom_col(position = "dodge" , fill = costum_pallet ) +
  labs(title = "Average car price by Region",
       x = "frequency ",
       y = "colour",
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "skyblue", color = NA)) +
  labs(title = "Barplot with Custom Background", x = "Color", y = "Count")+
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank() ) + 
  labs(title = "Barplot Without Grid Lines", x = "Color", y = "Count")

print(colour_count)

## as the visualization indict the choice of color is lean towards the pale withe color
# with 11256sales, which looks classy look on road and have become customers first preference followed by 
# black as legendary choice of customers with 7857 sales. And at the bottom we have red color with 4793 sales 
# preferred less frequently by customers.

gender_wise_color <- mod.1 %>%  
  group_by(Color, Gender)  %>%
  summarise(grnder_color_choice = n())  %>%
  ungroup()

gender_wise_color

#  gender_wise_color choice bar chart 

ggplot(gender_wise_color, aes(x = Color, y = grnder_color_choice, fill = Gender)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  labs(
    title = "Gender-wise Color Choice",
    x = "Color",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal()

print(gender_wise_color)

# The above analysis shows gender wise color selection. Black is refereed by  men's by 6204 sales
# and women's by 1653 sales. Pale white 8808 by men's and 2448 by women's and red color  
# 3786 by men's and 1007 by female sales. 

# Annual income, region wise distribution:-
attach(mod.1)
hist(Annual.Income)
# The majority of individuals in the data set seem to have annual incomes below 
# 500,000, with a very high frequency around the 0 to 250,000 range.
# Higher incomes, especially above 1,500,000, occur much less frequently, contributing to the long tail on the right.
# This type of distribution often represents a population where a small number of individuals earn significantly 
# higher incomes, while most fall within a lower income range.


##  CORELATION BETWEEN ANUUAL INCOME AND CAR PRIZE:-

attach(mod.1)
corelation = cor(Annual.Income, Price....)
corelation

ggplot(data = mod.1, aes(x = Annual.Income, y = Price....)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Annual Income vs Car Price",
    x = "Annual Income",
    y = "Car Price"
  ) +
  theme_minimal()
# As we have get the correlation vale of 0.1 it indicates weak positive correlation between Annual Income and Price 
# Variable. It means not any variable depend on another. They are independent on there own way. 
# Therefore analysis did not shows that higher annual income people buy expensive cars.


# ENGINE AND TRANSMISSION ANALYSIS:-

# Engine Analysis:-
Engine = unique(mod.1$Engine)
Engine = table(mod.1$Engine)
Engine<- data.frame(x)
Engine

colnames(Engine) <- c("Engine", "Count")
Engine
#  bar plot for Engine data:-
ggplot(data = Engine, aes(x = Count, y = Engine)) +
  geom_bar(stat = "identity", fill = c("lightpink","lightgreen"), width = 0.3) +
  theme_minimal() +
  labs(title = "Frequency of Engine Types",
       x = "Engine Type",
       y = "Count")
print(Engine)

## The analysis shows that DoubleÂ Overhead Camshaft is mostly prefered by 12571 customers.
# and Overhead Camshaft is prefered by 11335 Consumers.However, the difference is not significant,
# indicating a relatively balanced consumer preference between the two engine types.

# Transmission Analysis:-

transmission = unique(mod.1$Transmission)
transmission = table(mod.1$Transmission)
transmission= data.frame(transmission)
transmission
colnames(transmission) <- c("transmission", "Count")
transmission
# Bar plot to visualize data :
ggplot(data = transmission, aes(x = Count, y = transmission )) +
  geom_bar(stat = "identity", fill = c("yellow","lightblue"), width = 0.3) +
  theme_minimal() +
  labs(title = "Frequency of Engine Types",
       x = "Transmission",
       y = "Count")
print(transmission)

# As the analysis indicates both the count of manual with 11335 and Auto with 12571
# transmission type prefered by most people is Automatic as compared to Manual. 
# transmission type choices are some were depend on capacity to engins. Cars with robuts engin have 
# auto transmission type and vice versa for manual.

## MARKET TREND:-
# Extracted month and year from the Date Column
Date = mod.1$Date
Date 
class(Date)

Date = as.character(Date)
Date = as.Date(Date, format =  "%m/%d/%Y")
print(Date)

month_years = format(Date, "%Y/%m")  # "03" → Month
print(month_years)
unique(month_years)

# added month column in data set mod.1:- 

mod.1$month_years <- month_years
colnames(mod.1)
table(mod.1$month_years)

# Further analysis will include Revenue, total sales with respect to month:-

# Month wise Average price 

average_monthly_car_sales = mod.1 %>%
  group_by(month_years) %>% 
  summarise(average_price= mean(Price....)) %>% 
  ungroup()
# Arranged in descending order
average_monthly_car_sales <- arrange(average_monthly_car_sales, desc(average_price))
print(average_monthly_car_sales, n = 24) 

# Line Chart to analyse Month wise Average price 

ggplot(average_monthly_car_sales, aes(x = month_years, y = average_price, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Line
  geom_point(color = "red", size = 2) +  # Points on the line
  labs(
    title = "Average Monthly Car Sales Price",
    x = "month_years",
    y = "Average Price"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Month wise Total Sales:-


Totalsales_monthwise <-  mod.1  %>%
  group_by(month_years ) %>% 
  summarise( total_sales = n() ) %>% 
  ungroup()
# Arranged in descending order
Totalsales_monthwise  <- arrange(Totalsales_monthwise, desc(total_sales))
print(Totalsales_monthwise, n= 24)

# Line Chart to analyse Month wise Total  Sales:-

ggplot(Totalsales_monthwise, aes(x = month_years, y = total_sales, group = 1)) +
  geom_line(color = "darkgreen", size = 1) +  # Line
  geom_point(color = "orange", size = 2) +    # Points on the line
  labs(
    title = "Total Car Sales by Month",
    x = "month_years",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Revenue generated Monthly:-

montly_revenue <- mod.1   %>%
  group_by(month_years)    %>%
  summarise( revenue =  sum(Price....))    %>%
  ungroup()

montly_revenue <- arrange(montly_revenue, desc(revenue))
print(montly_revenue, n= 24)

# line chart for Revenue generated Monthly:-

ggplot(montly_revenue, aes(x = month_years, y = revenue, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Car Sales Revenue",
    x = "Month",
    y = "Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# According to Analysis Average car prize is almost on same price range, some noticeable  
# ups in August 2022 with 29175 average price and dips in March 2023 with 26752. 


# Sales also experience some shift in September 2022 with 1475 sales units and continuous  
# spike in November and December with 1620 and 1625 sales unit and 1625, and most peak 
# December 2023 with 1921 and November 2023 with 1850 sales unit. 
# This indicate the peak sales time and derive much higher revenue. 

# Revenue correlates strongly with total sales, with the highest revenues recorded during 
# months of high sales (e.g., November 2023 with 51.5M and December 2023 with 54.2M).









