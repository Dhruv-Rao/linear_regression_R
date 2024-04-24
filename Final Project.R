library(pacman)
library(RMySQL)
library(ggplot2)
p_load(tidyverse,rpart,tidymodels)
require(gridExtra)
library(GGally)
library(caret)

mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='cmsc398e',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='T3#mPsWr1505')
# Shows all tables in schema
dbListTables(mysqlconnection)

# This project was only worked on by Dhruv Rao and nobody else

# PROBLEM SUMMARY:
# Data Source: https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023/
# The table I will be using is a table of statistics about various world
# countries.I want to analyze how GDP per capita is affected by various other
# country-related factors, such as birth rate and life expectancy, among others.
# My hypothesis is that percentage of agricultural land, birth rate, fertility
# rate, infant mortality, and unemployment rate negatively correlate to GDP per
# capita, while life expectancy, minimum wage, urban population, and gross
# primary education enrollment positively correlate to GDP per capita. GDP per
# capita tends to be accepted as one of the core statistics in determining
# economic status of a country, so it would be interesting to see if other
# statistics about a country can be predictors of the state of a country's
# economy.

# SQL SECTION COMMENTS BELOW

# T1 cleans up the data table by removing percentage signs and dollar signs using the substring and length
# string methods, and extracting the columns we actually care about.
# T2 uses the average functionality to fill in null/empty values in gdp and population fields with their average, by
# using a case-when statement to distinguish between empty entries and filled ones.
# T3 creates a new column gdp per capita by diving each gdp entry by its corresponding population entry
# T4 uses a rank function to rank countries by gdp per capita in descending order and then order the table in
# descending gdp per capita order.
# T5 uses a case-when statement to sort the countries into 4 tiers of gdp per capita, with Tier 1 having the highest
# and Tier 4 having the lowest. These tiers are about the same size, which is approximately 45, because there are 180 countries.
# The final select statement uses a series of case-when statements, round and average functions, a partition by function to fill in
# all null values in the remaining columns (so not gdp, population, or gdp per capita) with the average of that column over the
# rows in the same gdp per capita tier as that country, and casting to integer types to ensure all the data types are consistent.
# We are using a chain of CTEs to build the final table, along with filling in null/empty values, aggregate functions like average,
# a partition by to only calculate average over the same gdp per capita tier, case-when statements to filter null/empty values,
# a rank function to sort and order by gdp per capita, and string functions like substring and length to clean the values and convert
# them into integer form.
query = "
  With T1 as (
    select substring(`Agricultural Land( %)`, 1, length(`Agricultural Land( %)`)-1) as agricultural_land,
    `Birth Rate` as birth_rate, `Fertility Rate` as fertility_rate,
    substring(`GDP`, 2, length(`GDP`)) as gdp, 
    substring(`Gross primary education enrollment (%)`, 1, length(`Gross primary education enrollment (%)`)-1)
    as primary_education,`Infant mortality` as infant_mortality, `Life expectancy`
    as life_expectancy, substring(`Minimum wage`, 2, length(`Minimum wage`)) as minimum_wage, `Population` as
    population, substring(`Unemployment rate`, 1, length(`Unemployment rate`)-1) as unemployment_rate, `Urban_population`
    as urban_population
    from world_data_2023),
  T2 as(
    select agricultural_land, birth_rate, fertility_rate,
    case when gdp like '' then (avg(gdp)) else gdp end as gdp_fixed, primary_education, infant_mortality,
    life_expectancy, minimum_wage, case when population like '' then (avg(population)) else population end as population_fixed,
    unemployment_rate, urban_population from T1
    group by agricultural_land, birth_rate, fertility_rate, primary_education, gdp, infant_mortality, life_expectancy,
    minimum_wage, population, unemployment_rate, urban_population),
  T3 as(
    select agricultural_land, birth_rate, fertility_rate, gdp_fixed, primary_education, infant_mortality, life_expectancy,
    minimum_wage, population_fixed, unemployment_rate, urban_population, gdp_fixed/population_fixed as gdp_per_capita
    from T2),
  T4 as(
    select agricultural_land, birth_rate, fertility_rate, gdp_fixed, primary_education, infant_mortality, life_expectancy,
    minimum_wage, population_fixed, unemployment_rate, urban_population, gdp_per_capita, rank() over (order by gdp_per_capita desc) gdp_per_capita_rank from T3),
  T5 as(
    select agricultural_land, birth_rate, fertility_rate, gdp_fixed, primary_education, infant_mortality, life_expectancy, minimum_wage,
    population_fixed, unemployment_rate, urban_population, gdp_per_capita, gdp_per_capita_rank, case when gdp_per_capita_rank <= 45 then 1
    when gdp_per_capita_rank > 45 and gdp_per_capita_rank <= 90 then 2 when gdp_per_capita_rank > 90 and gdp_per_capita_rank <= 135 then 3 else 4 end as gdp_per_capita_tier from T4)
  select case when agricultural_land like '' then cast(round((avg(agricultural_land) over (partition by gdp_per_capita_tier)), 2) as decimal(5,2)) else cast(agricultural_land as decimal(5,2)) end as agricultural_land_fixed,
  case when birth_rate like '' then round((avg(birth_rate) over (partition by gdp_per_capita_tier)), 2) else birth_rate end as birth_rate_fixed,
  case when fertility_rate like '' then round((avg(fertility_rate) over (partition by gdp_per_capita_tier)), 2) else fertility_rate end as fertility_rate_fixed, gdp_fixed,
  case when primary_education like '' then cast(round((avg(primary_education) over (partition by gdp_per_capita_tier)), 2) as decimal(5,2)) else cast(primary_education as decimal(5,2)) end as primary_education_fixed,
  case when infant_mortality like '' then round((avg(infant_mortality) over (partition by gdp_per_capita_tier)), 2) else infant_mortality end as infant_mortality_fixed,
  case when life_expectancy like '' then cast(round((avg(life_expectancy) over (partition by gdp_per_capita_tier)), 2) as decimal(5,2)) else cast(life_expectancy as decimal(5,2)) end as life_expectancy_fixed,
  case when minimum_wage like '' then cast(round((avg(minimum_wage) over (partition by gdp_per_capita_tier)), 2) as decimal(5,2)) else cast(minimum_wage as decimal(5,2)) end as minimum_wage_fixed, population_fixed,
  case when unemployment_rate like '' then cast(round((avg(unemployment_rate) over (partition by gdp_per_capita_tier)), 2) as decimal(5,2)) else cast(unemployment_rate as decimal(5,2)) end as unemployment_rate_fixed,
  case when urban_population like '' then cast(round((avg(urban_population) over (partition by gdp_per_capita_tier)), 2) as decimal(10,0)) else cast(urban_population as decimal(10,0)) end as urban_population_fixed,
  gdp_per_capita, gdp_per_capita_rank, gdp_per_capita_tier from T5 order by gdp_per_capita_rank;"

# R SECTION BELOW

result = dbSendQuery(mysqlconnection, query) 
# Stores resulting table as dataframe
df = fetch(result)

# Plot each of our 9 possible predictors in a separate scatter plot, with the predictor as the x-axis
# and the GDP per capita we calculated on the y-axis, to see if there are any trends that arise.
agricultural_land <- data.frame(al = df$agricultural_land_fixed, gdppc = df$gdp_per_capita)
scatterplot1 <- ggplot(agricultural_land, aes(x = al, y = gdppc)) + geom_point()
birth_rate <- data.frame(br = df$birth_rate_fixed, gdppc = df$gdp_per_capita)
scatterplot2 <- ggplot(birth_rate, aes(x = br, y = gdppc)) + geom_point() 
fertility_rate <- data.frame(fr = df$fertility_rate_fixed, gdppc = df$gdp_per_capita)
scatterplot3 <- ggplot(fertility_rate, aes(x = fr, y = gdppc)) + geom_point() 
primary_education <- data.frame(pe = df$primary_education_fixed, gdppc = df$gdp_per_capita)
scatterplot4 <- ggplot(primary_education, aes(x = pe, y = gdppc)) + geom_point() 
infant_mortality <- data.frame(im = df$infant_mortality_fixed, gdppc = df$gdp_per_capita)
scatterplot5 <- ggplot(infant_mortality, aes(x = im, y = gdppc)) + geom_point() 
life_expectancy <- data.frame(le = df$life_expectancy_fixed, gdppc = df$gdp_per_capita)
scatterplot6 <- ggplot(life_expectancy, aes(x = le, y = gdppc)) + geom_point() 
minimum_wage <- data.frame(mw = df$minimum_wage_fixed, gdppc = df$gdp_per_capita)
scatterplot7 <- ggplot(minimum_wage, aes(x = mw, y = gdppc)) + geom_point() 
unemployment_rate <- data.frame(ur = df$unemployment_rate_fixed, gdppc = df$gdp_per_capita)
scatterplot8 <- ggplot(unemployment_rate, aes(x = ur, y = gdppc)) + geom_point()
urban_population <- data.frame(up = df$urban_population_fixed, gdppc = df$gdp_per_capita)
scatterplot9 <- ggplot(urban_population, aes(x = up, y = gdppc)) + geom_point()
grid.arrange(scatterplot1, scatterplot2, scatterplot3, scatterplot4, scatterplot5, scatterplot6, scatterplot7, scatterplot8, scatterplot9, ncol=3, nrow = 3)
# It takes a bit to load all 9 plots in. From top left, going left to right and then top to bottom:
# Agricultural Land % vs GDP Per Capita, Birth Rate vs GDP Per Capita, Fertility Rate vs GDP Per Capita,
# Gross Primary Education Enrollment % vs GDP Per Capita, Infant Mortality vs GDP Per Capita, Life Expectancy vs GDP Per Capita,
# Minimum Wage vs GDP Per Capita, Unemployment Rate vs GDP Per Capita, Urban Population vs GDP Per Capita

# HYPOTHESIS:
# Based on the scatter plot trends, I am going to update my original hypothesis from the start of this project.
# Now, I am predicting that that life expectancy and minimum wage have a positive correlation to GDP per capita,
# birth rate, fertility rate, infant mortality, unemployment rate, and urban population have a negative correlation
# to GDP per capita, and agricultural land percentage and gross primary school enrollment % have no correlation to GDP
# per capita.

# Linear Regression

# Randomly splits data into train/test data. 
# 75% is train_data
# 25% is test_data
data_split <- 
  df %>%  rsample::initial_split(
    data = ,
    prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

train_index <- createDataPartition(df$gdp_per_capita, p = 0.75, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# R's default null hypothesis for linear regression models is that there is no
# correlation between the independent variables being tested and the dependent
# variable. So a p-value of less than 0.05 tells us that this null hypothesis is
# in fact rejected, so we can safely assume there is a correlation between the
# variables.

# Linear regression model between percentage of agricultural land and gdp per
# capita
linear_model1 = lm(gdp_per_capita ~ agricultural_land_fixed, data = train_data)
predictions1 = predict(linear_model1, 
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions1))
# Based on this particular test I got a p-value of 0.104, which suggests that
# there is no linear correlation between percentage of agricultural land and
# gdp per capita. This conclusion is mostly backed up by the scatter plot. We
# accept the null hypothesis here.

# Linear regression model between birth rate and gdp per capita
linear_model2 = lm(gdp_per_capita ~ birth_rate_fixed, data = train_data)
predictions2 = predict(linear_model2, 
                      newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions2))
# This particular test produced a very low p-value of 0.002417, which suggests
# that there should be a strong linear correlation between birth rate and gdp
# per capita. This is backed up by the scatter plot, which shows a strong
# negative linear relationship between the two variables. We reject the null
# hypothesis here.

# Linear regression model between fertility rate and gdp per capita
linear_model3= lm(gdp_per_capita ~ fertility_rate_fixed, data = train_data)
predictions3 = predict(linear_model3,
                      newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions3))
# This particular test produced a very low p-value of 0.009735, which suggests
# that there should be a strong linear correlation between fertility rate and 
# gdp per capita. This is backed up by the scatter plot, which shows a strong
# negative linear relationship between the two variables. We can reject the
# null hypothesis here.

# Linear regression model between primary education enrollment percentage and
# gdp per capita
linear_model4= lm(gdp_per_capita ~ primary_education_fixed, data = train_data)
predictions4 = predict(linear_model4,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions4))
# Based on this particular test I got a p-value of 0.229, which suggests that
# there is no linear correlation between primary education enrollment percentage
# and gdp per capita. This conclusion is mostly backed up by the scatter plot.
# We can accept the null hypothesis here.

# Linear regression model between infant mortality and gdp per capita
linear_model5= lm(gdp_per_capita ~ infant_mortality_fixed, data = train_data)
predictions5 = predict(linear_model5,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions5))
# This particular test produced a very low p-value of 0.005733, which suggests
# that there should be a strong linear correlation between infant mortality rate
# and gdp per capita. This is backed up by the scatter plot, which shows a
# strong negative linear relationship between the two variables. We can reject
# the null hypothesis here.

# Linear regression model between life expectancy and gdp per capita
linear_model6= lm(gdp_per_capita ~ life_expectancy_fixed, data = train_data)
predictions6 = predict(linear_model6,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions6))
# This particular test produced a very low p-value of 0.004319, which suggests
# that there should be a strong linear correlation between life expectancy
# and gdp per capita. This is backed up by the scatter plot, which shows a
# strong positive linear relationship between the two variables. We can reject
# the null hypothesis here.

# Linear regression model between minimum wage and gdp per capita
linear_model7= lm(gdp_per_capita ~ minimum_wage_fixed, data = train_data)
predictions7 = predict(linear_model7,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions7))
# Based on this particular test I got a p-value of 0.6437, which suggests that
# there is no linear correlation between minimum wage and gdp per capita.
# This conclusion is mostly backed up by the scatter plot. We can accept the
# null hypothesis here.

# Linear regression model between unemployment rate and gdp per capita
linear_model8= lm(gdp_per_capita ~ unemployment_rate_fixed, data = train_data)
predictions8 = predict(linear_model8,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions8))
# Based on this particular test I got a p-value of 0.8877, which suggests that
# there is no linear correlation between unemployment rate and gdp per capita.
# This conclusion is mostly backed up by the scatter plot. We can accept the
# null hypothesis here.

# Linear regression model between urban population and gdp per capita
linear_model9= lm(gdp_per_capita ~ urban_population_fixed, data = train_data)
predictions9 = predict(linear_model9,
                       newdata = test_data)
summary(lm(test_data$gdp_per_capita~predictions9))
# Based on this particular test I got a p-value of 0.7571, which suggests that
# there is no linear correlation between urban population and gdp per capita.
# This conclusion is mostly backed up by the scatter plot. We can accept the
# null hypothesis here.

# CONCLUSIONS/SUMMARY OF FINDINGS
# Overall, my updated hypothesis was proven partially correct. I was correct in
# predicting that life expectancy had a positive correlation to gdp per capita,
# but incorrect in assuming that minimum wage did so as well (it had no
# verifiable correlation). I was correct in assuming that birth rate, fertility
# rate, and infant mortality had a negative correlation to GDP per capita.
# However, I was incorrect in assuming that unemployment rate and urban
# population had a negative correlation to GDP per capita (they had no verifiable
# correlation). I was correct in assuming that agricultural land percentage and
# gross primary school enrollment percentage had no verifiable correlation to
# GDP per capita. Some possible sources of error in the experiment could arise
# from a low sample size of countries to check trends over, which can lead to
# varying p-values that may change my understanding of if there is a correlation
# or not. Given how low their p-values were, though, I can safely say that
# life expectancy is a reliable positive predictor of gdp per capita, and 
# birth rate, fertility rate, and infant mortality are reliable negative
# predictors of GDP per capita.


