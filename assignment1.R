# Author: Sutulova Tatiana, 30806151
# Assignment 1
# Describe the data overall, including things such as dimension, data types, 
# distribution of numerical attributes, variety of non-numerical (text) 
# attributes, missing values, and anything else of interest or relevance.

# Libraries and packages to be used
library(dplyr)
library(finalfit) 
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# Read the data from the file into a data frame
rm (list = ls())
set.seed(30806151) 
cvbase = read.csv("PsyCoronaBaselineExtract.csv", header = TRUE, na.strings = c("", " ", "NA")) # replacing empty spaces with NA
cvbase <- cvbase [sample(nrow(cvbase), 40000),] # take a sample of 40000

# Pre-processing: 
# Clear out duplicates
cvbase <- unique(cvbase)

# Remove rows where there is no country name
cvbase = cvbase[!is.na(cvbase$coded_country),]

# Placing the column coded_country to the front for easier processing
cvbase <- cvbase %>% relocate(coded_country)

# Remove rows where everything except country name is NA
cvbase <- cvbase %>% filter(!if_all(affAnx:c19ProSo04, is.na))

# Task 1: 
# Dimensions of data
print(ncol(cvbase))

# Data types
str(cvbase)

# Distribution of numerical attributes
par(mar=c(10,5,2,2))
boxplot(cvbase[2:21], las = 2)
boxplot(cvbase[32:54], las = 2)

# Variety of non-numerical (text) attributes
# Find how many countries are recorded in the data set 
unique_val <- unique(cvbase$coded_country)
length(unique_val)

# List down how many times each country is met
print(table(cvbase$coded_country))

# Create a pie chart to show top 20 countries
countries <- as.data.frame(table(cvbase$coded_country))

top20_countries <- countries %>%
  arrange(desc(Freq)) %>% 
  slice(1:20)

pie(top20_countries$Freq, labels = top20_countries$Var1, col = rainbow(20)) # show a pie chart

# Missing values
cvbase %>% missing_plot() # plot missing values

# Task 2:
# Preparing data
# Extracting responses of Indonesia into a new df
cvbase_focus = cvbase %>% filter(coded_country == c("Indonesia"))
# Extracting responses of all other countries except the focus one into a new df
cvbase_others = filter(cvbase, coded_country != "Indonesia")

# Replace all NA values for employment status with 0
cvbase_focus[c(22:31)][is.na(cvbase_focus[c(22:31)])] <- 0
# Remove all the rows with NA values
cvbase_focus <- na.omit(cvbase_focus)
cvbase_focus$coded_country <- NULL

# Replace all NA values for employment status with 0
cvbase_others[c(22:31)][is.na(cvbase_others[c(22:31)])] <- 0
# Remove all the rows with NA values
cvbase_others <- na.omit(cvbase_others)
cvbase_others$coded_country <- NULL

# Part A
# drawing a box plot to see the distribution for focus country
par(mar=c(10,5,2,2))
boxplot(cvbase_focus[c(1:20, 31:53)], las = 2, col = "deepskyblue")

par(mar=c(10,5,2,2))
boxplot(cvbase_others[c(1:20, 31:53)], las = 2, col = "hotpink")

# Part B
# Calculating the correlation with all the attributes for c19ProSo01,2,3 and 4

# Creating a linear regression model to check significant predictors in the model summary
fit01 <- lm(c19ProSo01~ ., data = cvbase_focus[1:50]) #c19ProSo01
summary(fit01)

fit02 <- lm(c19ProSo02~ ., data = cvbase_focus[c(1:49, 51)]) #c19ProSo02
summary(fit02)

fit03 <- lm(c19ProSo03~ ., data = cvbase_focus[c(1:49, 52)]) #c19ProSo03
summary(fit03)

fit04 <- lm(c19ProSo04~ ., data = cvbase_focus[c(1:49, 53)]) #c19ProSo04
summary(fit04)

# Part C
# Calculating the correlation with all the attributes for c19ProSo01,2,3 and 4
fit1.1 <- lm(c19ProSo01~ ., data = cvbase_others[1:50]) #c19ProSo01
summary(fit1.1)

fit1.2 <- lm(c19ProSo02~ ., data = cvbase_others[c(1:49, 51)]) #c19ProSo02
summary(fit1.2 )

fit1.3 <- lm(c19ProSo03~ ., data = cvbase_others[c(1:49, 52)]) #c19ProSo03
summary(fit1.3)

fit1.4 <- lm(c19ProSo04~ ., data = cvbase_others[c(1:49, 53)]) #c19ProSo04
summary(fit1.4)

# Task 3
# Part A
# Reading the database that was created based on several social, economic, health and policial indicators
countriesdf <- read.csv("Countries.csv")

# Make the countries as row names 
rownames(countriesdf) <- countriesdf$coded_country
countriesdf$coded_country <- NULL

# Scale numerical data
countriesdf = scale(countriesdf)

# Performing clustering and plotting
countriesdf_kfit = kmeans(countriesdf, 7, nstart = 20)
fviz_cluster(countriesdf_kfit, data = countriesdf)

#Part B
# Extracting responses of similar countries into a new df
cvbase_cluster = cvbase %>% filter(coded_country == c("India","Tukey", "Russia", "Vietnam", "Saudi Arabia", "Philippines", "Tunisia"))

# Prepare "clusters" for linear regression

# Replace all NA values for employment status with 0
cvbase_cluster[c(22:31)][is.na(cvbase_cluster[c(22:31)])] <- 0

# Remove all the rows with NA values
cvbase_cluster <- na.omit(cvbase_cluster)

# Calculating the correlation with all the attributes for c19ProSo01,2,3 and 4
# Remove the coded_country column as all are considered to be in the same group
cvbase_cluster$coded_country <- NULL

fit2.1 <- lm(c19ProSo01~ ., data = cvbase_cluster[1:50]) #c19ProSo01
summary(fit2.1)

fit2.2 <- lm(c19ProSo02~ ., data = cvbase_cluster[c(1:49, 51)]) #c19ProSo02
summary(fit2.2)

fit2.3 <- lm(c19ProSo03~ ., data = cvbase_cluster[c(1:49, 52)]) #c19ProSo03
summary(fit2.3)

fit2.4 <- lm(c19ProSo04~ ., data = cvbase_cluster[c(1:49, 53)]) #c19ProSo04
summary(fit2.4)


