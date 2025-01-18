install.packages("caret", dependencies = c("Depends", "Suggests")) 

install.packages("rpart")  

install.packages("rpart.plot")  

install.packages("forecast")  

library(caret)  

library(rpart)  

library(rpart.plot)  

library(forecast) 





# Load necessary libraries 

library(readr) # For reading CSV files 

library(dplyr) # For data manipulation 

library(ggplot2) # For data visualization 



suppressWarnings(RNGversion("3.5.3"))  



options(scipen=999)  

library(caret)  

library(rpart)  

library(rpart.plot)  

library(forecast)  

library(readxl)  

options(scipen=999)  



##    1&2)  Executive Summary (detailed in our proposal) 



# Our project examines a detailed flight booking dataset from "Ease My Trip" on Kaggle, a key player in online flight booking.  

# As digital platforms become vital for travelers' journey planning, we aim to understand, compare,  

# and predict flight prices involving some key variables and six different airlines in this evolving digital landscape. 





# Question: In our study, we will try to answer the following question: 

#What are the key determinants of flight pricing, and how can those factors be used to predict price variations? 



##    3)   Business Understanding 

# Our project explores the digital transformation of the aviation industry, with a focus on online booking platforms. 

# We aim to dissect the intricate pricing strategies of six major airlines, examining the primary factors influencing price fluctuations.  

# Our goal is to create predictive models that estimate flight prices based on various influencing variables. 

# This analysis will benefit stakeholders and travelers alike by offering insights into the pros and cons of different booking options for their upcoming flights.  









##    4)   Data Understanding 

#a) Data requirement 

# The original data we extracted from Kaggle came volumetric (300,154 rows) and with variables such as the Airlines, the flights number,  

#flight departures and destinations, the number of stops of the flights, the departure time and arrival time (moment of the day), 

#the class of the flight (Economy or Business), duration of the flight, the days left before the flight and the price. 





#b) Describe Data 

# After taking a look at the data, we can describe the variables as follow: 



#Airlines Companies (SpiceJet, Vistara, Indigo, GO_FIRST, Air_India, AirAsia): Categorical variable representing different airlines, 

#used to assess how each airline's services and pricing impact our study. 



#Flights: Categorical variable indicating individual flight bookings. 



#Flight Departure Time: Categorical variable categorizing the booking time (e.g., morning, evening), to identify patterns affecting flight prices. 



#Number of Stops: Discrete numerical variable, considering only non-stop flights to focus on price impacts. 



#The Class: Categorical variable, Economy or Business class seats to analyze ticket price influences. 



#Duration: Continuous numerical variable, may directly influence the price of the flights. 



#Days Left: Discrete numerical variable showing booking lead time, used to explore how advance booking affects prices for different airlines. 



#Price: Continuous numerical variable, the dependent variable representing flight costs, crucial for understanding flight booking decisions and airline competitiveness. 



# Source and destination of the flight: Categorical variable. Indicating the departure and arrival city of the flight.  



#c) Quality 

# I would say this is a good quality data because of its consistency and not having null values. Some business fight price are extremely high and I had to take 

# a close look at some of those values. Apart from that, I think this data is good for an analysis. 





##   5)   Data Preparation 

#a) Data selection 

#Among all the variables we have listed above, we have inspected the variables and have chosen only those that was found be significant and helpful 

# for this analysis. Here is a breakdown of what we found. 



# 1) First, we consider the potential variables that could affect the price, meaning that could be useful in building our model. 

# The Airlines, The Departure time, The Class, Duration, the Days left.  



# 2) We then check the relationship of each variable with the dependent variable while checking for outlier with different possible plots. 

# Comment 1: "Class": We notice the the Business class has some unsually extremely high flight prices and seem to have a little or no variation in prices  

# with the number of days left. We were able to visualize those trend with a scatter plot and a histogram plot. For simplicity we decided to use only "Economy" Class 

# Comment 2: The Departure time has two major changes in the averages, all the airline together and within the airlines. "Late Night" (lowder average price) and  

# and all the other moments of the day (Standard). We decided to group them into "Late Ngith" and "Not Late Night" 

# Commment 3: "Stops" are majoritary either "one" and "zero". to eliminate the influence some flights too many number of stops, we focused only only "One" and "Zero" 



#b) Data Cleaning & Preparation 

# Afte applying those changes to our original dataset, we now have our clean up dataset named "Simpler_Clean_Up" with 194,464 rows with five (5) entity varialbes: 

# Duration, Days_left, Stops, Departure Time, and Airlines. 

library(readr) 

Simpler_Clean_Up <- read_csv("C:/OneDrive/GRAD SCHOOL materials/FALL 2023/BUAN 6356 B. ANALYTICS with R/SEMESTER PROJECT -Personal/Simpler_Clean_Up.csv") 

myData <- Dummy_Variables_Created 

myData[194453,]
myData[145840,] 
myData[97227,] 
myData[48614,] 
head(myData)


# Scatter plot for Price
plot(myData$Price, main = "Scatter Plot for Price", xlab = "Index", ylab = "Price")

# Add ablines at specified values
abline(v = c(194453, 145840, 97227, 48614), col = c("red", "red", "red", "red"), lty = c(3, 3, 3, 3))


# Scatter plot for Price
plot(myData$Days_left, main = "Scatter Plot for Days Left", xlab = "Index", ylab = "Days Left")

# Add ablines at specified values
abline(v = c(194453, 145840, 97227, 48614), col = c("red", "red", "red", "red"), lty = c(3, 3, 3, 3))

# Assuming your data frame is named 'myData'

# Create intervals for the 'Price' variable
intervals <- cut(myData$Airline, breaks = c(0, 48613, 97226, 145839, 194453), include.lowest = TRUE)

myData_interval_1 <- myData[0:48613,]
myData_interval_2 <- myData[48614:97226,]
myData_interval_3 <- myData[97227:145839,]
myData_interval_4 <- myData[145840:194453,]

# Create a table of counts for each airline
airline_counts_1 <- table(myData_interval_1$Airline)
airline_counts_1
airline_counts_2 <- table(myData_interval_2$Airline)
airline_counts_2
airline_counts_3 <- table(myData_interval_3$Airline)
airline_counts_3
airline_counts_4 <- table(myData_interval_4$Airline)
airline_counts_4

airline_counts <- matrix(c(11673, 4012, 6992, 9349, 2928, 13659,
                           11674, 3215, 6750, 10049, 2206, 14719,
                           10371, 3741, 5504, 10712, 2402, 15883,
                           11015, 2884, 3522, 12272, 1475, 17446),
                         nrow = 4, byrow = TRUE,
                         dimnames = list(c("Partition 1", "Partition 2", "Partition 3", "Partition 4"),
                                         c("Air_India", "AirAsia", "Go First", "Indigo", "SpiceJet", "Vistara")))

# Specify custom colors for each partition
custom_colors <- c(rgb(0,0,0.8),rgb(0,0.2,0.8) ,rgb(0,0.4,0.8) ,rgb(0,0.6,0.8))

# Create a bar plot with custom colors
barplot(as.matrix(airline_counts), beside = TRUE, col = custom_colors, 
        main = "Airline Counts for Different Categories", 
        xlab = "Airline", ylab = "Count", 
        legend.text = rownames(airline_counts), 
        args.legend = list(x = "topleft", bty = "n", inset = c(0, -0.1)))

Basic_summary <- summary(myData) 

Basic_summary 

# Variable Selection 
plot(myData$Days_left, myData$Price,   
     main = "Scatter Plot of Days Left vs Price",   
     xlab = "Days Left",   
     ylab = "Price",   
     pch = 16,   
     col = rgb(0, 0, 1, 0.5), 
     cex = 0.2) 

# Add the regresion line to the plot 
model <- lm(Price ~ Days_left, data = myData) 
abline(model, col = "red") 



plot(myData$Duration, myData$Price,   
     main = "Scatter Plot of Price vs. Duration",   
     xlab = "Duration",   
     ylab = "Price",   
     pch = 16,   
     col = "blue",
     cex = 0.3)  

# Add a linear model trend line  
model_dur <- lm(Price ~ Duration, data = myData) 
abline(model_dur, col = "red")  
summary(model_dur) 

boxplot(Price ~ Stops, data = myData,   
                main = "Box Plot of Price by Number of Stops",   
                xlab = "Number of Stops",   
                ylab = "Price",  
                col = c("blue", "red"))  

average_prices_by_time <- aggregate(Price ~ Departure_time, data = myData, mean)  

# We create a bar plot for the average prices by departure time  
barplot(average_prices_by_time$Price,   
        names.arg = average_prices_by_time$Departure_time,  
        main = "Average Prices by Departure Time",   
        xlab = "Departure Time",   
        ylab = "Average Price",  
        col = c(rgb(0,0,0.8),rgb(0,0.2,0.8) ,rgb(0,0.4,0.8) ,rgb(0,0.6,0.8),rgb(0,0.75,0.8),rgb(0,0.9,0.8)), # Using a color spectrum 
        las = 2) # Rotate the labels on x-axis 

average_prices <- aggregate(Price ~ Airline, data = myData, mean)  
barplot(average_prices$Price,   
        names.arg = average_prices$Airline,   
        main = "Average Prices by Airline",   
        xlab = "Airline",   
        ylab = "Average Price",  
        las = 2, # Rotate the labels on x-axis  
        col = c(rgb(0,0,0.8),rgb(0,0.2,0.8) ,rgb(0,0.4,0.8) ,rgb(0,0.6,0.8),rgb(0,0.75,0.8),rgb(0,0.9,0.8))) # Color of the bars  

average_prices_matrix <- tapply(myData$Price,   
                                list(myData$Airline, myData$Departure_time), 
                                mean)  

average_prices_matrix[is.na(average_prices_matrix)] <- 0  
barplot_matrix <- as.matrix(average_prices_matrix)  
rownames(barplot_matrix) <- rownames(average_prices_matrix) 
legend_names <- colnames(barplot_matrix)  

barplot(height = barplot_matrix,   
        beside = TRUE,   
        col = c(rgb(0,0.3,0.8),rgb(0,0.4,0.8) ,rgb(0,0.5,0.8) ,rgb(0,0.6,0.8),rgb(0,0.7,0.8),rgb(0,0.8,0.8)),   
        main = "Average Prices by Airline and Departure Time",   
        xlab = "Airline",   
        ylab = "Average Price",  
        las = 2, # to make the airline names horizontal  
        names.arg = rownames(barplot_matrix), # setting the x-axis labels to airline names  
        space = c(0.3, 2)) 

legend("bottomright", 
       legend = legend_names, fill = c(rgb(0,0.3,0.8),rgb(0,0.4,0.8) ,rgb(0,0.5,0.8) ,rgb(0,0.6,0.8),rgb(0,0.7,0.8),rgb(0,0.8,0.8)), 
       cex = 0.5, # smaller font size  
       ncol = 2) # more columns  


# To make our current new data ready for analysis, we will need to use dummy variables for Stops, Departure time, and Airlines. 

# 1) For "Stops": We will use "0" or no stop flights and "1" for flights with at least one stop. 

# 2) For "Departure time": We will use "0" for "Late Night" and "1" for "Not Lat Night" 

# 3) For "Airlines": Since Vistara was the airline with the highest average prices, we have chosen that to be the reference. Now we have the other 5 airlines 

# encoded as columns with the values either "0" or "1". We save our new data file as "Dummy_Variable_Created". with the total 9 predictor variables:  

# Duration (continuous), Daysleft (numeric), Stops (binary), AirAsia (binary), Air_India (binary), Go_FIRST (binary), Indigo (binary), SpiceJet (binary), and  

# and Not_Late_Night (binary). Now we can proceed with the analysis.  



library(readr) 

Dummy_Variables_Created <- read_csv("C:/OneDrive/GRAD SCHOOL materials/FALL 2023/BUAN 6356 B. ANALYTICS with R/SEMESTER PROJECT -Personal/Dummy_Variables Created.csv") 

m1Data <- Dummy_Variables_Created 

m1Data 



#By setting the random seed to 1, we will be able to replicate the results in   

#the future by generating the same partitions  



#We use the createDataPartition function to randomly allocate 70% of the data   

#into the training data set and 30% into the validation data set.  

set.seed(1) 

myIndex <- createDataPartition(m1Data$Price, p=0.7, list=FALSE)  



#We now use this list of index to subset the data for training and validation  

trainSet <- m1Data[myIndex,]  
validationSet <- m1Data[-myIndex,]  



##   6)    Modeling - Building Model(s) (at least 2) 



##(Data partition, Holdout Method,) --------------------(to be completed) 



## Model1: Linear Multiple Linear Regression 

# After checking the relationship of each predictor with the independent variable, we have decided to start with the most relevant predictors 

# and then continuously add other predictors to our model to evaluate their influence. 



model_a <- lm(Price ~ AirAsia + Air_India + GO_FIRST + Indigo + SpiceJet + Days_left, data  = trainSet) 

model_a 

summary(model_a) 



model_b <- lm(Price ~ AirAsia + Air_India + GO_FIRST + Indigo + SpiceJet + Days_left + Stops, data = trainSet) 

model_b 

summary(model_b) 



model_c <- lm(Price ~ AirAsia + Air_India + GO_FIRST + Indigo + SpiceJet + Days_left + Stops + Not_Late_Night, data = trainSet) 

model_c 

summary(model_c) 



model_d <- lm(Price ~ AirAsia + Air_India + GO_FIRST + Indigo + SpiceJet + Days_left + Stops + Not_Late_Night + Duration, data = trainSet) 

model_d 

summary(model_d) 







# View the summary of the model to get the coefficients 

summary(model_a) 

summary(model_b) 

summary(model_c) 

summary(model_d) 



## Model 2: Using Regression Tree 



### The departure time wasn't found to be statistically significant on our previous model, 

# We will drop that variable in the model 2 analysis. 

m2tData <- trainSet[, c("Price", "Duration", "Days_left", "Stops", "AirAsia", "Air_India", "GO_FIRST", "Indigo", "SpiceJet")]  

m2tData 



m2vData <- validationSet[, c("Price", "Duration", "Days_left", "Stops", "AirAsia", "Air_India", "GO_FIRST", "Indigo", "SpiceJet")]  

m2vData 



# We create the default tree first 

default_tree <- rpart(Price ~., data = m2tData, method = "anova")  

summary(default_tree)  



# Plot the Tree 

prp(default_tree, type = 1, extra = 1, under = TRUE)  

printcp(default_tree) 



# Growing the Full Tree 

# Due to the number of the variables and the complexity of the data, R couldn't run with a cp = 0, so we chose a  

# minimum cp = 0.00009 

set.seed(1)  

full_tree <- rpart(Price ~ ., data = m2tData, method = "anova", cp = 0.00009, minsplit = 2, minbucket = 1)  



prp(full_tree, type = 1, extra = 1, under = TRUE)  



printcp(full_tree) 

summary(full_tree)  



# Pruning The Tree to the best pruned Tree 

# we have found the Tree 104 to have the minimum error of 0.33878, with a cp = 0.000105892, std = 0.0035265. 

# We found the minimum best pruned (withing 1 std) to have a cp = 0.000224152. We will use this cp value to prune to the Tree 



pruned_tree <- prune(full_tree, cp= 0.000224154)  



prp(pruned_tree, type = 1, extra = 1, under = TRUE)  

printcp(pruned_tree) 

summary(pruned_tree)  









#  What type of decision making model(s) is appropriate for the decision‐making tasks? 

# with linear regression model performed above, we can see that "model_d" shows better R2...(to be completed.) 



# c. Provide rationale for choice of model(s) 

# d. Detail model development and output. 





##  7)    Models Evaluation  

#m Model 1 

# We will use our validation set to test our best model, model_d 

predicted_value <- predict(model_b, validationSet) 
accuracy(predicted_value, validationSet$Price) 

#Creating a Residual Plot
residuals <- resid(model_b)
plot(residuals ~ trainSet$Price,xlab="", ylab="Error", main = "Residual Plot (Linear Regression )")
abline(h=0,col='Red')

#Creating a Residual Distribution Plot
residuals <- validationSet$Price - predicted_value
hist(residuals, breaks = 100, col = "lightblue", main = "Distribution of Residuals (Linear Regression)", xlab = "Residuals")
abline(v=0,col='Red')





# Model 2 

# We will use our validation set to test the pruned model 
predicted_value <- predict(pruned_tree, m2vData)  
accuracy(predicted_value, m2vData$Price) 

#Creating a Residual Plot
residuals <- resid(pruned_tree)
plot(residuals ~ m2tData$Price,xlab="", ylab="Error", main = "Residual Plot (Regression Tree)")
abline(h=0,col='Red')

#Creating a Residual Distribution Plot
residuals <- validationSet$Price - predicted_value
hist(residuals, breaks = 100, col = "lightblue", main = "Distribution of Residuals (Regression Tree)", xlab = "Residuals")
abline(v=0,col='Red')



