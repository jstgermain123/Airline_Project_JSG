# Final Project - ALY6015
# Jacob St. Germain
# October 21, 2020

################################################################################
#                      Uploading and Formatting Data                           #
################################################################################

# Please choose the dataset called "2019 July"
myData <- read.csv(file.choose(), sep=",", header=TRUE)
# Now choose the dataset called "2019 Full Data" for a different exercise
fullData <- read.csv(file.choose(), sep=",", header=TRUE)


# Renaming dataset for a specific exercises later on
air_2019 <- myData
data <- myData

# Change FL_DATE from character to date format
myData$FL_DATE <- as.Date(myData$FL_DATE)

# Changing all the following columns from chr to factors
myData$CARRIER_TYPE <- as.factor(myData$CARRIER_TYPE)
myData$DEP_DEL15 <- as.factor(myData$DEP_DEL15)
myData$CANCELLED <- as.factor(myData$CANCELLED)
myData$CANCELLATION_CODE <- as.factor(myData$CANCELLATION_CODE)
myData$ORIGIN <- as.factor(myData$ORIGIN)
myData$ORIGIN_STATE_NM <- as.factor(myData$ORIGIN_STATE_NM)


# Change the name of the column from full dataset 
names(fullData)[names(fullData) == "FL_DATE"] <- "Date"
# Change the format of the column to date
fullData$Date <- as.Date(fullData$Date, format="%Y-%m-%d")

################################################################################
#                               Descriptive Statistics                         #
################################################################################

View(data)
str(data)
summary(data)

#histogram for the complete data
hist(data$ARR_TIME, col= "red", xlab ="Length", main= "Airtime")
hist(data$DISTANCE, col= "red", xlab ="Length", main= "Distance")
#sublet the data
mainlinedata<-data[data$CARRIER_TYPE=="Mainline",]
regionaldata<-data[data$CARRIER_TYPE=="Regional",]

#histogram for subletted data
hist(mainlinedata$ARR_TIME, col= "light blue", xlab ="Length", main= "Mainline-Airtime")
hist(regionaldata$AIR_TIME, col= "gold", xlab ="Length", main= "Regional-Airtime")
hist(mainlinedata$DISTANCE, col= "light blue", xlab ="Length", main= "Mainline-Distance")
hist(regionaldata$DISTANCE, col= "gold", xlab ="Length", main= "Regional-Distance")

#boxplot 
boxplot(data$AIR_TIME~ data$CARRIER_TYPE, col=c("gold", "darkgreen"), main = "Airtime data")
boxplot(data$DISTANCE~ data$CARRIER_TYPE, col=c("gold", "darkgreen"), main = "Distance data")


################################################################################
#                               Elastic Net Regression                         #
################################################################################


# install "glmnet" package for Elasticnet Model (uncomment to install packages)
# install.packages("glmnet")
# install.packages("tidyr")
library(tidyr)
library(glmnet)  # Package to fit ridge/lasso/elastic net models

set.seed(123)  # Set seed for reproducibility

#standardize numeric continuous variables

# Create Matrix
n <- 659029 #number of observations in dataset
y <- air_2019$CARRIER_DELAY # what we are trying to predict 
y <- replace_na(y,0) # replace na with 0 since delay time for these entries were 0
x <- data.matrix(air_2019)
x <- replace_na(x,0)

#standardize numeric continuous variables


x<- x[,-30] # remove predicted variable y
x<- x[,-24] # remove variable "cancelation code"
x <- x[,-21] # remove arrival delay varaible
x <- x[,-22] # remove arrival delay 15 varaible

# Split data into training and testing datasets.
train_rows <- sample(1:n, .66*n) # generate row numbers for training set randomly (use 2/3 of data)

x.train <- x[train_rows, ] # create training data
x.test <- x[-train_rows, ] # create testing data

y.train <- y[train_rows]# create training data
y.test <- y[-train_rows]# create testing data


################################
## The following loop uses 10-fold Cross Validation to determine the
## optimal value for lambda for alpha = 0, 0.1,etc
## using the Training dataset.
## WARNING: WILL TAKE A COUPLE OF MINUTES
################################

list.of.fits <- list()
for (i in 0:10) {
  # We are testing alpha = i/10.
  # First, make a variable name that we can use later to refer
  # to the model optimized for a specific alpha.
  fit.name <- paste0("alpha", i/10)
  # Now we fit a model and store it in a list that 
  # uses the variable name we just created
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10, 
              family="gaussian")
}

# We will now look at which value (0, 0.1, ... , 0.9, 1) does the best job
# predicting the values in the Testing dataset.
results <- data.frame() # create data frame where results will be stored

for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  # Use each model to predict 'y' given the Testing dataset
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
  
  # Calculate the MSE
  mse <- mean((y.test - predicted)^2)
  
  # Store results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

## View the results
# View(results)

# use .6 as it minimizes MSE
alpha0.6.fit <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0.6, family="gaussian")
# CV Plot
plot(alpha0.6.fit) # CREATE CROSS VALIDATION PLOT

alpha0.6.predicted <- predict(alpha0.6.fit, s=alpha0.6.fit$lambda.1se, newx=x.test) # validate actuals vs predicted

mean((y.test - alpha0.6.predicted)^2) # calculate MSE for the model


# View Variables Used in Model
coef(alpha0.9.fit,s="lambda.1se")

# model utilizes 16 variables to predict

# view plot of variables and their coefficients
plot(alpha0.6.fit$glmnet.fit, xvar= "lambda") # PLOT VARIABLES COEFFICIENT VALUES VS LAMBDA 
abline(v = log( c ( alpha0.6.fit$lambda.min, alpha0.6.fit$lambda.1se ) ), lty= 2) # PLOT LINES FOR 1SE AND LOWEST MSE

################################################################################
#                            Hypothesis Testing                                #
################################################################################

# Install and load these packages for this exerise (uncomment to install)
# install.packages("dplyr")
library(dplyr)

# For the first test, I want to compare the flight cancellation rate between 
# Delta Air Lines and Frontier

# First I want to get Delta Air Lines data only 
delta <- myData %>% filter(OP_CARRIER == "DL")

# I then want to rearrange the data to get cancelled and total flights
deltaC <- delta %>%
  group_by(CANCELLED) %>%
  summarise(Flights = n())
deltaC

# Doing the same for American Airlines  
frontier <- myData %>% filter(OP_CARRIER == "F9")

# I then want to rearrange the data to get cancelled and total flights
frontierC <- frontier %>%
  group_by(CANCELLED) %>%
  summarise(Flights = n())


# Saving "success" (p) and "total" (t) variables for Delta (D) and Frontier (F)
# And then join them in a vector

D_p <- deltaC$Flights[deltaC$CANCELLED == 1]
D_t <- sum(deltaC$Flights)

F_p <- frontierC$Flights[frontierC$CANCELLED == 1]
F_t <- sum(frontierC$Flights)

success <- c(D_p, F_p)
total <- c(D_t, F_t)

# Construct the test
prop.test(x = success, #I indentify my "successes" as previously mentioned
          n = total, #I do the same for the total number of patients 
          alternative = "two.sided", #Given that I just want to establish 
          #if there's a difference, it's two sided
          conf.level = 0.95, #I will be testing at alpha 0.05
          correct = FALSE) #I make sure I don't apply Yates' continuity correction


# Looking at the difference in Delays between Delta and Frontier

# Rearrange the data to get departure delayed flights and total flights
deltaDD <- delta %>%
  group_by(DEP_DEL15) %>%
  summarise(Flights = sum(!is.na(DEP_DEL15)))
deltaDD


# Doing the same for American Airlines  
# Rearrange the data to get departure delayed flights and total flights
frontierDD <- frontier %>%
  group_by(DEP_DEL15) %>%
  summarise(Flights = sum(!is.na(DEP_DEL15)))
frontierDD


# Saving "success" (p) and "total" (t) variables for Delta (D) and American (A)
# And then join them in a vector

D_p2 <- deltaDD$Flights[deltaDD$DEP_DEL15 == 1][1]
D_t2 <- sum(deltaDD$Flights)

F_p2 <- frontierDD$Flights[frontierDD$DEP_DEL15 == 1][1]
F_t2 <- sum(frontierDD$Flights)

success2 <- c(D_p2, F_p2)
total2 <- c(D_t2, F_t2)


# Construct the test
prop.test(x = success2, #I indentify my "successes" as previously mentioned
          n = total2, #I do the same for the total number of patients 
          alternative = "two.sided", #Given that I just want to establish 
          #if there's a difference, it's two sided
          conf.level = 0.95, #I will be testing at alpha 0.05
          correct = FALSE) #I make sure I don't apply Yates' continuity correction



################################################################################
#                            Time Series Analysis                              #
################################################################################

# Install and load these packages for this exerise 
install.packages("forecast")
library(forecast)

# Plot the time series 
plot(fullData$Date, fullData$flights, xlab="Date", ylab ="Flights", 
     main = "Number of Flights by Date", pch=16)
# Connect the dots with a line
lines(fullData$Date, fullData$flights, pch=16)

# Saving the number of flights as a vector to be able to actually save it 
# in a timeseries format this time
myVector <- fullData$flights

# Saving the vector as a time series. I'm telling it to start the first point
# on Week 1, Day 1 (July 1, 2019), and go all the way up to Week 5, Day 3 
# (July 31, 2019), with each week having 7 days
myTS <- ts(myVector, start=c(1,2), end=c(53,2), frequency=7)

# Plot it with specific y & x axis labels
plot.ts(myTS, main = "Number of Flights by Date",
        ylab="Flights", xlab="Weeks")

# Saving trend, seasonal, and random components to a variable
comp <- decompose(myTS)

# Plotting those components
plot(comp)

# Seasonally Adjusting (SA) the timeseries
SAmyTS <- myTS - comp$seasonal
plot.ts(SAmyTS, main = "Seasonally Adjusted",ylab="Flights", xlab="Weeks")

# Removing trend (Seasonally Adjusted, Trend Adjusted)
# a.k.a, just the random component
SATAmyTS <- SAmyTS - comp$trend
plot.ts(SATAmyTS, main = "SA and TA", ylab="Flights", xlab="Weeks")

# Look for autocorrelation of timeseries
myACFResults <- acf(myTS)

# Look for partial autorcorrelation of timeseries 
myPACFResults <- pacf(myTS)


# Now lets actually forecast values
# Holt Winters model 
# Initially we don't want beta or gamma for benchmarking 
myDataForecast <- HoltWinters(myTS, beta=FALSE, gamma=FALSE)
myDataForecast

# Plot the forecasted line along with our data
plot(myDataForecast)

# Measure SSE for benchmark
SSE1 <- myDataForecast$SSE
SSE1

# Create another model, this time with beta 
myDataForecast2 <- HoltWinters(myTS, beta=TRUE, gamma=FALSE)
myDataForecast2

# Once again, we plot the forecasted line along with our data 
plot(myDataForecast2)

# And we measure the SSE to compare to benchmark model
SSE2 <- myDataForecast2$SSE
SSE2

# Create another model, this time with gamma = true
myDataForecast3 <- HoltWinters(myTS, beta=FALSE, gamma=TRUE)
myDataForecast3

# Once again, we plot the forecasted line along with our data 
plot(myDataForecast3)

# And we measure the SSE to compare to benchmark model
SSE3 <- myDataForecast3$SSE
SSE3

# But that was only for the time period we had, what if we actually want to forecast
# I will use Jan-Nov data to forecast Dec data 

# save vector with daily number of flights in dec 2019
dec_flights <- c(22784, 21942, 20467, 20250, 21240, 21290, 16128, 20266, 21011, 19750, 20264,
                 21257, 21299, 16178, 20289, 21036, 19727, 20586, 21359, 21425, 19980, 21049, 
                 21117, 16945, 16567, 21061, 21113, 19929, 21001, 21045, 17408)

# new vector with only jan-nov, since we want to predict dec 
Jan_Nov_TS <- ts(myTS[1:334], start=c(1,2), end=c(48,6), frequency=7)

# create another model with the new data and gamma = true
myOtherForecast <- HoltWinters(Jan_Nov_TS, beta=FALSE, gamma=TRUE)

# use that model to forecast next 31 days (dec)
myFutureForecast <- forecast(myOtherForecast, h=31)
myFutureForecast

# plot the mode's forecasts
plot(myFutureForecast)
# measure the error of the model 
# create dataframe from Forecasted results
forecast_df <- as.data.frame(myFutureForecast)
# this way we can access the point estimate and compare with actual for SSE
SSE_forecast1 <- sum((dec_flights - forecast_df$`Point Forecast`)^2)
SSE_forecast1

# what if we hadn't use gamma = true
# create another model with the new data and gamma = false
myOtherForecast2 <- HoltWinters(Jan_Nov_TS, beta=FALSE, gamma=FALSE)

# use that model to forecast next 31 days (dec)
myFutureForecast2 <- forecast(myOtherForecast2, h=31)
myFutureForecast2

# plot the mode's forecasts
plot(myFutureForecast2)
# measure the error of the model 
# create dataframe from Forecasted results
forecast_df2 <- as.data.frame(myFutureForecast2)
# this way we can access the point estimate and compare with actual for SSE
SSE_forecast2 <- sum((dec_flights - forecast_df2$`Point Forecast`)^2)
SSE_forecast2

#measure the difference
SSE_forecast2 - SSE_forecast1


################################################################################
#                               Classification Analysis                        #
################################################################################

# Install and load these packages for this exerise (uncomment to install)
# install.packages("ggplot2")
# install.packages("MASS")
library(MASS)
library(ggplot2)

# Setting seed for reproducibility 
set.seed(123)

# specifying sample size for my train data
sample_size <- floor(0.7 * nrow(myData)) 

# creating index to split dataset into training and testing
index <- sample(seq_len(nrow(myData)), size=sample_size)

# generating train and test datasets from original dataset
train <- myData[index, ]
test <- myData[-index, ]

# Looking into DEP_DELAY to understand distribution between mainline and carrier
ggplot(myData, aes(x=CARRIER_TYPE, y=DEP_DELAY, fill=CARRIER_TYPE)) + 
  geom_boxplot() + ylim(0,100)

# creating LDA model
myModel <- lda(CARRIER_TYPE ~ DEP_DELAY + AIR_TIME + DISTANCE, # Type is the column 
               #that represents the class
               data=train)

#We predict results using our model, but testing 
#it on the test dataset
myResults <- predict(myModel, newdata=test)
myResults


# CHECKING ON TEST DATA
# confussion matrix with test data to determine accuracy
p2 <- predict(myModel, test)$class #selecting predicted outputs and saving
# we then generate a table with predicted vs actual
table2 <- table(Predicted = p2, Actual = test$CARRIER_TYPE)  
table2 #show the table
# determining accuracy by adding the correct predictions 
# and dividing by total
sum(diag(table2))/sum(table2)


# creating another LDA model
myModel4 <- lda(CARRIER_TYPE ~ DEP_DELAY + AIR_TIME + DISTANCE + ARR_TIME
                + CRS_DEP_TIME + CRS_ELAPSED_TIME,
                
                data=train)

myModel4

#confussion matrix with train data to determine accuracy
p4 <- predict(myModel4, test)$class #selecting predicted outputs and saving
#we then generate a table with predicted vs actual
table4 <- table(Predicted = p4, Actual = test$CARRIER_TYPE)
table4 #show the table
#determining accuracy by adding the correct predictions 
#and dividing by total
sum(diag(table4))/sum(table4)
