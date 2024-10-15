# Installing and loading required libraries
install.packages("randomForest")
install.packages("caTools")

library(randomForest)
library(caTools)

# Loading the raw data set
data <- read.csv(file.choose())
head(data)
summary(data)
colnames(data)
str(data)

#preprocess data
?POSIXct
# Convert 'DateTime' column to datetime format
data$DateTime <- as.POSIXct(data$DateTime, format="%m/%d/%Y %H:%M")

# Check for any missing values
colSums(is.na(data))

# Normalize the environmental features (Temperature, Humidity, Wind Speed, etc.)
# scale the data to a range between 0 and 1.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to the relevant columns
data$Temperature <- normalize(data$Temperature)
data$Humidity <- normalize(data$Humidity)
data$Wind.Speed <- normalize(data$Wind.Speed)
data$general.diffuse.flows <- normalize(data$general.diffuse.flows)
data$diffuse.flows <- normalize(data$diffuse.flows)

# After normalization, dataset preview
head(data)


#Split the Data into Training and Testing Sets
# Setting a seed for reproducibility
set.seed(123)

# Split the data into training (70%) and test (30%)
split <- sample.split(data$Zone.1.Power.Consumption, SplitRatio = 0.7)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

#Train the Random Forest Model
# Train Random Forest model for Zone 1 Power Consumption
#Use mtry to reduce error
#best_mtry <- tuneRF(training_set[,c('Temperature', 'Humidity', 'Wind.Speed', 'general.diffuse.flows', 'diffuse.flows')],
 #                   training_set$Zone.1.Power.Consumption, 
 #                   stepFactor = 1.5, 
  #                  improve = 0.01, 
   #                 ntreeTry = 500, 
    #                trace = TRUE)
#best_mtry_value <- best_mtry[which.min(best_mtry[, "OOBError"]), "mtry"]

rf_model_zone1 <- randomForest(Zone.1.Power.Consumption ~ Temperature + Humidity + Wind.Speed + general.diffuse.flows + diffuse.flows,
                               data = training_set, ntree = 100)

# Train Random Forest model for Zone 2 Power Consumption
rf_model_zone2 <- randomForest(Zone.2..Power.Consumption ~ Temperature + Humidity + Wind.Speed + general.diffuse.flows + diffuse.flows,
                               data = training_set, ntree = 100)

# Train Random Forest model for Zone 3 Power Consumption
rf_model_zone3 <- randomForest(Zone.3..Power.Consumption ~ Temperature + Humidity + Wind.Speed + general.diffuse.flows + diffuse.flows,
                               data = training_set, ntree = 100)

# Display model summaries

print(rf_model_zone1)
print(rf_model_zone2)
print(rf_model_zone3)


#Predict Power Consumption on the Test Set
# Predictions for Zone 1
pred_zone1 <- predict(rf_model_zone1, newdata = test_set)

# Predictions for Zone 2
pred_zone2 <- predict(rf_model_zone2, newdata = test_set)

# Predictions for Zone 3
pred_zone3 <- predict(rf_model_zone3, newdata = test_set)


#Model evaluation
# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Zone 1 Evaluation
zone1_rmse <- rmse(test_set$Zone.1.Power.Consumption, pred_zone1)
zone1_r2 <- summary(lm(pred_zone1 ~ test_set$Zone.1.Power.Consumption))$r.squared

# Zone 2 Evaluation
zone2_rmse <- rmse(test_set$Zone.2..Power.Consumption, pred_zone2)
zone2_r2 <- summary(lm(pred_zone2 ~ test_set$Zone.2..Power.Consumption))$r.squared

# Zone 3 Evaluation
zone3_rmse <- rmse(test_set$Zone.3..Power.Consumption, pred_zone3)
zone3_r2 <- summary(lm(pred_zone3 ~ test_set$Zone.3..Power.Consumption))$r.squared

# Print RMSE and R-squared
cat("Zone 1 RMSE:", zone1_rmse, "\nZone 1 R-squared:", zone1_r2, "\n")
cat("Zone 2 RMSE:", zone2_rmse, "\nZone 2 R-squared:", zone2_r2, "\n")
cat("Zone 3 RMSE:", zone3_rmse, "\nZone 3 R-squared:", zone3_r2, "\n")



#Visualization of results
# Visualization for Zone 1
plot(test_set$Zone.1.Power.Consumption, 
     pred_zone1, main = "Zone 1: Actual vs Predicted",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "blue")
abline(0, 1)

# Visualization for Zone 2
plot(test_set$Zone.2..Power.Consumption, pred_zone2, main = "Zone 2: Actual vs Predicted",
     xlab = "Actual Power Consumption", ylab = "Predicted Power Consumption", col = "green")
abline(0, 1)

# Visualization for Zone 3
plot(test_set$Zone.3..Power.Consumption, pred_zone3, main = "Zone 3: Actual vs Predicted",
     xlab = "Actual Power Consumption", ylab = "Predicted Power Consumption", col = "red")
abline(0, 1)



print("big errors therefore more analysis")
  # Incorporate Hour and Calendar Data for better model performance
# Convert DateTime to hour and day of the week
training_set$Hour <- as.numeric(format(training_set$DateTime, "%H"))
training_set$DayOfWeek <- as.factor(weekdays(as.Date(training_set$DateTime)))

# Include these features in the model training
rf_model_zone11 <- randomForest(Zone.1.Power.Consumption ~ Temperature 
                               + Humidity + Wind.Speed + general.diffuse.flows 
                               + diffuse.flows + Hour + DayOfWeek, 
                               data = training_set, 
                               ntree = 500)

# Train Random Forest model for Zone 2 Power Consumption
rf_model_zone22 <- randomForest(Zone.2..Power.Consumption ~ Temperature 
                               + Humidity + Wind.Speed + general.diffuse.flows 
                               + diffuse.flows+ Hour + DayOfWeek, 
                               data = training_set, 
                               ntree = 500)

# Train Random Forest model for Zone 3 Power Consumption
rf_model_zone33 <- randomForest(Zone.3..Power.Consumption ~ Temperature 
                               + Humidity + Wind.Speed + general.diffuse.flows 
                               + diffuse.flows + Hour + DayOfWeek, 
                               data = training_set, 
                               ntree = 500)




#-------------------------------------------------------------------------------
#add hour and dayofweek to test too before we predict using our test set
test_set$Hour <- as.numeric(format(test_set$DateTime, "%H"))
test_set$DayOfWeek <- as.factor(weekdays(as.Date(test_set$DateTime)))

# Make predictions using the updated models
pred_zone11 <- predict(rf_model_zone11, newdata = test_set)
pred_zone22 <- predict(rf_model_zone22, newdata = test_set)
pred_zone33 <- predict(rf_model_zone33, newdata = test_set)

# Evaluate the updated models
zone11_rmse <- rmse(test_set$Zone.1.Power.Consumption, pred_zone11)
zone11_r2 <- summary(lm(pred_zone11 ~ test_set$Zone.1.Power.Consumption))$r.squared

zone22_rmse <- rmse(test_set$Zone.2..Power.Consumption, pred_zone22)
zone22_r2 <- summary(lm(pred_zone22 ~ test_set$Zone.2..Power.Consumption))$r.squared

zone33_rmse <- rmse(test_set$Zone.3..Power.Consumption, pred_zone33)
zone33_r2 <- summary(lm(pred_zone33 ~ test_set$Zone.3..Power.Consumption))$r.squared

# Print RMSE and R-squared for the updated models
cat("Zone 1 (with Hour & DayOfWeek) RMSE:", zone11_rmse, "\nZone 1 R-squared:", zone11_r2, "\n")
cat("Zone 2 (with Hour & DayOfWeek) RMSE:", zone22_rmse, "\nZone 2 R-squared:", zone22_r2, "\n")
cat("Zone 3 (with Hour & DayOfWeek) RMSE:", zone33_rmse, "\nZone 3 R-squared:", zone33_r2, "\n")


# Plotting RMSE Comparison
rmse_values <- c(5249.296, 1154.46, 3366.984, 801.8158 , 3265.414, 740.9489 )
zone_names <- c("Zone 1 Before", "Zone 1 After", "Zone 2 Before", "Zone 2 After", "Zone 3 Before", "Zone 3 After")
barplot(rmse_values, names.arg=zone_names, main="RMSE Comparison Before and After Feature Addition", col=c("red", "green"), las=2)

# Visualization for Zone 1 (After adding Hour & DayOfWeek)
plot(test_set$Zone.1.Power.Consumption, pred_zone11, 
     main = "Zone 1: Actual vs Predicted (with Hour & DayOfWeek)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

# Visualization for Zone 2 (After adding Hour & DayOfWeek)
plot(test_set$Zone.2..Power.Consumption, pred_zone22, 
     main = "Zone 2: Actual vs Predicted (with Hour & DayOfWeek)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "green", pch = 16)
abline(0, 1, col = "red", lwd = 2)

# Visualization for Zone 3 (After adding Hour & DayOfWeek)
plot(test_set$Zone.3..Power.Consumption, pred_zone33, 
     main = "Zone 3: Actual vs Predicted (with Hour & DayOfWeek)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "red", pch = 16)
abline(0, 1, col = "green", lwd = 2)




#-------------------------------------------------------------------------------
#TRAINING MODEL ON MORE DATA AFTER NEW TRANFORMATIONS
#ASSIGNMENT

# Create 2-hour intervals
data$TwoHourInterval <- as.numeric(format(data$DateTime, "%H")) %/% 2

# Create a weekend/weekday feature
# ifelse(..., "Weekend", "Weekday"):
# SYNTAX: ifelse(condition, true_value, false_value)
data$Weekend <- ifelse(weekdays(as.Date(data$DateTime)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
data$Weekend <- as.factor(data$Weekend)


# Split the Data into Training and Testing Sets
set.seed(123)

# Split the data into training (70%) and test (30%)
split <- sample.split(data$Zone.1.Power.Consumption, SplitRatio = 0.7)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

# Train the Random Forest Model for Zone 1, Zone 2, and Zone 3 Power Consumption
rf_model_zone111 <- randomForest(Zone.1.Power.Consumption ~ Temperature + Humidity + Wind.Speed 
                                 + general.diffuse.flows + diffuse.flows + 
                                   TwoHourInterval + Weekend,
                               data = training_set, ntree = 500)

rf_model_zone222 <- randomForest(Zone.2..Power.Consumption ~ Temperature + Humidity + Wind.Speed 
                                 + general.diffuse.flows + diffuse.flows + 
                                   TwoHourInterval + Weekend,
                               data = training_set, ntree = 500)

rf_model_zone333 <- randomForest(Zone.3..Power.Consumption ~ Temperature + Humidity + Wind.Speed 
                                 + general.diffuse.flows + diffuse.flows + 
                                   TwoHourInterval + Weekend,
                               data = training_set, ntree = 500)

# Display model summaries
print(rf_model_zone111)
print(rf_model_zone222)
print(rf_model_zone333)

# Predict Power Consumption on the Test Set
pred_zone111 <- predict(rf_model_zone111, newdata = test_set)
pred_zone222 <- predict(rf_model_zone222, newdata = test_set)
pred_zone333 <- predict(rf_model_zone333, newdata = test_set)

# Model Evaluation
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Zone 1 Evaluation
zone111_rmse <- rmse(test_set$Zone.1.Power.Consumption, pred_zone111)
zone111_r2 <- summary(lm(pred_zone111 ~ test_set$Zone.1.Power.Consumption))$r.squared

# Zone 2 Evaluation
zone222_rmse <- rmse(test_set$Zone.2..Power.Consumption, pred_zone222)
zone222_r2 <- summary(lm(pred_zone222 ~ test_set$Zone.2..Power.Consumption))$r.squared

# Zone 3 Evaluation
zone333_rmse <- rmse(test_set$Zone.3..Power.Consumption, pred_zone333)
zone333_r2 <- summary(lm(pred_zone333 ~ test_set$Zone.3..Power.Consumption))$r.squared

# Print RMSE and R-squared
cat("Zone 1 RMSE:", zone111_rmse, "\nZone 1 R-squared:", zone111_r2, "\n")
cat("Zone 2 RMSE:", zone222_rmse, "\nZone 2 R-squared:", zone222_r2, "\n")
cat("Zone 3 RMSE:", zone333_rmse, "\nZone 3 R-squared:", zone333_r2, "\n")

# Visualization of results
# Visualization for Zone 1
plot(test_set$Zone.1.Power.Consumption, 
     pred_zone111, main = "Zone 1: Actual vs Predicted (with 2-Hour Interval & Weekend/Weekday)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

# Visualization for Zone 2
plot(test_set$Zone.2..Power.Consumption, 
     pred_zone222, main = "Zone 2: Actual vs Predicted (with 2-Hour Interval & Weekend/Weekday)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "green", pch = 16)
abline(0, 1, col = "red", lwd = 2)

# Visualization for Zone 3
plot(test_set$Zone.3..Power.Consumption, 
     pred_zone333, main = "Zone 3: Actual vs Predicted (with 2-Hour Interval & Weekend/Weekday)",
     xlab = "Actual Power Consumption", 
     ylab = "Predicted Power Consumption", 
     col = "red", pch = 16)
abline(0, 1, col = "green", lwd = 2)

#A new row for prediction based on this scenario
new_data <- data.frame(
  DateTime = as.POSIXct("2024-10-02 10:30:00"),  # Monday, 10:30 AM
  Temperature = 20.5,                            # Assume temperature is 20.5°C
  Humidity = 65,                                 # Assume humidity is 65%
  Wind.Speed = 1.00,                             # Assume wind speed is 1 m/s
  general.diffuse.flows = 120,                   # Assume general diffuse flow is 120 W/m²
  diffuse.flows = 100,                           # Assume diffuse flow is 100 W/m²
  TwoHourInterval = 5,                           # As per 10:00 AM - 12:00 PM window
  Weekend = factor("Weekday", levels = c("Weekday", "Weekend"))  # Monday is a weekday
)
str(training_set)
str(new_data)
new_data$TwoHourInterval <- as.numeric(format(new_data$DateTime, "%H")) %/% 2
new_data$Weekend <- ifelse(weekdays(as.Date(new_data$DateTime)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
new_data$Weekend <- factor(new_data$Weekend, levels = levels(training_set$Weekend))

predicted_zone3_power <- predict(rf_model_zone333, new_data)
cat("The predicted power consumption for Zone 3 at 10am to 12pm AM on Monday is:", predicted_zone3_power, "kW")

















