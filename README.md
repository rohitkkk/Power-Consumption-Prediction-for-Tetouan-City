# Power-Consumption-Prediction-for-Tetouan-City
This project uses machine learning techniques to predict power consumption across three distribution zones in Tetouan City, Morocco. The model employs a Random Forest regression algorithm to forecast energy consumption based on environmental data such as temperature, humidity, wind speed, and solar radiation.

## Dataset
The dataset used in this project is sourced from the UCI Machine Learning Repository and contains hourly measurements of environmental parameters and power consumption for three zones:

Source: <a href="https://archive.ics.uci.edu/dataset/849/power+consumption+of+tetouan+city">Power Consumption of Tetouan City</a>

### dataset attributes:
* DateTime: Time of the measurement (10-minute intervals)
* Temperature: Weather temperature in °C
* Humidity: Relative humidity in %
* Wind Speed: Wind speed in m/s
* General Diffuse Flows and Diffuse Flows: Solar radiation in W/m²
* Zone 1, Zone 2, Zone 3 Power Consumption: Target variables for prediction (kW)

## Tools and Libraries

The following tools and libraries were used:

* R: Programming language for statistical computing
* Random Forest: Machine learning model for regression
### Libraries:
* randomForest
* ggplot2
* caret
* lubridate

## Methodology

### Data Preprocessing:

Handling missing values and converting the DateTime field to a suitable format.
Normalizing environmental variables.
Feature Engineering:

Original features: Temperature, Humidity, Wind Speed, General Diffuse Flows, Diffuse Flows.
Created additional features: TwoHourInterval, Weekend, DayOfWeek and Hour.

### Model Training and Evaluation:

Random Forest regression was applied separately for each zone.
Evaluation metrics included Root Mean Squared Error (RMSE) and R-squared.

### Improvements:

Incorporating temporal features (Hour, DayOfWeek, TwoHourInterval, Weekend) led to a significant reduction in RMSE.

## Results
After incorporating additional features, the model showed improved accuracy across all zones:

Zone 1: RMSE reduced from 5249.296 to 1154.46.
Zone 2: RMSE reduced from 3366.984 to 801.8158.
Zone 3: RMSE reduced from 3265.414 to 740.9489.
