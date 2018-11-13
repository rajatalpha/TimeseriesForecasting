# TimeseriesForecasting
This is an R project for studying Time Series Analysis in R
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
links:-
https://towardsdatascience.com/time-series-forecasting-arima-models-7f221e9eee06
http://r-statistics.co/Time-Series-Analysis-With-R.html

-------------------------------------------------------------------------------------
      
During this project we are going to have look into the concept of Time Series Forecasting.We have 
the sample data set coming from kaggle.
        On a broader perpespective, we are going to forecast the candy sales in US for next 3 months.
 
**Introduction** 

We are going to look at the production of candy sales from 1972 to 2017 as a time series and forecast the production for the next few years.

We'll perform the following steps:

- Cleaning and Visualizing the data
- Time series decomposition
- Test for stationarity of the time series
- Fitting the ARIMA model
- Calculating Forecasts
 
 **Visualizing the data**
 
 Starting off with reading the data for analysis,which is a CSV file.

```{r setup,echo=FALSE}
knitr::opts_knit$set(root.dir="C:\\Users\\1500202\\Documents\\NEW_R_Scripts")
```           

```{r include=FALSE}
library("tseries", lib.loc="~/R/win-library/3.5")
library("tidyr", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("rmarkdown", lib.loc="~/R/win-library/3.5")
library("vcd", lib.loc="~/R/win-library/3.5")
library("psych", lib.loc="~/R/win-library/3.5")
library("rmarkdown", lib.loc="~/R/win-library/3.5")
library("gplots", lib.loc="~/R/win-library/3.5")
library("gtable", lib.loc="~/R/win-library/3.5")
library("lattice", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("randomForest", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggthemes", lib.loc="~/R/win-library/3.5")
library("plyr", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")
```

```{r}
candy<-read.csv("candy_production.csv");
```
        
Having a first look at the data.
         
```{r}
View(candy);
head(candy); 
```

Let look at the struture of data

```{r}
str(candy)
```

Now lets rename the column for better understanding and for using them in our analysis.
 
```{r}
colnames(candy)<-c("Months","Production")
head(candy)
```

Convert it to a time series dataset using the tseries package for our further analysis to finds the trends in the data.

```{r}
candy_1 <-ts(candy$Production, start = c(1972,1), end = c(2017,8), frequency=12);
head(candy_1);
```

The very first Step in any of the time Series Analysis is to **plot** the time Series.

```{r echo=FALSE}
plot.ts(candy$Production, main = 'Candy production from 1972 - 2017', xlab = 'Year', ylab = 'Candy Production Percentage')
```

Further looking into the production for Monthwise analysis 

```{r}
monthplot(candy_1, xlab = 'Month', ylab = 'Candy Production Percentage',
         main = 'Monthly US candy production from 1972 - 2017')
```

We can also look for any outliers using the boxplots for the production of candies monthly from 1972-2017.

```{r}
boxplot(candy_1 ~ cycle(candy_1),xlab='Months',type='count',ylab='Production',main='Boxplot for Production of Candies Monthly',col=c("red","blue","green","yellow","orange"));

Observations:-

Yearly trend - Candy production has increased till 2000, it remained stagnant till mid 2000's, descreased till 2010 and now is showing and upward trend.

Seasonality - As expected candy production is the highest during the months of Oct-Dec, which can be accounted by the holiday season (Halloween & Christmas).

Multiplicative time series - This dataset appears to be a multiplicative time series as the amplitude of the seasonal variation varies with months.




Now decomposing the Orginal time Series into its components, by looking at the plot we can figure out that the given time Series contains all the following fluctuations/
- Trend
- Seasonal 
- Irregular

Thus the above given time series is thus **Mulitplicative** in nature.
Now lets look the components individually.


```{r}

tscomponents_mul <- decompose(candy_1, type = "multiplicative");
plot(tscomponents_mul, col = "blue");

```

Time Series Forecasting
We'll use the popular ARIMA model for the forecasting. ARIMA stands for AutoRegressive Integrated Moving Average.

-Auto Regressive (AR) terms refer to the lags of the differenced series
-Moving Average (MA) terms refer to the lags of errors
-I is the number of difference used to make the time series stationary.
Test for stationarity

One of the assumptions for an ARIMA model is that the data should be stationary. The model having white noise that is weakly stationary(cyclical behaviour) can be considered to be stationary.

Let's test for stationarity using the Augmented Dickey-Fuller Test.

Null Hypothesis Ho : The candy time series is non stationary.
Alternative Hypothesis Ha : The candy time series is stationary.

```{r}

adf.test(candy_1);

```










