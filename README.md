# TimeseriesForecasting
This is an R project for studying Time Series Analysis in R
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
links:-
https://towardsdatascience.com/time-series-forecasting-arima-models-7f221e9eee06
http://r-statistics.co/Time-Series-Analysis-With-R.html
https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials---Important

-------------------------------------------------------------------------------------
---
title: "Time Series Forecasting"
author: "Rajat Shrivastav"
output: html_document
---
      
During this project we are going to have look into the concept of Time Series Forecasting.We have the sample data set coming from kaggle.
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

```{r setup,echo=FALSE,warning=FALSE}
knitr::opts_knit$set(root.dir="C:\\Users\\1500202\\Documents\\NEW_R_Scripts")
```           

```{r message=FALSE,echo=FALSE,warning=FALSE}
library("tseries", lib.loc="~/R/win-library/3.5")
library("tidyr", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("rmarkdown", lib.loc="~/R/win-library/3.5")
library("vcd", lib.loc="~/R/win-library/3.5")
library("psych", lib.loc="~/R/win-library/3.5")
library("rmarkdown", lib.loc="~/R/win-library/3.5")
library("gplots", lib.loc="~/R/win-library/3.5")
library("gtable", lib.loc="~/R/win-library/3.5")
library("DataCombine", lib.loc="~/R/win-library/3.5")
library("lattice", lib.loc="C:/Program Files/R/R-3.5.0/library")
library("randomForest", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggthemes", lib.loc="~/R/win-library/3.5")
library("plyr", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library("forecast", lib.loc="~/R/win-library/3.5")
library("gmodels", lib.loc="~/R/win-library/3.5")
library("knitr", lib.loc="~/R/win-library/3.5")
library("scales", lib.loc="~/R/win-library/3.5")
library("mice", lib.loc="~/R/win-library/3.5")
library("gmodels", lib.loc="~/R/win-library/3.5")
```


```{r}
candy<-read.csv("candy_production.csv");
View(candy)
```
        
Having a first look at the data.
         
```{r}
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
candy_1 <- tsclean(candy_1);
head(candy_1);

```


The very first Step in any of the time Series Analysis is to **plot** the time Series.We didn't used the ggplot function beacuse for that we would reuire to make a dataframe and since all our calculations are based on the time series object, we didn't used ggplot.


```{r echo=FALSE}
plot.ts(candy_1, main = 'Candy production from 1972 - 2017', xlab = 'Year', ylab = 'Candy Production Units')
```

Further looking into the production for Monthwise analysis 

```{r}
monthplot(candy_1, xlab = 'Month', ylab = 'Candy Production Units',
         main = 'Monthly US candy production from 1972 - 2017')
```

We can also look for any outliers using the boxplots for the production of candies monthly from 1972-2017.Monthplot and Boxplots both yield the same results.


```{r}
boxplot(candy_1 ~ cycle(candy_1),xlab='Months',type='count',ylab='Production',main='Boxplot for Production of Candies Monthly',col=c("red","blue","green","yellow","orange"));
```


Observations:-

Yearly trend - Candy production has increased till 2000, it remained stagnant till mid 2000's, descreased till 2010 and now is showing and upward trend.

Seasonality - As expected candy production is the highest during the months of Oct-Dec, which can be accounted by the holiday season (Halloween & Christmas).

Multiplicative time series - This dataset appears to be a multiplicative time series as the amplitude of the seasonal variation **varies** with months.


Now decomposing the Orginal time Series into its components, by looking at the plot we can figure out that the given time Series contains all the following fluctuations/
- Trend
- Seasonal 
- Irregular

Formally, if Y is the candy production, we can decompose the series in two ways: by using either an additive or multiplicative model,

$$Y = S_t + T_t + E_t$$$$Y = S_t * T_t * E_t$$
where St is the seasonal component, T is trend and cycle, and E is the remaining error.

Thus the above given time series is thus **Mulitplicative** in nature.
Now lets look the components individually.


```{r}

tscomponents_mul <- decompose(candy_1, type = "multiplicative");
plot(tscomponents_mul, col = "blue");

```

Time Series Forecasting
We'll use the popular ARIMA model for the forecasting. ARIMA stands for AutoRegressive Integrated Moving Average.
Also we are using a Seasonal ARIMA as the timeseries exhibits Seasonality.

-Auto Regressive (AR) terms refer to the lags of the differenced series
-Moving Average (MA) terms refer to the lags of errors
-I is the number of difference used to make the time series stationary.

ARIMA stands for auto-regressive integrated moving average and is specified by these three order parameters: (p, d, q). The process of fitting an ARIMA model is sometimes referred to as the Box-Jenkins method.

An auto regressive (AR(p)) component is referring to the use of past values in the regression equation for the series Y. The auto-regressive parameter p specifies the number of lags used in the model. For example, AR(2) or, equivalently, ARIMA(2,0,0), is represented as

$$Y_t = c + \phi_1y_{t-1} + \phi_2 y_{t-2}+ e_t$$

where φ1, φ2 are parameters for the model.

The d represents the degree of differencing in the integrated (I(d)) component. Differencing a series involves simply subtracting its current and previous values d times. Often, differencing is used to stabilize the series when the stationarity assumption is not met, which we will discuss below.

A moving average (MA(q)) component represents the error of the model as a combination of previous error terms et. The order q determines the number of terms to include in the model

$$Y_t = c + \theta_1 e_{t-1} + \theta_2 e_{t-2} +...+ \theta_q e_{t-q}+ e_t$$

Differencing, autoregressive, and moving average components make up a non-seasonal ARIMA model which can be written as a linear equation:

$$ Y_t = c + \phi_1y_d{_{t-1}} + \phi_p y_d{_{t-p}}+...+\theta_1 e_{t-1} +  \theta_q e_{t-q} + e_t$$

where yd is Y differenced d times and c is a constant.

Note that the model above assumes non-seasonal series, which means you might need to de-seasonalize the series before modeling. 

Removing trend and Seasonality from the Time Series and Random Variations in order to make it stationary.

Now we will get all the components using the **stl** command which stands for (Seasonal Decomposition of Time Series by Loess) removing all trend, seasonality and randomness through different buckets. 

```{r}
decomposed <- stl(log(candy_1), s.window="periodic")
seasonal <- decomposed$time.series[,1]
trend   <- decomposed$time.series[,2]
remainder <- decomposed$time.series[,3]
```

Detrending the time Series 
```{r}
decomposed_1<-candy_1 - seasonal;
decomposed_2<-decomposed_1 - trend;
plot(decomposed_2)
```

By only removing the seasonal component
```{r}
ap.sa_new<- exp(seasadj(decomposed))
autoplot(cbind(decomposed_2, SeasonallyAdjusted=ap.sa_new)) +
  xlab("Year") + ylab("Number of passengers (thousands)")
```

This is What a **DESEASONALED** time series looks like one.

Test for stationarity

One of the assumptions for an ARIMA model is that the data should be stationary. The model having white noise that is weakly stationary(cyclical behaviour) can be considered to be stationary.

Let's test for stationarity using the Augmented Dickey-Fuller Test and KPSS test

Null Hypothesis Ho : The candy time series is non stationary.
Alternative Hypothesis Ha : The candy time series is stationary.

```{r}
adf.test(decomposed_2);
kpss.test(decomposed_2)
```

From the analysis we can figure out the the p-value is less than the significance Level , therefore we reject the Null Hypothesis and accept the alternate hypothesis.
Thus the candy series is stationary.

One way to identify the stationarity of the Series is creating lags and studying ACF(Auto Correlation Functions) and PACF(Partial Auto Correlation Function) plots.

```{r}
laggedTS <- lag(candy$Production,3);
leadTSq <- lead(candy$Production,8);
par(mfrow=c(3,1));
plot.ts(candy$Production, main = 'Candy production from 1972 - 2017', xlab = 'Years', ylab = 'Candy Production Percentage');
plot.ts(laggedTS, main = 'Thrice Lagged Candy production from 1972 - 2017', xlab = 'Years')
plot.ts(leadTSq, main = 'Eight Leaded Candy production from 1972 - 2017', xlab = 'Years');
par(mfrow=c(1,1))
```

 Here we can compare the original series and the lagged Series.Hence we can say  that 
 Autocorrelation is the correlation of a Time Series with lags of itself.
-In the autocorrelation chart, if the autocorrelation crosses the dashed blue line, it means that specific lag is significantly correlated with current series.
-It is used commonly to determine if the time series is stationary or not. A stationary time series will have the autocorrelation fall to zero fairly quickly but for a non-stationary series it drops gradually.
 Now moving on to ACF and PACF plots.
 
```{r}
acfRes <- acf(candy_1) # autocorrelation
pacfRes <- pacf(candy_1)  # partial autocorrelation
```

As we can see in the ACf plot that the autocorrelation abruptly falls to zero and then again rises.

Now Fitting this into the ARIMA model.This is where the actual Forecasting Begins.........
     













