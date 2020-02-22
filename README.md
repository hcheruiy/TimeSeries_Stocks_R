# Forecasting the Stock Market (R)
## Time-Series Analysis on S&P 500 Stock Index with R
(Project can be found at: https://hcheruiy.github.io)

Dashboard found [here](https://raviolli77.shinyapps.io/forecast_dashboard/)
Due to dashboard hitting too many views if link doesn't work, back up is [here](http://ravishinydashboard.com/shiny/timeSeries_sp500_R/shiny_dashboard/)

# Table of Contents
* [Abstract](#Abstract)
* [Packages Required](#Packages-Required)
* [Shiny Dashboard](#shiny-dashboard)
* [Steps Required](#Steps-Required)
* [Methodology](#Methodology)
* [Sources Cited](#Sources-Cited)

## <a name="Abstract"></a>Abstract
This project focuses on using univariate time series forecasting methods for the stock market index, Standard & Poor's 500 (abbreviated commonly as S&P 500, which is the notation we will use in this project) emphasizing on Box-Jenkins **AutoRegressive Integrated Moving Average (ARIMA)** modeling. We went about the time series analysis was through using **R** and **R studio** to both predict and visualize our predictions. Along with the interactivity of [plotly](https://plot.ly/) through the [ggplot2 package](https://github.com/tidyverse/ggplot2) we were able to create stunning visuals that help in understanding which time series forecasting method is most appropriate for your own time series analysis.

## <a name="Packages-Required"></a>Packages Required

**IMPORTANT TO NOTE** (Updated 1/11/2018): Script was changed to include package `packrat` which will act as a version control. So once you clone this repo and open in *Rstudio*, select in *File* - *Open Project in new Sesssion...*, upon opening the existing project, `timeSeries_sp500_R.Rproj`, `packrat` will automatically download all dependecies.

Once `packrat` is done you should be prompted with:

	Restarting R session...


Once this happens a new *Rstudio* session will appear and all scripts should work.

## <a name="shiny-dashboard"></a>Shiny Dashboard

Dashboard should run using current scripts, if you want to publish to [Shiny](https://rstudio.github.io/shinydashboard/) contact [Ravi](www.linkedin.com/in/raul-eulogio)

## <a name="Steps-Required"></a>Steps Required

### Create plotly Account (Optional)
If you would like to have the images you create (using **plotly** and **ggplot2**) published so that you can customise the plots to your liking or brag about the interactivety of your visuals simply create a [plolty account](https://plot.ly/). Once you do so you will have access to your username and more importantly your API key, these will be necessary to publishing your plots (If you do not wish to publish your plots skip this step).

### Using Plotly account in Rstudio session
Important to note, when posting on GitHub never publish API keys (this is a common mistake I see people do). Once you gain access to your API key, have **plotly** in your current working directory, you run:

	Sys.setenv("plotly_username"="userName")
	Sys.setenv("plotly_api_key"="d1X4Hrmbe")

From here you will be able to publish your **ggplotly** visuals by running (our **ggplot2** object is called timeSeriesPlot for this example):

	plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")

If ran correctly this line of code should open up a browser with your newly published **plotly** graph!

**UPDATE** (08/17/2017): With `plotly version2`, `plotly_POST` has been **deprecated** use instead:

	api_create(timeSeriesPlot, filename = "timeSeriesPlot", fileopt='overwrite')

### Create appropriate working directory
Once the preliminary process of ensure your **Rstudio** has all parameters to ensure the code will run smoothly we suggest create an appropriate directory. For those using git we recommend using the  following line on a terminal:

	git clone git@github.com:wH4teVr folder-name

But if you are doing it manually you choose the "Clone or download" button and choose "Download ZIP". From here assuming you opened `Rstudio` and used the created project the `here` package will ensure that you are in the correct working directory without using `setwd()`.


## <a name="Methodology"></a>Methodology
For our time series analysis, we chose to focus on the [Box-Jenkins](https://en.wikipedia.org/wiki/Box%E2%80%93Jenkins#Box-Jenkins_model_identification) methodology which incorporates a series of steps to ensure we  produce the best model to forecasting. We used the years 1995 to 2014, withholding 2015 so that we can compare the forecast.

But before we outline the steps we would like to outline some  necessary assumptions for univariate time series analysis:

- The Box-Jenkins Model assumes weakly stationarity process.
- The residuals are white noise (independently and identically distributed random variables) and homoscedastic


### ARIMA Model
For this project we will be using the **Autoregressive Integrated Moving Average** model and its variations to forecast the S&P 500. For each component we have a corresponding variable for which we model if there is sign of these components. Here we roughly outline the parts that make an **ARIMA(p,d,q)** model
- **Autoregressive [AR(p)]** - a stochastic process where future values are dependent on past values signifying that past values have a linear effect on the future values.
- **Integration [I(d)]** - when differencing is done to make a process stationary, we include the differenced value(i.e. if we took the first difference it would be I(d=1))
- **Moving Average [MA(q)]** - a prcoess where the current value is linearly regressed on current and past white noise terms (residuals)

Next we outline the steps to ensure we fit the appropriate **ARIMA(p,d,q)** model!

### Stationary process and Seasonality
The first step is checking to see if the time series object is stationary, this can be done in various methods which can also be explained as exploratory analysis since we are in essence "getting a feel" for our data. Here we include some of the processes:

-  Plot the time series object: sometimes simply plotting the time series object can tell you if a process is stationary or not. As well as telling you if there are strong seasonal patterns!
- Plot Decomposed time series object: decomposing allows us to view a time series object in components (four components see website for more information). Further discussion can be seen in the project, but when we decompose our time series objects we get a glimpse of its seasonal and trend components independently.

- Seasonal Plot: The name speaks for itself but this plot is a great way to check for seasonal components which is something common when dealing with yearly, quarterly and monthly data.

These plots will help us in our Box-Jenkins Model estimation, as well as doing transformations such as differencing (and taking the log if necessary) of our time series objects to take into consideration non-stationarity and heteroskedasticity respectively.

### Autocorrelation and Partial Autocorrelation Plots
These plots play a crucial role in time series analysis, because we can estimate our **ARIMA(p,d,q)** model based on the behaviour of these plots or justify the need to do an appropriate transformation.  

We won't go into too much detail since we outlined the process in the project, but through the use of our ACF and PACF plots for our original time series we were able to make the deduction to take the first difference of our time series. Once we did that we saw that the ACF and PACF plot showed characteristics of a MA(1) model, but since we took the first difference it becomes a mixed model; **ARIMA(0, 1, 1)**

From here we do residual diagnostics to see if our model displays residuals that are white noise.

### Residual Diagnostics
We visually inspect the residual diagnostics of our model to ensure our residuals are white noise; we employ the `tsdisplay` to give us the standardized residuals, ACF plot of the residuals and the Ljung-Box statistics which are all explained more indepth in the project.

We also included a histogram of the residuals to show that they display a fairly normally distribution which ensure we haven't violated our assumptions.

### Forecast
Once we have our model, we forecast the year 2015 and see how it compares to the actual values!

We won't go into detail here but we outlined several other forecasting methods to use as comparisons. The other forecasting methods we included are:

- **Box-Cox Transformation Forecast**
- **Mean Forecast**
- **Naive Forecast**
- **Seasonal Naive Forecast**
- **Exponential Smoothing Forecast**

These forecasting methods more concisely detailed on [Here](https://www.otexts.org/fpp/2/3) by **Rob J Hyndman** and **George Athanasopoulos**

## Conclusions
Finally we draw conclusions using scale-dependent errors as to which model is best for forecasting our time series object!

| Model | RMSE | MAE | MPE | MAPE | MASE | ACF1 | Theil's U |
|-------|------|-----|-----|------|------|-----|-----------|
| ARIMA | 165.054 | 119.31 |  2.761 | 5.089 | 0.629 |  0.866 | 2.877 |
| Box-Cox Transformation | 712.993 | 606.664 | 26.294 | 26.294 | 3.200 | 0.903 | 12.826 |
| Exponential Smoothing | 221.442 | 155.624 | 3.668 | 6.586 | 0.821 | 0.889 | 3.842 |
| Mean Forecast Method | 1043.815 | 1023.667 | 45.944 | 45.944 | 5.400 | 0.887 | 19.516 |
| Naive Forecast Method | 259.020 | 183.291 | 6.472 | 7.690 | 0.967 | 0.887 | 4.476 |
| Seasonal Naive Forecast Method | 339.903 | 280.988 | 12.001 | 12.127 | 1.482 | 0.898 | 6.032 |
| Neural Network | 579.195 | 407.790  | 16.483 | 17.028 | 2.151 | 0.907 | 10.049 |

Ultimately we decided that our **ARIMA(0,1,1)** was the best model at forecasting based on the scale-dependent errors outlined in the project.


## <a name="Sources-Cited"></a>Sources Cited

- Hyndman, Rob J., and George Athanasopoulos. ["Forecasting: Principles and Practice"](https://www.otexts.org/fpp) Otexts. May 2012. Web.

- NIST/SEMATECH e-Handbook of Statistical Methods, [Introduction to Time Series Analysis](http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4.htm). June, 2016.

- Schmidt, Drew. ["Autoplot: Graphical Methods with ggplot2"](http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/) Wrathematics, my stack runneth over. June, 2012. Web.

- Shumway, Robert H. & Stoffer David S. ["Time Series Analysis and Its Applications With R Examples"](http://www.stat.pitt.edu/stoffer/tsa4/), 3rd edition. 2012

- ["Stack Exchange"](http://stats.stackexchange.com/) Many contributions made from people on Stack Exchange, we cannot thank you enough.
