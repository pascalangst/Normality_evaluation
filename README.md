# Normality_evaluation
This application allows to evaluate the normality of a variable of a data frame. The data frame must be loaded on the first tab ``Upload data``. After that, a variable must be selected from the frame in the ``Select column`` tab. The following tabs help to decide whether the variable is normally
distributed based on visualizations (Histogram, Boxplot, QQ Plot), kurtosis, skewness, and a Shapiro-Wilk test.

## Step-by-step guide

Open the code (preferrably in ![R studio](https://www.rstudio.com/products/rstudio/download/)) and run it using the ``Run App`` button in the top right corner.  

<img src="figures/1.png" width="500"> 

----------------------

Select your data with ``Browse`` and adjust the options as needed.  

<img src="figures/2.png" width="500">  

----------------------

Click the field to update to your column names and choose the variable of interest from the drop down menu.

<img src="figures/3.png" width="500">  

----------------------

Apply transormations to the variable (optional).  
Visualize your variable with a histogram or a boxplot.  

<img src="figures/4.png" width="500">  

----------------------

Display a QQ Plot of the variable.

<img src="figures/5.png" width="500">  

----------------------

Get the Kurtosis and the Skewness of the variable.  

<img src="figures/6.png" width="500">  

----------------------

Apply a Shapiro-Wilk test. Here the test suggests no deviation from normality (p > 0.05).  

<img src="figures/7.png" width="500">  

----------------------
