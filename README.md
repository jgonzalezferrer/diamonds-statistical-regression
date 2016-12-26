# diamonds-statistical-regression: Statistical Regression Analysis of the diamonds dataset.

The diamonds dataset contains the prices and other attributes such as the caratage, clarity or colour of 308 stones. The goal of this project is to come up with a Multiple Linear Regression model and compare this initial model against different solutions to improve the initial simple approach.

Installation
----------- 
Dependencies:
````
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("car")
install.packages("lmtest")
install.packages("tseries")
install.packages("astsa")
install.packages("nlme")
````

Statistical Learning models
----------- 

* [Linear Regression](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html): the function <i>lm</i> is used to fit linear models. We will include other two different interaction terms: a) clusters of diamonds, b) square of the carat.

* [Generalized Least Square](https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/gls.html): the function <i>gls</i> fits a linear model using generalized least squares. The errors are allowed to be correlated.