# STAT360-Final-Project 
Multivariate Adaptive Regression Splines (MARS)

Team Member1: YuXiang Chen 301368904
Team Member2: Kangjie Zhang 301385112

Building a R package, Using MARS Regression

The List of file: first include NAMESPACE, LICENSE, and DESCRIPTION
A list os R function R files: 
mars.R, which contains functions like hinge function, mars class function, and forward/backward stepwise function and so on.
anova.R, print out the p-value of the lm. 
print.R, Prints out a mars object, print out the coefficient after regression.
summary.R, prints a summary of the mars object and the summary of the hinges of each basis function.
predict.R, Predict with an mars model for new data, returns the predicted basis function.
plot.R and ggplot.R, use plot and ggplot to plot the fitted vs residual plot, and the normal Q-Q plot
data.R

And another list of testdata files. Such as testmc.rda, testfwd_stepwise.rda RDATA files.
A vignettes Rmd file.
Programmed functions in R, which include anova(), plot(), predict(), print() and summary() methods for mars objects


Then we tested functions, including developed standard methods (print, plot, and summary) for MARS object, using the mtcars data, wage data, and the given marstestdata data.
Designed vignettes to document functional requirements of the package and function calls on data for better user understanding.
