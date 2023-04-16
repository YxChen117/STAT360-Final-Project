#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param formula an R formula
#'
#'
#' @export
#'
#' @examples
#'
#' @import stats
#'
#' @import ggplot2

library(ggplot2)
library(gridExtra)
ggplot.mars <- function(formula,which = NULL,data){
  qqnorm(formula$residuals, pch = 16, main = "Residual QQ",
         xlab = "Normal Quantiles",
         ylab = "Residual Quantiles")
  qqline(formula$residuals, col = "red")

  p <- ggplot(data,aes(x=formula$fitted.values,y=formula$residuals)) + geom_smooth()+
    geom_point(alpha = 0.2) + geom_abline(intercept = 0,slope = 0,color = "red") +
    labs(title = "Residuals vs Fitted",x = "Fitted" , y = "Residuals")
  p

  qqplot <- ggplot(data, aes(sample = formula$residuals))+ stat_qq() + stat_qq_line()+
    labs(title = "Normal Q-Q plot",x = "Normal Quantiles" , y = "Residual Quantiles")
  qqplot
  grid.arrange(p, qqplot, nrow=2)
}

