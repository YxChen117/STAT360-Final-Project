#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param object an R formula
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import stats
#'
#'

anova.mars <-function(object){
  cat("\nThe anova of it is:\n")
  print(anova(object))
}

