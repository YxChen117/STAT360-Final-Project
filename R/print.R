#' Title
#'
#' @param object a mars object
#'
#' @return
#' @export
#'
#' @examples
print.mars <- function(object) {

  cat("mars object\n\n")

  cat("Call:\n",deparse(object$call),"\n")

  cat("\nCoefficients: \n")

  print(object$coefficients)

}
