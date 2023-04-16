#' summary.mars
#'
#' @description It prints a summary of the mars object
#' @param object an R formula
#,
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import stats

summary.mars <- function(object)
{
  for(i in 2:length(object$Bfuncs))
  {
    cat("Coefficient ",names(object$coefficients)[i],":\n")
    for(j in 1:nrow(object$Bfuncs[[i]]))
    {
      cat("covariates:",object$x_names[object$Bfuncs[[i]][j,2]],"split at value t:",object$Bfuncs[[i]][j,3],"\n")
    }
  }
}
