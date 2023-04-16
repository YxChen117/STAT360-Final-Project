#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param object an R formula
#' @param newdata a data frame containing the data for the model

#'
#' @return an object of class 'mars'
#' @export
#'
#' @examples
#'
#' @import stats

predict.mars<- function(object,newdata){
  if(missing(newdata) || is.null(newdata)) {
    B <-as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula, data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1]
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}
make_B<- function(X, Bfuncs) {
  Xout<-init_B(N=nrow(X), Mmax=length(Bfuncs)-1)
  for (i in 2:ncol(Xout)) {
    B <- Xout[1]
    for (j in 1:nrow(Bfuncs[[i]])) {
      B <- B*h(Bfuncs[[i]][j,"s"], X[,Bfuncs[[i]][j,"v"]],Bfuncs[[i]][j,"t"])
    }
    Xout[i] <-B
  }
  colnames(Xout) <- paste0("B", (0:(ncol(Xout)-1)))
  return(as.matrix(Xout))
}
