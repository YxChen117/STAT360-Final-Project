#student: Yuxiang Chen & Kangjie Zhang
#SFU ID: 301368904  301385112
#library(rpart)
library(earth)
library(dplyr)
#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @param formula an R formula
#' @param data a data frame containing the data for the model
#' @param control an object of class 'mars.control'
#'
#'
#' @return an object of class 'mars'
#' @export
#'
#' @examples
#'
#' @import stats
#' @import ISLR

#Mars Function

mars <- function(formula, data, control=NULL, ...) {
  cc <- match.call()
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  fwd <- fwd_stepwise(y,x[,-1],control)
  bwd <- bwd_stepwise(fwd,control)
  if(is.null(control)){
    control <- mars.control()
  }
  fit <- lm(y~.-1, data=data.frame(y=y, bwd$B))
  out <- c(list(call=cc, formula=formula, y=y, B=bwd$B, Bfuncs=bwd$Bfuncs,x_names=colnames(x)[-1]), fit)
  class(out) <- c("mars", class(fit))
  return(out)
}

#The constructor of
new_mars.control <- function(Mmax,d,trace) {
  structure(list(Mmax=Mmax,d=d,trace=trace), class='mars.control')}

#The validator
validate_mars.control <- function(control) {
  #stopifnot(is.integer(control$Mmax),is.numeric(control$d),
   #         is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  #if(control$Mmax %% 2 > 0) {
   # control$Mmax <- 2*ceiling(control$Mmax/2)
  #  warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  return(control)
}



mars.control<-function(Mmax=2,d=3,trace=FALSE)
{
  control <- new_mars.control(Mmax,d,trace)
  validate_mars.control(control)
  return(control)
}

#Hinge Function
h <- function(s,v,t){
  return(pmax(0,s*(v-t)))
}

split_points <- function(x,Bm) {
  output <- sort(unique(x[Bm>0]))
  output <- output[-length(output)]
  return(output)
}

init_B <- function(N,Mmax) {
  # Input: N- # of rows; Mmax: # of basis funcs
  # output: a N by (Mmax+1) dataframe
  B <- data.frame( matrix(NA,nrow=N,ncol=(Mmax+1)) )
  B[,1] <- 1 # first column for intercept: B0
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}


#The forward setpwise
fwd_stepwise <- function(y,x,control){
  N <- length(y)
  n <- ncol(x)
  B <- init_B(N,control$Mmax)
  Bfuncs <- vector(mode = "list",length = control$Mmax+1)
  splits <- data.frame(m=rep(NA,control$Mmax),v=rep(NA,control$Mmax),t=rep(NA,control$Mmax))#in ex
  for (i in 1:(control$Mmax/2)){
    M <- 2*i -1
    lof_best <- Inf
    for (m in 1:M){
      vset <- setdiff(1:n,Bfuncs[[m]][2]) #Bfuncs[[m]]["v"]
      for (v in vset){
        tt <- split_points(x[,v],B[,m])
        for(t in tt){
          Bnew = data.frame(B[,(1:M)],
                            B1 = B[,m]*h(+1,x[,v],t),
                            B2 = B[,m]*h(-1,x[,v],t))
          gdat <- data.frame(y=y, Bnew)
          lof <- LOF(y~.,gdat,control)
          if (lof < lof_best){
            lof_best <- lof
            mstar <- m
            vstar <- v
            tstar <- t
            splits[M,] <- c(m,v,t)
          }
        }
      }
    }

    m <- splits[M,1]; v <- splits[M,2]; t <- splits[M,3]
    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]],c(s=-1,vstar,tstar))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]],c(s=+1,vstar,tstar))
    B[,M+1] <- B[,m] * h(-1,x[,v],t)
    B[,M+2] <- B[,m] * h(+1,x[,v],t)

  }
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  #return(list)
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}

#The backward Stepwise
bwd_stepwise <- function(fwd,control){
  Mmax <- ncol(fwd$B)-1
  Jstar <- 1:(Mmax+1)
  Kstar <- Jstar
  dat <- data.frame(y = fwd$y,fwd$B)
  lofstar <- LOF(y~.,dat,control)
  for (M in ((Mmax+1):2)){
    b <- Inf
    L <- Kstar
    if(control$trace) cat("L:",L,"\n")
    for (m in 2:(control$Mmax+1)){
      K<- setdiff(L,m)
      gdat <- data.frame(y = fwd$y,fwd$B[,K])
      lof <- LOF(y~.,gdat,control)
      if (lof > b){
        b <- lof
        Kstar <- K
      }
      if (lof < lofstar){
        lofstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs = fwd$Bfuncs[Jstar]))
}

LOF <- function(formula,data,control){
  ff <- lm(formula,data)
  RSS <- sum((ff$res)^2)
  M <- length(ff$coefficient) - 1
  N <- nrow(data)
  Ctilda_m <- sum((hatvalues(ff)))+control$d*M
  GCV <- RSS * N/((N - Ctilda_m)^2)
  return(GCV)
}
