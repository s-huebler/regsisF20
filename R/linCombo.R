#' Estimate and CI for Linear Combination of Parameters
#'
#' @param Y, a vector of outputs
#' @param X , a design matrix
#' @param a , parameter values
#' @param l , a number (estimate of output given parameters)
#' @param alpha , confidence level
#'
#' @return , a confidence interval and pairs figure
#' @export
#'
#' @examples
#' clerical<-data.frame(Hours=c(128.5, 113.6, 146.6, 124.3, 100.4, 119.2, 109.5, 128.5, 131.2, 112.2),
#' Mail=c(7781,7004,7267,2129,4878,3999,11777,5764,7392,8100),
#' Gifts=c(100,110,61,102,45,144,123,78,172,126),
#' Charge=c(886,962,1342,1153,803,1127,627,748,876,685),
#' Returns=c(235,388,398,457,577,345,326,161,219,287),
#' Checks=c(644,589,1081,891,537,563,402,495,823,555)
#' )
#'
#' lm1<-stats::lm(Hours~Mail+Gifts+Charge+Returns+Checks, data=clerical)
#' design<-stats::model.matrix(lm1)
#' a1<-matrix(c(0,1,-1,0,0,0), nrow=6)
#'
#' coeff<-lm1$coefficients
#' l1<-coeff[2]-coeff[3]
#'
#' linCombo(clerical$Hours, design, a1, l1 )
#'
linCombo<-function(Y,X,a,l, alpha=0.95){
  n = dim(X)[1]
  kplus1 = dim(X)[2]
  df = n-(kplus1)

  tval=stats::qt(alpha,df)

  betaHat= solve(t(X) %*% X) %*% t(X) %*% Y
  SSE=t(Y) %*% Y-t(betaHat) %*% t(X) %*% Y
  s=sqrt(SSE/df)

  b1<-l-tval*s*sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
  b2<-l+tval*s*sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
  Int=c("Lower"=min(b1,b2), "Upper"=max(b1,b2))

  fig<-graphics::pairs(X[,-1])
  print(fig)

  Int}
