
#' Classic Confidence Intervals for Binomial Estimate
#'
#' Calculates the alpha% confidence interval for a binomial estimate.
#'
#' @param x , number of successes
#' @param n , number of trials
#' @param alpha , alpha
#'
#' @return Confidence interval as a list of lowerbound, upperbound
#' @export
#'
#' @examples
#' x=5; n=15; classic_ci(x,n)
#'
classic_ci<- function(x,n,alpha=0.05){
  z<-stats::qnorm(alpha/2)
  if (n==0){
    return ("Not Enough Samples")
  }
  x<- x/n
  discrim<-x*(1-x)/n+z^2/(4*n^2)
  if (discrim < 0 | x>1)
  {
    return ("Discriminant Less Than Zero")
  }

  Ci<- c(x-z*sqrt(x*(1-x)/n),x+z*sqrt(x*(1-x)/n))
  Ci<-c(min(Ci), max(Ci))

  class(Ci)<-"confidenceInts"
  Ci
}
