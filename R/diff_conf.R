#' Confidence Interval for the True Difference Between Two Proportions
#'
#' Takes in a one way table and estimates the difference between the true proportions of 2 categories. Uses Bonferroni correction.
#'
#' @param tab , a one way table
#' @param a , index for the fist estimate
#' @param b , index for the second estimate (greater than a)
#' @param FWER , family wise error rate
#' @param ntest , number of multiple comparisons (default value assumes all pairwise comparisons checked)
#'
#' @return List of estimate of the difference, lower bound of confidence interval, and upperbound of confidence interval
#' @export
#'
#' @examples
#' tab=c("x"=1, "y"=2, "z"=3); a=1; b=3; diff_conf(tab, a, b)
#'
diff_conf<-function(tab, a, b, FWER=0.05, ntest = (length(tab)*(length(tab)-1))/2){
  alpha=FWER/ntest
  p1<-tab[a]/sum(tab)
  p2<-tab[b]/sum(tab)
  diffEst<-p1-p2
  z<- stats::qnorm(alpha/2)
  discrim<-(p1*(1-p1)+p2*(1-p2)+2*p1*p2)/length(tab)

  low<- diffEst + z* sqrt(discrim)
  high<- diffEst- z*sqrt(discrim)

  ret<-c(diffEst, low, high)
  names(ret)<-c("Estimate", "Lower Bound", "Upper Bound")
  ret
}
