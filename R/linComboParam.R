#' Estimates and CIs for All Rows of a Design Matrix
#'
#' @param df , a named data frame
#' @param ind , the index for the independent variable column
#' @param dep , the indicies for the dependent variable columns
#' @param a , parameter values
#' @param alpha , confidence level
#'
#' @return a list of all the estimates and confidence intervals for the
#' parameter values as set by the design matrix created by the independent and
#' dependent variables; a pairs plot; a series of scatter plots for all of the
#' dependent variables against the dependent variable, and a plot of the estimates
#' and their confidence interval for every row of the design matrix.
#'
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
#' linComboParam(clerical, 1, c(2,3,4,5,6), a1 )
#'
linComboParam<-function(df,ind, dep, a, alpha=0.95){
  Y=as.matrix(df[ind],ncol=1)
  vars=names(df[dep])

  Yname<-names(df[ind])

  f<-stats::as.formula(
    paste(Yname, paste(vars, collapse="+"), sep="~")
  )

  X=stats::model.matrix(f, data=df)

  n = dim(X)[1]
  kplus1 = dim(X)[2]
  dfree = n-(kplus1)

  tval=stats::qt(alpha,dfree)

  betaHat= solve(t(X) %*% X) %*% t(X) %*% Y
  SSE=t(Y) %*% Y-t(betaHat) %*% t(X) %*% Y
  s=sqrt(SSE/dfree)

  yhat<-t(a) %*% betaHat

  b1<-yhat-tval*s*sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
  b2<-yhat+tval*s*sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
  ret=c("Estimate"=yhat,"Lower"=min(b1,b2), "Upper"=max(b1,b2))
  print(ret)

  cis<-data.frame("Row"=1:nrow(Y), "Estimate"=NA, "Lower"=NA, "Upper"=NA)

  for (i in 1:nrow(Y)){
    new<-X[i,]
    est<-t(new) %*% betaHat

    int1<-est-tval*s*sqrt(t(new) %*% solve(t(X) %*% X) %*% new)
    int2<-est+tval*s*sqrt(t(new) %*% solve(t(X) %*% X) %*% new)

    cis[i,2:4]<-c(est, min(int1,int2), max(int1,int2))
  }

  pointsPlot<-ggplot2::ggplot(cis, aes(x = Row, y = Estimate))+
    geom_point(size=1, show.legend = FALSE)+
    geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2)+
    xlab("Row of Design Matrix")+
    ylab("Confidence Intervals")+
    labs(title="Estimates and Confidence Intervals", caption = "Estimates based off parameters \n as described by the rows \n of the design matrix")


  pairPlot<-graphics::pairs(X[,-1])

  for (s in vars){
    plotVars<-ggplot2::ggplot(data=as.data.frame(df))+
      geom_point(aes(x=unlist(df[,s]), y=Y))+
      ggtitle(paste(Yname, "vs", s, sep=" "))+
      xlab(as.character(s))+
      ylab(Yname)
    print(plotVars)
  }

  print(pairPlot)
  print(pointsPlot)

  print(cis)
  invisible(as.list(cis))
}
