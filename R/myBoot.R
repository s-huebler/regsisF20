#' Bootstrap From Scratch
#'
#' @param data , a data frame where the first column has dependent variable and all other columns are independent
#' @param iter , number of bootstrap iterations
#' @param alpha , alpha level
#'
#' @return , histograms for each beta estimate, confidence intervals for each beta estimate, summary statistics 
#' the iter length list of beta estimates
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
#' myBoot(clerical, 1000)
#' 
myBoot<-function(data, iter, alpha=.20){
  #Defining the degrees of freedom and z stat to be used in conf. ints 
  dfree=nrow(data)-ncol(data)
  t<-qt(1-alpha/2, dfree)
  z<-qnorm(0.80)
  
  #Counter for singular matrices  
  fails<-0
  
  #Empty data frame where beta estimates will be stored
  betas<-data.frame(matrix(NA, nrow=iter, ncol=ncol(data)))
  names(betas)<-names(data)
  names(betas)[1]<-"Intercept"
  
  #Empty data frame where resampled data will be stored   
  newDF<-data.frame(matrix(NA, nrow=nrow(data), ncol=ncol(data)))
  names(newDF)<-names(data)
  
  #Loop through the number of iterations 
  for (i in 1:iter){
    
    #Sample the rows. Ind is a vector of indices that is the same
    #length as the number of rows of the original data frame
    ind<-sample(1:nrow(data), nrow(data), replace=TRUE)
    
    #Construct new data frame s.t. each row is defined by the ind vecor   
    for(j in 1:nrow(data)){
      newDF[j,]<-data[ind[j],]
    }
    
    
    #Linear algebra
    Y<-as.matrix(newDF[,1])
    X<-newDF[-1]
    X<-as.matrix(cbind(1,X))
    
    #Taking the inverse. try()
    iXTX<-try(solve(t(X)%*%X), silent=TRUE)
    
    # If try caught an error then the singular matrix counter increases by 1   
    if(inherits(iXTX, "try-error")){fails<-fails+1}
    
    #Otherwise calculate beta estimates for the new data frame and add to 
    #the betas matrix
    else{
      betaHat<-iXTX %*% t(X) %*% Y
      betas[i,]<-betaHat
    }
    
    #End of iterations loop 
  }
  
  #Remove na rows from betas data frame
  betas<-na.omit(betas)
  
  #Summary statistics
  summ<-sapply(betas, function(x){
    ave<-mean(x)
    s<-sd(x)
    min<-min(x)
    max<-max(x)
    n<-length(x)
    return(list("Count"=n ,"Mean"=ave, "Std Dev"=s, "Min"=min, "Max"=max))
  })
  
  #Confidence intervals
  cis<-mapply(betas, alpha, FUN=function(x,a){
    b1<-quantile(x, probs=a/2)
    b2<-quantile(x, probs=1-a/2)
    L<-min(b1,b2)
    U<-max(b1,b2)
    ret<-c( "Lower"=L, "Upper"=U)
    ret
  })
  
  #The list to be returned  
  ret<-list("Summary Statistics"=summ, "Confidence Intervals"=cis, "Singular Matrices"=fails)
  
  #Printing the return to the console here because otherwise it shows up in the 
  #middle of histograms    
  print(ret)
  
  #Histograms  
  mapply(betas, 0:(ncol(betas)-1), 
         FUN=function(vec, index){hist(vec, 
                                       main=paste("Histogram of Beta",index), 
                                       xlab=paste("Beta", index, "Values"))},USE.NAMES = TRUE)
  
  #Return what was printed before as an invisible list  
  invisible(ret)
  
}