#' Expected Values of a Two-Way Table
#'
#' Calculates the expected values for a multinomial 2xR data frame
#'
#' @param df , a data frame with  2 rows and R columns
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#'
expect_2xR<-function(df){
  cols<-names(df)
  ret=data.frame(matrix(NA, nrow=2, ncol=length(cols)),row.names=row.names(df))
  colnames(ret)<-cols
  for(j in 1:ncol(df)){
    for(i in 1:nrow(df)){
      ret[i,j]=sum(df[i,])*sum(df[,j])/sum(df)
    }
  }
  ret
}
