#' Print Confidence Intervals
#'
#' Takes in a list of 2 numbers and prints it as (a,b)
#'
#' @param ci , a list or vector of length 2
#'
#' @return A string
#' @export
#'
#' @examples
#' print_confidenceInts(c(2,3))
#'
print_confidenceInts<-function(ci){
  if (is.character(ci)){
    cat(ci)
  } else {
        cat(paste0( "(", paste(ci[1:2], collapse=",") , ")"))
  }
}
