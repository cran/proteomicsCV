
#' @importFrom stats sd
#' @export
protLogCV<-function(logData, base){
  val=log(base)
  data=logData*val
  cv=apply(data, 1, function(x) sqrt(exp(stats::sd(x, na.rm = TRUE)^2)-1))*100
  cv
}

#' @export
protCV<-function(data){
    cv=apply(data, 1, function(x) sd(x)/mean(x))*100
}
