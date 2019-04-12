#' mean.diff
#'
#' population mean difference
#'
#' @param formula,method
#'
#' @return diff,lwr.ci,upr,ci,p-value,ci plot
#'
#' @examples
#' y1<-c(8.44,8.36,8.28)
#' y2<-c(8.59,8.91,8.6)
#' y3<-c(9.34,9.41,9.69)
#' y4<-c(8.92,8.92,8.74)
#' y <- c(y1, y2, y3,y4)
#' n <- rep(3, 4)
#' group <- rep(1:4, n)
#' group<-as.factor(group)
#' z<-data.frame(y,group)
#'
#' @export
#'
#' @importFrom
#' DescTools PostHocTest

mean.diff<-function(formula,method="lsd"){
  s<-aov(formula)
  if(method=="hsd"){
    as<-PostHocTest(s,method="hsd")
    print(as)
    plot(as)
  }else if(method=="bonferroni"){
    as<-PostHocTest(s,method="bonferroni")
    print(as)
    plot(as)
  }else if(method=="scheffe"){
    as<-PostHocTest(s,method="scheffe")
    print(as)
    plot(as)
  }else if(method=="duncan"){
    as<-PostHocTest(s,method="duncan")
    print(as)
    plot(as)
  }else if(method=="newmankeuls"){
    as<-PostHocTest(s,method="newmankeuls")
    print(as)
    plot(as)
  }else{
    as<-PostHocTest(s,method="lsd")
    print(as)
    plot(as)
  }
}
