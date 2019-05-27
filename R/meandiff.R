#' mean.diff
#'
#' population mean difference
#'
#' @param formula,method,fac,two,ran
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

mean.diff<-function(formula,method="lsd",fac=NULL,two=FALSE,ran=FALSE){
  s<-aov(formula)

  if(ran==FALSE){
    if(method=="hsd"){
      as<-PostHocTest(s,method="hsd")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }else if(method=="bonferroni"){
      as<-PostHocTest(s,method="bonferroni")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }else if(method=="scheffe"){
      as<-PostHocTest(s,method="scheffe")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }else if(method=="duncan"){
      as<-PostHocTest(s,method="duncan")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }else if(method=="newmankeuls"){
      as<-PostHocTest(s,method="newmankeuls")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }else{
      as<-PostHocTest(s,method="lsd")
      par(mfrow=c(2,2))
      print(as)
      plot(as)
    }
  }else{
    if(method=="hsd"){
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="hsd")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="hsd")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="hsd")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="hsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="hsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="hsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }else if(method=="bonferroni"){
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="bonferroni")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="bonferroni")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="bonferroni")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="bonferroni")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="bonferroni")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="bonferroni")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }else if(method=="scheffe"){
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="scheffe")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="scheffe")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="scheffe")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="scheffe")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="scheffe")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="scheffe")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }else if(method=="duncan"){
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="duncan")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="duncan")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="duncan")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="duncan")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="duncan")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="duncan")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }else if(method=="newmankeuls"){
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="newmankeuls")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="newmankeuls")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="newmankeuls")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="newmankeuls")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="newmankeuls")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="newmankeuls")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }else{
      if(fac=="a"&&two==FALSE){
        as<-PostHocTest(s,which="a",method="lsd")
        print(as)
        plot(as)
      }else if(fac=="b"&&two==FALSE){
        as<-PostHocTest(s,which="b",method="lsd")
        print(as)
        plot(as)
      }else if(fac=="c"&&two==FALSE){
        as<-PostHocTest(s,which="c",method="lsd")
        print(as)
        plot(as)
      }else if(fac=="a"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="lsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="b"&&two==TRUE){
        as<-PostHocTest(s,which=c("a","c"),method="lsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }else if(fac=="c"&&two==TRUE){
        as<-PostHocTest(s,which=c("b","c"),method="lsd")
        print(as)
        par(mfrow=c(1,2))
        plot(as)
      }
    }
  }
}
