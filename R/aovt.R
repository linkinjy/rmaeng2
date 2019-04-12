#' aov.t
#'
#' analysis of variance
#'
#' @param formula
#'
#' @return aov(df,sumsq,meansq)
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-as.factor(c(rep(c(1,2,3),4)))
#' y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' stats formula
#' stats aov
#'
aov.t<-function(formula){
s<-summary(aov(formula))
print(s)
}
