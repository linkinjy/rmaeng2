#' oneplot
#'
#' one way anova (main effect)
#'
#' @param formula,data,alpha,na.rm
#'
#' @return CI
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
#' z<-data.frame(group,y)
#'
#' @export
#'
#' @importFrom
#' gplots plotmeans

oneplot<-function(formula,data,na.rm=TRUE){
  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

  if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

  y = data[[dp[[2L]]]]
  group = data[[dp[[3L]]]]
  if (!is.factor(group)) stop("The group variable must be a factor.")
  if (!is.numeric(y)) stop("The response must be a numeric variable.")
  print(summary(aov(formula)))
  plotmeans(formula,xlab ="x",barcol = "white")
}


