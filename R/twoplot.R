#' twoplot
#'
#' two way anova (main effect / no repetition)
#'
#' @param formula,data
#'
#' @return anova
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-c(rep(c(1,2,3),4))
#' b<-factor(b)
#' y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' gplots plotmeans

twoplot<-function(formula,data){
  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(stats::model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)
  Y.name<-formula.t[2]

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }
  print(summary(aov(formula)))

  opar <- par(mfrow=c(1, 2))
  plotmeans(data[[3]]~data[[1]],xlab=data.n[1],ylab=Y.name,barcol = "white")
  plotmeans(data[[3]]~data[[2]],xlab=data.n[2],ylab=Y.name,barcol = "white")
}

