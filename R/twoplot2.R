#' twoplot2
#'
#' two way anova (main effect, interaction / repetition)
#'
#' @param formula,data
#'
#' @return plot, anova
#'
#' @examples
#' a<-c(1,1,2,2,3,3)
#' a<-as.factor(c(rep(a,6)))
#' b<-as.factor(c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)))
#' y<-c(305,302,322,325,320,322,335,337,350,348,342,344,366,364,326,324,338,336,372,374,330,330,348,348,376,373,327,330,350,350,348,350,310,308,330,328)
#' z<-data.frame(a,b,y)
#'
#' @export
#'
#' @importFrom
#' gplots plotmeans
twoplot2<-function(formula,data){
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
  mo<-anova(lm(formula))
  print(anova(lm(formula)))
  if(mo[3,5]>0.1){
    print("stop")
  }else{

  }


  opar <- par(mfrow=c(2, 2))
  plotmeans(data[[3]]~data[[1]],xlab="a",ylab=Y.name,barcol = "white")
  plotmeans(data[[3]]~data[[2]],xlab="b",ylab=Y.name,barcol = "white")
  interaction.plot(data[[1]],data[[2]],data[[3]],xlab=data.n[1],trace.label = data.n[2],ylab=data.n[0])
}
