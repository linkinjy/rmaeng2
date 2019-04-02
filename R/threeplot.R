#' threeplot
#'
#' three way anova (main effect, interaction / no repetition)
#'
#' @param formula,data,alpha
#'
#' @return plot, anova
#'
#' @examples
#' a<-as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
#' b<-as.factor(c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
#' c<-as.factor(rep(c(1,2,3),9))
#' y<-c(74,86,76,72,91,87,48,65,56,61,78,71,62,81,77,55,72,63,50,70,60,49,68,64,52,69,60)
#' z<-data.frame(a,b,c,y)
#'
#' @export
#'
#' @importFrom
#' gplots plotmeans
#' stats interaction.plot

threeplot<-function(formula,data,alpha){
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
  opar <- par(mfrow=c(2, 3))
  plotmeans(data[[4]]~data[[1]],xlab=data.n[1],ylab=Y.name,barcol = "white")
  plotmeans(data[[4]]~data[[2]],xlab=data.n[2],ylab=Y.name,barcol = "white")
  plotmeans(data[[4]]~data[[3]],xlab=data.n[3],ylab=Y.name,barcol = "white")
  interaction.plot(data[[1]],data[[2]],data[[4]],xlab=data.n[1],trace.label = data.n[2],ylab = data.n[0])
  interaction.plot(data[[1]],data[[3]],data[[4]],xlab=data.n[1],trace.label = data.n[3],ylab = data.n[0])
  interaction.plot(data[[2]],data[[3]],data[[4]],xlab=data.n[2],trace.label = data.n[3],ylab = data.n[0])
}
