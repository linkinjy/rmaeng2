#' munova3
#'
#' three way anova (no repetition / unknown value)
#'
#' @param formula,data,alpha
#'
#' @return ci, plot, anova
#'
#' @examples
#' a<-as.factor(c(rep(1,9),rep(2,9),rep(3,9)))
#' b<-as.factor(c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3))
#' c<-as.factor(rep(c(1,2,3),9))
#' y<-c(74,86,76,72,91,87,48,65,56,61,NA,71,62,81,77,55,72,63,50,70,60,49,68,64,52,69,60)
#' z<-data.frame(a,b,c,y)
#'
#' @export
#'
#' @importFrom
#' stats aov
#' stats na.omit
#' stats anova
#' stats lm

munova3<-function(formula,data){
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

  j<-suppressWarnings(anova(lm(formula)))
  print(j)
  l<-j[1,1]
  m<-j[2,1]
  n<-j[3,1]

  t<-sum(na.omit(data[[4]]))
  ho<-data[!complete.cases(data),]

  dd<-ho[1,1]
  dd<-as.numeric(dd)
  t1<-sum(na.omit(y[data[1]==dd]))

  ff<-ho[1,2]
  ff<-as.numeric(ff)
  t2<-sum(na.omit(y[data[2]==ff]))

  gg<-ho[1,3]
  gg<-as.numeric(gg)
  t3<-sum(na.omit(y[data[3]==gg]))

  tt1<-sum(na.omit(y[data[1]==dd & data[2]==ff]))
  tt2<-sum(na.omit(y[data[1]==dd & data[3]==gg]))
  tt3<-sum(na.omit(y[data[2]==ff & data[3]==gg]))
  om<-(t-(l+1)*t1-(m+1)*t2-(n+1)*t3+(l+1)*(m+1)*tt1+(l+1)*(n+1)*tt2+(m+1)*(n+1)*tt3)/(l*m*n)
  data[[4]]<-ifelse(is.na(data[[4]]),om,data[[4]])
  print("replicate na value ")
  summary(aov(data[[4]]~data[[1]]+data[[2]]+data[[3]]+data[[1]]*data[[2]]+data[[1]]*data[[3]]+data[[2]]*data[[3]]))
}
