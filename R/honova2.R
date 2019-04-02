#' honova2
#'
#' two way anova (repetition / mixed model)
#'
#' @param formula,data,alpha,k
#'
#' @return CI, plot
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
#' stats anova
#' stats lm
#' stats df
#' stats pf
#' stats qt
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_point
#' ggplot2 geom_errorbar
#' ggplot2 ymax
#' ggplot2 ymin
#' ggplot2 labs
#' ggplot2 geom_text
#'
honova2<-function(formula,data,alpha,k){
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
  j[1,4]<-j[1,3]/j[3,3]
  j[1,4]
  j[1,5]<-1-pf(j[1,4],j[1,1],j[4,1])
  j[1,5]
  q<-j[2,3]
  s<-j[4,1]
  f<-j[4,3]
  ky<-j[3,1]
  kyo<-j[3,3]
  l<-j[1,1]+1
  m<-j[2,1]+1
  df<-((q+l*kyo-f)^2)/((q^2/(m-1))+((l*kyo)^2/ky)+(f^2/s))
  dd<-df-floor(df)
  tt<-qt(1-alpha/2,floor(df))-dd*(qt(1-alpha/2,floor(df))-qt(1-alpha/2,df))
  ac<-sqrt((q+l*kyo-f)/(l*m*k))

  a.levels<-levels(factor(a))
  a.mean <- NULL
  La<-NULL
  Ua<-NULL
  a.lower<-NULL
  a.upper<-NULL
  a.error<-tt*ac

  for(i in a.levels){
    a.mean[i]<-round(mean(y[a==i]),2)
    La[i]<-round(a.mean[i]-a.error,2)
    Ua[i]<-round(a.mean[i]+a.error,2)
    a.lower[i]<-as.numeric(La[i])
    a.upper[i]<-as.numeric(Ua[i])
  }

  a.CI<-data.frame(a.levels,a.lower,a.mean,a.upper)
  print(a.CI)
  ggplot(a.CI,aes(x=a.CI$a.levels,y=a.CI$a.mean))+geom_point(size=3)+geom_errorbar(aes(ymax=a.CI$a.upper, ymin=a.CI$a.lower))+geom_text(aes(label = round(a.CI$a.mean,2)),vjust = 1.5)+labs(y="A CI")+labs(x = "A")

}
