#' mean.est
#'
#' population mean of main effect
#'
#' @param formula,data,factor,alpha
#'
#' @return population mean, graph
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
#' stats model.frame
#' stats lm
#' stats anova
#' ggplot2 ggplot
#' ggplot2 aes
#' ggplot2 geom_errorbar
#' ggplot2 geom_point
#' ggplot2 geom_text
#' ggplot2 xlab
#' ggplot2 ylab

mean.est <- function(formula,data,factor,alpha){

  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)
  y.name<-formula.t[2]

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }

  anv<-suppressWarnings(anova(lm(formula)))
  E<-anv["Residuals","Df"]
  VE<-anv["Residuals","Mean Sq"]

  lv<-levels(factor)

  num<-NULL
  ci<-NULL
  dat.mean<-NULL
  lw<-NULL
  up<-NULL

  for(i in lv){
    num[i]<-length(which(factor==i))
    print(num[i])
    ci[i]<-qt(1-alpha/2,E)*sqrt(VE/(num[i]))

    dat<-subset(data,factor==i)

    dat.mean[i]<-mean(dat[[y.name]])

    lw[i]<-as.numeric(dat.mean[i]-ci[i])
    up[i]<-as.numeric(dat.mean[i]+ci[i])
  }

  result<-data.frame(lv, lw, dat.mean, up)

  names(result)[1]<-c(paste(deparse(substitute(factor))))
  names(result)[3]<-c("mean")

  print(result)

  ggplot(result,aes(x=lv, y=dat.mean))+
    geom_errorbar(aes(ymax=up, ymin=lw),colour="dodgerblue3",size=1) + geom_point(size=3,colour="dodgerblue4")+
    geom_text(aes(label = round(dat.mean,2)),vjust = 1.5) + xlab(names(result[1])) + ylab("Mean")
}
