#' aova2
#'
#' two way anova (no repetition / unknown value)
#'
#' @param formula,data
#'
#' @return ci plot, anova
#'
#' @examples
#' a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
#' b<-c(rep(c(1,2,3),4))
#' b<-factor(b)
#' y<-c(NA,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
#' z<-data.frame(a,b,y)
#'
#' @export
aova2<-function(formula,data){
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
  l<-j[1,1]
  m<-j[2,1]
  t<-sum(na.omit(data[[3]]))
  ho<-data[!complete.cases(data),]
  print(j)

  dd<-ho[1,1]
  dd<-as.numeric(dd)
  t1<-sum(na.omit(y[data[1]==dd]))

  ff<-ho[1,2]
  ff<-as.numeric(ff)
  t2<-sum(na.omit(y[data[2]==ff]))

  om<-((t1*(l+1)+t2*(m+1)-t)/(l*m))
  data[[3]]<-ifelse(is.na(data[[3]]),om,data[[3]])


  print("replicate na value ")

  summary(aov(data[[3]]~data[[1]]+data[[2]]))

}
