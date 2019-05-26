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
#' stats anova
#' stats lm
#'
aov.t<-function(formula, ranfac=NULL){

  if(is.null(ranfac)){
    anv<-summary(aov(formula))
    anv<-do.call(rbind.data.frame, anv)
    print(anv)
  }else{
    anv<-anova(lm(formula))
    fac.name<-rownames(anv)

    fac.main<-subset(fac.name, fac.name!='Residuals')
    dele<-fac.main[grep(":", fac.main)]
    for(d in dele) fac.main<-subset(fac.main, fac.main!=d)
    fac.main<-subset(fac.main, fac.main!=ranfac)


    for(f in fac.main){
      raninter<-paste(f,":",ranfac, sep='')
      raninter2<-paste(ranfac,":",f, sep='')

      raninter<-subset(fac.name, fac.name==raninter)
      raninter2<-subset(fac.name, fac.name==raninter2)

      if(length(raninter)==0) raninter<-raninter2

      if(length(raninter)==0){
        anv[f,"F value"]<-anv[f,"Mean Sq"]/anv['Residuals',"Mean Sq"]
        anv[f,"Pr(>F)"]<-1-pf(anv[f,"F value"], anv[f,"Df"], anv['Residuals',"Df"])
      }else{
        anv[f,"F value"]<-anv[f,"Mean Sq"]/anv[raninter,"Mean Sq"]
        anv[f,"Pr(>F)"]<-1-pf(anv[f,"F value"], anv[f,"Df"], anv[raninter,"Df"])
      }
    }
    print(anv)
  }

}
