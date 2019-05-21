#' dispersion
#'
#' estimation of dispersion
#'
#' @param formula,rc,data,ranmodel
#'
#' @return result of dispersion
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

dispersion <- function(formula, rc=TRUE, data, ranmodel){

  Call<-match.call()
  indx<-match(c("formula","data"),names(Call),nomatch=0L)
  if(indx[1]==0L)
    stop("a 'formula' argument is required")
  temp<-Call[c(1L,indx)]
  temp[[1L]]<-quote(model.frame)
  m<-eval.parent(temp)
  Terms<-attr(m,"terms")

  formula.t<-as.character(formula)

  data.n<-strsplit(formula.t[3]," \\+ ")[[1]]

  if(data.n[1]=="."){
    var.list<-colnames(data)[colnames(data)!=Y.name]
  } else{
    temp1<-unlist(sapply(data.n,strsplit," "))
    var.list<-unique(temp1[temp1!=" " & temp1 !="*"& temp1!=""])
  }

  ranmod<-paste(deparse(substitute(ranmodel)))

  anv<-suppressWarnings(anova(lm(formula)))
  VE<-anv["Residuals","Mean Sq"]

  fac.name<-rownames(anv)
  ran.fac<-fac.name[grep(ranmod, fac.name)]

  fac.main<-data.frame(fac.name)
  dele<-fac.name[grep(":", fac.name)]
  for(i in dele) fac.main<-subset(fac.main, fac.main!=i)
  fac.main<-subset(fac.main, fac.main!='Residuals')


  v<-NULL
  if(rc==TRUE){ ###?ݺ??? ??��
    level<-NULL
    for(i in fac.main[,1]){
      level[i]<-anv[i,"Df"]+1
    }

    dft<-sum(anv[,"Df"])
    r<-(dft+1)/prod(level)

    for(i in ran.fac){

      inter<-i[grep(":", i)]

      v[[i]]<-anv[i,"Mean Sq"]

      if(length(inter)!=0){ ###??ȣ?ۿ?
        sp.inter<-strsplit(inter, split= ":")
        sp.inter<-levels(factor(sp.inter[[1]]))
        lv<-NULL

        for(k in fac.main[,1]){
          lv[k]<-level[k]
          for(j in sp.inter) if(k==j) lv[k]<-1
        }

        num<-prod(lv)*r
        disp<-(v[[i]]-VE)/num
      }

      else{ ###??ȿ??
        lv<-length(levels(data[[i]]))
        num<-length(which(data[[i]]==lv))

        disp<-(v[[i]]-VE)/num
      }

      cat("dispersion",i,": \n" )
      print(disp)
      cat("\n")
    }
  }

  else{ ###?ݺ??? ??��x
    for(i in ran.fac){
      inter<-i[grep(":", i)]

      if(length(inter)==0){###??ȿ??
        v[[i]]<-anv[i,"Mean Sq"]

        lv<-levels(data[[i]])

        num<-NULL
        disp<-NULL
        for(j in lv){
          num[j]<-length(which(data[[i]]==j))
          disp[j]<-(v[[i]]-VE)/num[j]
        }

        cat("dispersion",i,": \n" )
        print(disp)
        cat("\n")
      }
    }
  }
}
