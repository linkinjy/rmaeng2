% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanest.R
\name{mean.est}
\alias{mean.est}
\title{mean.est}
\usage{
\method{mean}{est}(formula, data, factor, ranfac = NULL, alpha = 0.05)
}
\arguments{
\item{formula, data, factor, ranfac, alpha}{}
}
\value{
population mean, graph
}
\description{
population mean of main effect
}
\examples{
a<-c(1,1,2,2,3,3)
a<-as.factor(c(rep(a,6)))
b<-as.factor(c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)))
y<-c(305,302,322,325,320,322,335,337,350,348,342,344,366,364,326,324,338,336,372,374,330,330,348,348,376,373,327,330,350,350,348,350,310,308,330,328)
z<-data.frame(a,b,y)

}
