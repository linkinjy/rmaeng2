% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monova3.R
\name{monova3}
\alias{monova3}
\title{monova3}
\usage{
monova3(formula, data, alpha, k)
}
\arguments{
\item{formula, data, alpha, k}{}
}
\value{
ci plot
}
\description{
two way anova (CI plot / no repetition / fixed model)
}
\examples{
a<-as.factor(c(rep(1,3),rep(2,3),rep(3,3),rep(4,3)))
b<-as.factor(rep(c(1,2,3),4))
y<-c(97.6,97.3,96.7,98.6,98.2,96.9,99,98,97.9,98,97.7,96.5)
z<-data.frame(a,b,y)

}
