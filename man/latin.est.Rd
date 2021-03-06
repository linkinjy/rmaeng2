% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latinest.R
\name{latin.est}
\alias{latin.est}
\title{latin.est}
\usage{
latin.est(formula, data, alpha, latin = TRUE, ro, co)
}
\arguments{
\item{formula, data, alpha, latin, ro, co}{}
}
\value{
lower,maen,upper,plot
}
\description{
population mean estimation (latin,graeco)
}
\examples{
a <- as.factor(c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
b <- as.factor(rep(c(1,2,3,4,5),5))
c <- as.factor(c("C1","C2","C3","C4","C5","C2","C3","C4","C5","C1","C3","C4","C5","C1","C2","C4","C5","C1","C2","C3", "C5","C1","C2","C3","C4"))
y <- c(68,64,71,71,72,74,70,80,74,80,63,65,70,69,68,64,58,69,66,65,70,72,76,70,78)
z<-data.frame(a,b,c,y)

}
