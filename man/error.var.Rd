% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errorvar.R
\name{error.var}
\alias{error.var}
\title{error.var}
\usage{
error.var(formula, data, alpha)
}
\arguments{
\item{formula, data, alpha}{}
}
\value{
error variance
}
\description{
error variance estimation of one way anova
}
\examples{
y <- c(8.44,8.36,8.28,8.59,8.91,8.6,9.34,9.41,9.69,8.92,8.92,8.74)
n <- rep(3, 4)
level <- as.factor(rep(1:4, n))
z<-data.frame(level,y)

}
