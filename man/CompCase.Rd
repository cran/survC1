\name{CompCase}
\alias{CompCase}
\title{Creates complete case data}
\description{
Creates a complete-case data set, removing subjects with any missing value in the outcome or predictors}
\usage{CompCase(mydata)}
\arguments{
  \item{mydata}{
   A data matrix that consists of numeric data. No character data is allowed.
   }
}
\examples{
D=CompCase(pbc[,c(2:4,10:14)])
}
