\name{cvC}
\alias{cvC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross-validation estimate for C}
\description{
Provides a cross-validation estimate for C}
\usage{
cvC(mydata, tau, cvK = 10, Rep = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Input data. The 1st column should be time-to-event, and the 2nd column is event indicator (1=event, 0=censor). The rest of the columns are covariates/predictors used in the model. No character variable or missing is allowed. 
}
  \item{tau}{Truncation time. The resulting C tells how well the given prediction model works in predicting events that occur in the time range from 0 to \code{tau}. Note that the survival function for the underlying censoring time distribution needs to be positive at \code{tau}. 
}
  \item{cvK}{
The number of fold. A K-fold cross-validation is performed, according to the number given for \code{cvK}. Generally, a value from 2 to 10 will be specified.
}
  \item{Rep}{
The number of iteration of the random splits for the K-fold cross-validation. The average of the \code{Rep} times of \code{cvK}-fold cross-validation estimates is provided as a resulting estimate for C.
}
}
\examples{

D=CompCase(pbc[,c(2:4,10:14)]) 
D[,2]=as.numeric(D[,2]==2)

tau=365.25*8

model1=D[1:200,c(1,2,4)]

cvC(model1,tau,cvK=2,Rep=10)

}