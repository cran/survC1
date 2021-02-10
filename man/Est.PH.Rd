\name{Est.PH}
\alias{Est.PH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Derivation of a risk score by a Cox proportioal hazarzs model
}
\description{
Provides risk score by fitting data to a Cox's proportional hazards model with a given set of predictors.}

\usage{
Est.PH(mydata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Input data. The 1st column should be time-to-event, and the 2nd column is event indicator (1=event, 0=censor). The rest of the columns are covariates/predictors used in the model. No character variable or missing is allowed. 
}
}
\value{
\item{beta}{Estimates for regression coefficient in the Cox model}
\item{var}{Variance-Covariance matrix for the beta above}
\item{rs}{Risk score of each individual}
\item{ft}{\code{coxph} object with the fitted model}
}
\examples{
D=CompCase(pbc[1:200,c(2:4,10:14)]) 
D[,2]=as.numeric(D[,2]==2)

ft=Est.PH(D)

}
