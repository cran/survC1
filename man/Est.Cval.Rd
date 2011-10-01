\name{Est.Cval}
\alias{Est.Cval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Esimtation of C}
\description{
Performs a point estimation of C
}
\usage{
Est.Cval(mydata, tau)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Input data. The 1st column should be time-to-event, and the 2nd column is event indicator (1=event, 0=censor). The rest of the columns are covariates/predictors used in the model. No character variable or missing is allowed. 
}
  \item{tau}{Truncation time. The resulting C tells how well the given prediction model works in predicting events that occur in the time range from 0 to \code{tau}. Note that the survival function for the underlying censoring time distribution needs to be positive at \code{tau}. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Dhat}{Estimate of C}
\item{beta}{Estimates for regression coefficient in the Cox model}
\item{beta.var}{Variance-Covariance matrix for \code{beta} above}
\item{rs}{Risk score of each individial}
\item{ft}{\code{coxph} object with the fitted model}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

D=CompCase(pbc[1:200,c(2:4,10:14)]) 
D[,2]=as.numeric(D[,2]==2)
tau=365.25*8
model=D[,c(1,2,4,5)]

Est.Cval(model, tau)$Dhat

}
