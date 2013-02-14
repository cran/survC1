\name{Inf.Cval}
\alias{Inf.Cval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inference of C
}
\description{
Performes the inference for C
}
\usage{
Inf.Cval(mydata, tau, itr = 1000, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Input data. The 1st column should be time-to-event, and the 2nd column is event indicator (1=event, 0=censor). The rest of the columns are covariates/predictors used in the model. No character variable or missing is allowed. 
}
  \item{tau}{Truncation time. The resulting C tells how well the given prediction model works in predicting events that occur in the time range from 0 to \code{tau}. Note that the survival function for the underlying censoring time distribution needs to be positive at \code{tau}. 
}
  \item{itr}{Iteration of perturbation-resampling. 
}
  \item{seed}{A seed for generating random numbers for the resampling. The default is NULL.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Dhat}{Estimate for C}
\item{se}{Standard error estiamte}
\item{low95}{Lower limit of a 0.95 two-sided confidence interval for C}
\item{upp95}{Upper limit of a 0.95 two-sided confidence interval for C}
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

C=Inf.Cval(D, tau, itr=200)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=3)

}
