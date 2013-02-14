\name{Inf.Cval.Delta}
\alias{Inf.Cval.Delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inference of Delta C
}
\description{
Performes the inference for the difference in C between two competing risk prediction models}
\usage{
Inf.Cval.Delta(mydata, covs0, covs1, tau, itr = 1000, seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Input data. The 1st column should be time-to-event, and the 2nd column is event indicator (1=event, 0=censor). 
}
  \item{covs0}{A matrix that consists of a set of predictors for a base model (Model 0)
}
  \item{covs1}{A matrix that consists of a set of predictors for a new model (Model 1)
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
A 4x3 matrix that consists of point estimates, standard error estiamtes, and lower and upper limits of 0.95 two-sided intervals for C of Model 1, Model 0 and that for the difference of C}
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


model0<-D[,c(1:2,4:5)] ; 
model1<-D
covs1<-as.matrix(model1[,c(-1,-2)])
covs0<-as.matrix(model0[,c(-1,-2)])

tau=365.25*8

Delta=Inf.Cval.Delta(model0[,1:2], covs0, covs1, tau, itr=200)
round(Delta, digits=3)
}
