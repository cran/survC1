\name{kmcens}
\alias{kmcens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kaplan-Meier for Censoring
%%  ~~function to do ... ~~
}
\description{
This is a child function called in the main functions in this package. Specifically, this obtains the survival function for censroing time and gives quantities used in the perturbation-resampling method for C.
}
\usage{
kmcens(time, status, tau)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time}{time to event variable
}
  \item{status}{indicator of event (1:event, 0: censor)
}
  \item{tau}{see \code{cvK}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
