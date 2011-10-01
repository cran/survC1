\name{unoCW}
\alias{unoCW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A child function used in Inf.Cval and Inf.Cval.Delta
}
\description{
This is a child function (R-wrapper) used in the main functions of this package. 
}
\usage{
unoCW(X, D, W, Wstar, RS, RSstar, REX, Chat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{time
}
  \item{D}{event indicator (1:event, 0:censor)
}
  \item{W}{Weight used in calculating C
}
  \item{Wstar}{Weight used in calculating C (pertubed version)
}
  \item{RS}{risk score
}
  \item{RSstar}{risk score (perturbed version)
}
  \item{REX}{Random numbers assinged to each individual for the perturbation-resmapling method
}
  \item{Chat}{C-statisitc estimated with the present dataset
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
