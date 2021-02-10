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