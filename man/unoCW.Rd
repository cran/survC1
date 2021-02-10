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