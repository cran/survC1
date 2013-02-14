\name{survC1-package}
\alias{survC1-package}
\alias{survC1}
\docType{package}
\title{C-statistics for risk prediction models with censored survival data}
\description{
Performes inference of overall adequecy of risk prediction models with censored survival data. 
}
\details{
\tabular{ll}{
Package: \tab survC1\cr
Type: \tab Package\cr
Version: \tab 1.0-2\cr
Date: \tab 2012-02-14\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Performs inference for C of risk prediction models with censored survival data, using the method proposed by Uno et al. (2011). Inference for the difference in C between two competing prediction models is also implemented.

}
\author{Hajime Uno

Maintainer: Hajime Uno <huno@jimmy.harvard.edu>
}
\references{
Hajime Uno, Tianxi Cai, Michael J. Pencina, Ralph B. D'Agostino, and LJ Wei. 
On the C-statistics for evaluating overall adequacy of risk prediction procedures
with censored survival data. Statistics in Medicine 2011, 30:1105-16. 
}
\keyword{ package }
\examples{

#==============================================
# read sample data (PBC in survival package)
#==============================================
D=CompCase(pbc[1:200,c(2:4,10:14)]) 
D[,2]=as.numeric(D[,2]==2)

#==============================================
# Inference of C
#==============================================
tau=365.25*8
C=Inf.Cval(D, tau, itr=200)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=3)

#==============================================
# Inference of Delta C between 2 models
#==============================================
model0<-D[,c(1:2,4:5)] ; 
model1<-D
covs1<-as.matrix(model1[,c(-1,-2)])
covs0<-as.matrix(model0[,c(-1,-2)])

Delta=Inf.Cval.Delta(model0[,1:2], covs0, covs1, tau, itr=200)
round(Delta, digits=3)

#==============================================
# Point estimation via cross-validation
#==============================================
model1=D[,c(1,2,4)]

cvC(model1,tau,cvK=2,Rep=10)
    
}
