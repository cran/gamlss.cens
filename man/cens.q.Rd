\name{cens.q}
\alias{cens.q}
\title{Inverse cumulative probability density function to help with censored response}
\description{
Creates the inverse cumulative density function from a current GAMLSS family distribution suitable for censored data.
This is a dummy function identical to the unsensored one needed for centile estimation from censored data. 
}
\usage{
cens.q(family = "NO", ...)
}

\arguments{
  \item{family}{ a \code{\link[gamlss]{gamlss.family}} object, which is used to 
          define the distribution and the link functions of the various parameters. 
          The distribution families supported by \code{gamlss()} can be found in 
          \code{\link[gamlss]{gamlss.family}}.}
  \item{\dots}{for extra arguments }
}
\details{
 This is dummy function only used to calculate centiles from censored response variable.
 This function is not supposed to be used on it own but is used in \code{gen.cens}.
 }
\value{
 Returns a modified q family function.  
The argument of the original function q function are the same.  
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 
 }
\author{ Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk} }
\note{For an example see \code{gen.cens()}}
\seealso{ \code{\link{cens.d}}, \code{\link{cens.p}}, \code{\link{gen.cens}} }
\examples{#see gen.cens for an example}

\keyword{distribution}
\keyword{regression}
