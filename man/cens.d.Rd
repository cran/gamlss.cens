\name{cens.d}
\alias{cens.d}

\title{Censored probability density function of a GAMLSS family distribution}
\description{
 Creates a probability density function from a current GAMLSS family distribution for censored data
}
\usage{
cens.d(family = "NO", type = c("right", "left", "interval"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{ a \code{\link[gamlss]{gamlss.family}} object, which is used to 
          define the distribution and the link functions of the various parameters. 
          The distribution families supported by \code{gamlss()} 
        can be found in \code{\link[gamlss]{gamlss.family}} and in the package \code{gamlss.dist}.}
  \item{type}{ whether \code{right}, \code{left}  or in \code{interval} censoring is required, (right is the default) }
  \item{\dots}{for extra arguments }
}

\details{
This function is used to calculate the likelihood function for censored data. 
This function is not supposed to be used on it own but is used in \code{gen.cens}. 
}
\value{
Returns a modified d family function.  
The argument of the original function d function are the same.  
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}). 
 }
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk}}
\note{For an example see \code{gen.cens()}   
}
\seealso{  \code{\link{cens.p}},  \code{\link{gen.cens}}}
\examples{#see gen.cens for an example}

\keyword{distribution}
\keyword{regression}
