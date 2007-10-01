\name{gen.cens}
\alias{gen.cens}

\title{Generates functions to help the fitted of censored data within GAMLSS}
\description{
The \code{gen.cens()} function allows the user to generate a
\code{d}, a \code{p}, and the fitting 
  \code{gamlss} fitting functions of a truncated  distribution given that the input \code{family} is a current GAMLSS family distribution. 
}
\usage{
gen.cens(family = "NO", name = "cens", 
          type = c("right", "left", "interval"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{a \code{\link[gamlss]{gamlss.family}} object, which is 
        used to define the distribution and the link functions of the various parameters.
        The distribution families supported by \code{gamlss()} 
        can be found in \code{\link[gamlss]{gamlss.family}} and in the package \code{gamlss.dist}.}
  \item{name}{the characters you want to add to the name of new functions, by default is \code{cens}}
   \item{type}{ whether \code{right}, \code{left}  or in \code{interval} censoring is required, 
              (right is the default) }
  \item{\dots}{for extra arguments}
}
\details{}
\value{
  Returns  the \code{d}, the \code{p}, and the list, i.e. Norc, used in the fitting gamlss algorithm. 
}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).
 }
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk}}
\note{}
\seealso{ \code{\link{cens.d}}, \code{\link{cens.p}}, \code{\link{cens}}  }
\examples{

}
\keyword{distribution}
\keyword{regression}

