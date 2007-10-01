\name{cens}
\alias{cens}

\title{Fits censored data using a GAMLSS family distribution}
\description{
 This function can be used to fit censored data. 
 It takes as an argument an existing GAMLSS family distribution  and  generates 
 a \code{\link[gamlss]{gamlss.family}} object which then can be used to fit 
 a right, left or interval censored data. 
}
\usage{
cens(family = "NO", name = "cens", type = c("right", "left", "interval"),
       local = TRUE, delta = NULL, ...)
}

\arguments{
   \item{family}{ a \code{\link[gamlss]{gamlss.family}} object, which is used to 
          define the distribution and the link functions of the various parameters. 
          The distribution families supported by \code{gamlss()} 
        can be found in \code{\link[gamlss]{gamlss.family}} and in the package \code{gamlss.dist}.}
  \item{name}{the characters you want to add to the name of new functions, by default is \code{cens}}
  \item{type}{what type of censoring is required, \code{right}, \code{left} or \code{interval}.}
  \item{local}{if TRUE the function will try to find the environment of \code{gamlss} to generate 
               the \code{d} and \code{p} functions required for the fitting, 
               if FALSE the functions will be generated in the global environment}
  \item{delta}{the delta increment used in the numerical derivatives} 
  \item{\dots}{for extra arguments }
}
\details{
This function is created to help the user to fit censored data using an existing \code{gamlss} distribution.
It does this by taking an existing \code{\link[gamlss]{gamlss.family}} and changing some of the components 
of the distribution to help the fitting process. 
It particular it i) creates a (\code{d}) function (for calculating the censored likelihood) and a (\code{p}) 
        function (for generating the quantile residuals) within \code{gamlss}, 
ii) changes  the global deviance function \code{G.dev.incr}, the first derivative funtions (see note below) 
   and other quantities from the original distribution.   
}
\value{
 It returns a \code{\link[gamlss]{gamlss.family}} object which has all the 
 components needed for fitting a distribution in \code{gamlss}.
}
\references{ Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2003) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).}

\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk}}

\note{
This function is experimental and could be changed. 
The function \code{cens} changes the first derivatives of the original gamlss family \code{d} 
function to numerical derivatives for the new censored \code{d} function. The default increment \code{delta},
for this numerical derivatives function, is \code{eps * pmax(abs(x), 1)} where 
\code{eps<-sqrt(.Machine$double.eps)}. The default \code{delta} could be inappropriate for 
specific applications and can be overwritten by using the argument \code{delta}.}
\seealso{\code{\link{cens.d}}, \code{\link{cens.p}},  \code{\link{gen.cens}} 
}
\examples{

}
\keyword{distribution}
\keyword{regression}

