\name{lip}
\alias{lip}
\docType{data}
\title{Date for lip}
\description{
  The data set used in this section was given by Dr Peggy Braun (University of Leipzig) 
  passed on by the project co-ordinator Dr Jane Sutherland of London Metropolitan University. 

  It consists of experimental results concerned with enzymology from a research project which 
  attempted to develop a generic food spoilage model.
  
  The data set contains a column called NAMES, which shows the experiment name, three columns 
  with values of the environmental conditions: temperature (Tem), pH and water activity (aw), 
  and the rest of the columns containing the activity of the cocktails, observed at certain days. 

  The researchers recorded the activity of proteases and lipases in each cocktail and were 
  interested in predicting the time when the activity started given some environmental conditions. 
  The activity is a positive integer and enzymes are considered inactive when activity=0. 

}
\usage{data(lip)}
\format{
  A data frame with 120 observations on the following 14 variables.
  \describe{
    \item{\code{name}}{a factor with levels the different experiment} 
    \item{\code{Tem}}{a numeric vector showing the temperature }
    \item{\code{pH}}{a numeric vector PH}
    \item{\code{aw}}{a numeric vector water activity}
    \item{\code{X0.d}}{a numeric vector if enzyme reacted at days 0-1}
    \item{\code{X1.d}}{a numeric vector}
    \item{\code{X2.d}}{a numeric vector}
    \item{\code{X4.d}}{a numeric vector}
    \item{\code{X11.d}}{a numeric vector}
    \item{\code{X18d.}}{a numeric vector}
    \item{\code{X25.d}}{a numeric vector}
    \item{\code{X32.d}}{a numeric vector}
    \item{\code{X39.d}}{a numeric vector}
    \item{\code{y}}{a matrix with 3 columns: this is a \code{Surv()} 
    object indicating the start the finish and censored  indicator as defined in 
    function \code{Surv()}}
  }
}
\details{

}
\source{
Prof Jane Sutherland 
}
\references{
}
\examples{
data(lip)
## maybe str(lip) ; plot(lip) ...
}
\keyword{datasets}
