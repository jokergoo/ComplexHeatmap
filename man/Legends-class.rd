\name{Legends-class}
\docType{class}
\alias{Legends-class}
\title{
The Class for Legends
}
\description{
The Class for Legends
}
\details{
This is a very simple class for legends that it only has one slot which is the real \code{\link[grid]{grob}} of the legends.
The design of the \code{Legends} class is to support more helper functions such as \code{\link{width.Legends}} to get 
more information of the legends grob.
}
\examples{
lgd = Legend(at = 1:4)
lgd
lgd@grob
}
