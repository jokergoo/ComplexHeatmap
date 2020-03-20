\name{smartAlign2}
\alias{smartAlign2}
\title{
Adjust positions of rectanglar shapes
}
\description{
Adjust positions of rectanglar shapes
}
\usage{
smartAlign2(start, end, range, plot = FALSE)
}
\arguments{

  \item{start}{position which corresponds to the start (bottom or left) of the rectangle-shapes.}
  \item{end}{position which corresponds to the end (top or right) of the rectanglar shapes.}
  \item{range}{data ranges (the minimal and maximal values)}
  \item{plot}{Whether plot the correspondance between the original positions and the adjusted positions. Only for testing.}

}
\details{
This is an improved version of the \code{\link[circlize]{smartAlign}}.

It adjusts the positions of the rectangular shapes to make them do not overlap
}
\examples{
range = c(0, 10)
pos1 = rbind(c(1, 2), c(5, 7))
smartAlign2(pos1, range = range, plot = TRUE)

range = c(0, 10)
pos1 = rbind(c(-0.5, 2), c(5, 7))
smartAlign2(pos1, range = range, plot = TRUE)

pos1 = rbind(c(-1, 2), c(3, 4), c(5, 6), c(7, 11))
pos1 = pos1 + runif(length(pos1), max = 0.3, min = -0.3)
omfrow = par("mfrow")
par(mfrow = c(3, 3))
for(i in 1:9) {
    ind = sample(4, 4)
    smartAlign2(pos1[ind, ], range = range, plot = TRUE)
}
par(mfrow = omfrow)

pos1 = rbind(c(3, 6), c(4, 7))
smartAlign2(pos1, range = range, plot = TRUE)

pos1 = rbind(c(1, 8), c(3, 10))
smartAlign2(pos1, range = range, plot = TRUE)
}
