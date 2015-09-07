\name{ColorMapping-class}
\docType{class}
\alias{ColorMapping-class}
\title{
Class to map values to colors
}
\description{
Class to map values to colors
}
\details{
The \code{\link{ColorMapping-class}} handles color mapping with both discrete values and continuous values.
Discrete values are mapped by setting a vector of colors and continuous values are mapped by setting
a color mapping function.
}
\section{Methods}{
The \code{\link{ColorMapping-class}} provides following methods:

\itemize{
  \item \code{\link{ColorMapping}}: contructor methods.
  \item \code{\link{map_to_colors,ColorMapping-method}}: mapping values to colors.
  \item \code{\link{color_mapping_legend,ColorMapping-method}}: draw legend or get legend as a \code{\link[grid]{grob}} object.
}}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# for examples, please go to `ColorMapping` method page
NULL

}
