\name{is_abs_unit}
\alias{is_abs_unit}
\title{
Test Whether it is an Absolute Unit
}
\description{
Test Whether it is an Absolute Unit
}
\usage{
is_abs_unit(u)
}
\arguments{

  \item{u}{A \code{\link[grid]{unit}} object.}

}
\details{
Besides the normal absolute units (e.g. "mm", "inches"), this function
simply assumes \code{\link[grid:grid.grob]{grob}} objects as absolute units.

For a complex unit which is combination of different units, it is absolute
only if all units included are absolute units.
}
\value{
A logical value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
is_abs_unit(unit(1, "mm"))
is_abs_unit(unit(1, "npc"))
is_abs_unit(grobWidth(textGrob("foo")))
is_abs_unit(unit(1, "mm") + unit(1, "npc"))
}
