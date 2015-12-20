\name{is_abs_unit}
\alias{is_abs_unit}
\title{
Whether the unit object contains absolute unit
}
\description{
Whether the unit object contains absolute unit
}
\usage{
is_abs_unit(u)
}
\arguments{

  \item{u}{a \code{\link[grid]{unit}} object}

}
\details{
Besides the normal absolute units (e.g. "mm", "inches"), this function
simply treat \code{\link[grid]{grob}} objects as absolute units.

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
is_abs_unit(textGrob("foo"))
is_abs_unit(unit(1, "mm") + unit(1, "npc"))
}
