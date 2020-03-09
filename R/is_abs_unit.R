
# is_abs_unit = function(x) UseMethod("is_abs_unit")

abs_units = c("cm", "inches", "mm", "points", "picas", "bigpts", "dida", "cicero",
		               "scaledpts", "lines", "char", "strwidth", "strheight", "grobwidth",
		               "grobheight", "strascent", "strdescent", "mylines", "mychar", 
		               "mystrwidth", "mystrheight", "centimetre", "centimetres", "centimeter",
		               "centimeters", "in", "inch", "line", "millimetre", "millimetres",
		               "millimeter", "millimeters", "point", "pt")

# grid::absolute.size() treats grobwidth and grobheight as non-absolute units,
# thus, I write another function to test
.is_abs_unit.unit = function(x) {

	unit = unitType(x)

	if(all(unit %in% abs_units)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

.is_abs_unit.unit.list = function(x) {
	all(sapply(x, function(y) {
		if(inherits(y, "unit.arithmetic")) .is_abs_unit.unit.arithmetic(y)
		else if(inherits(y, "unit.list")) .is_abs_unit.unit.list(y)
		else if(inherits(y, "unit")) .is_abs_unit.unit(y)
		else FALSE
	}))
}

.is_abs_unit.unit.arithmetic = function(x) {
	all(sapply(x, function(y) {
		if(inherits(y, "unit.arithmetic")) .is_abs_unit.unit.arithmetic(y)
		else if(inherits(y, "unit.list")) .is_abs_unit.unit.list(y)
		else if(inherits(y, "unit")) .is_abs_unit.unit(y)
		else TRUE
	}))
}

.is_abs_unit.default = function(x) {
	FALSE
}

# == title
# Test Whether it is an Absolute Unit
#
# == param
# -u A `grid::unit` object.
#
# == details
# Besides the normal absolute units (e.g. "mm", "inches"), this function
# simply assumes `grid::grob` objects as absolute units.
#
# For a complex unit which is combination of different units, it is absolute
# only if all units included are absolute units.
#
# == value
# A logical value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# is_abs_unit(unit(1, "mm"))
# is_abs_unit(unit(1, "npc"))
# is_abs_unit(grobWidth(textGrob("foo")))
# is_abs_unit(unit(1, "mm") + unit(1, "npc"))
#
is_abs_unit = function(u) {
	NULL
}

is_abs_unit_v3 = function(u) {
	if(inherits(u, "unit.arithmetic")) .is_abs_unit.unit.arithmetic(u)
	else if(inherits(u, "unit.list")) .is_abs_unit.unit.list(u)
	else if(inherits(u, "unit")) .is_abs_unit.unit(u)
	else FALSE
}

is_abs_unit_v4 = function(u) {
	u = unitType(u, recurse = TRUE)
	u = unlist(u)
	all(u %in% abs_units)
}

rv = R.Version()
if(getRversion() >= "4.0.0" && as.numeric(rv$`svn rev`) >= 77889) {
	is_abs_unit = is_abs_unit_v4
} else {
	is_abs_unit = is_abs_unit_v3
}
