
# is_abs_unit = function(x) UseMethod("is_abs_unit")

.is_abs_unit.unit = function(x) {
	unit = attr(x, "valid.unit")
	if(all(unit %in% c(1:4, 7:24))) {
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
# Whether the unit object contains absolute unit
#
# == param
# -u a `grid::unit` object
#
# == details
# Besides the normal absolute units (e.g. "mm", "inches"), this function
# simply treat `grid::grob` objects as absolute units.
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
# is_abs_unit(textGrob("foo"))
# is_abs_unit(unit(1, "mm") + unit(1, "npc"))
#
is_abs_unit = function(u) {
	if(inherits(u, "unit.arithmetic")) .is_abs_unit.unit.arithmetic(u)
	else if(inherits(u, "unit.list")) .is_abs_unit.unit.list(u)
	else if(inherits(u, "unit")) .is_abs_unit.unit(u)
	else FALSE
}
