
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

is_abs_unit = function(y) {
	if(inherits(y, "unit.arithmetic")) .is_abs_unit.unit.arithmetic(y)
	else if(inherits(y, "unit.list")) .is_abs_unit.unit.list(y)
	else if(inherits(y, "unit")) .is_abs_unit.unit(y)
	else FALSE
}
