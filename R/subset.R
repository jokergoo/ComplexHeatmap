
# "[.Heatmap" = function(x, i, j, drop = FALSE) {

# 	if(!missing(i) && !missing(j)) {
# 		return(x[i, j, drop = FALSE])
# 	}
# 	if(nargs() == 2) {
# 		return(x[i, ])
# 	}
# 	if(nargs() == 3 && missing(i)) {
# 		return(x[, j])
# 	}
# 	if(missing(i)) {
# 		return(x[i, j, drop = drop])
# 	}
# 	x = x[i, , drop = FALSE]

# }