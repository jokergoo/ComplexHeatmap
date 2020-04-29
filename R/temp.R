if(getRversion() >= "4.0.0") {
	sum.unit = function(..., na.rm = FALSE) {
		lt = list(...)
		u = NULL
		for(i in seq_along(lt)) {
			if(length(lt[[i]]) > 1) {
				for(k in seq_along(lt[[i]])) {
					if(is.null(u)) {
						u = lt[[i]][k]
					} else {
						u = u + lt[[i]][k]
					}
				}
			} else {
				if(is.null(u)) {
					u = lt[[i]]
				} else {
					u = u + lt[[i]]
				}
			}
		}
		return(u)
	}
}
