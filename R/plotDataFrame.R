
# == title
# Visualize a data frame
#
# == param
# -df a data frame
# -overlap how to group numeric columns. If the overlapping rate between the ranges in the
#          current column and previous numeric column is larger than this value, the two columns
#          are thought under same measurement and should be grouped.
# -nlevel If the number of levels of a character column is larger than this value, the column will
#         be excluded.
# -show_row_names whether show row names after the last heatmap if there are row names.
# -group a list of index that defines the groupping
# -group_name names for each group
# -cluster_rows whether perform clustering on rows of the first heatmap
# -cluster_columns whether perform clustering on columns for all heatmaps
#
# == details
# The data frame contains information from different aspects and different measurements are applied
# on the same element for each row. The `plotDataFrame` function provides a simple and quick way to
# visualize information that are stored in a data frame.
#
# There are not many advanced settings in this function. Users can customize the style of the heatmaps
# by constructing a `HeatmapList` object.
# 
# == value
# a `HeatmapList` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
plotDataFrame = function(df, overlap = 0.5, nlevel = 30, show_row_names = TRUE,
	group = NULL, group_name = names(group), cluster_rows = TRUE, 
	cluster_columns = TRUE) {

	if(is.matrix(df)) {
		ht_list = Heatmap(df, show_row_names = show_row_names)
	} else if(is.data.frame(df)) {
		
		nc = ncol(df)
		cn = colnames(df)
		ht = NULL
		current_range = NULL
		current_group = 0

		if(is.null(group)) {
			group = list()

			for(i in seq_len(nc)) {
				if(is.numeric(df[[i]])) {
					if(is.null(current_range)) {
						# if previous column are character/factor
						current_range = quantile(df[[i]], c(0.1, 0.9))
						current_group = current_group + 1
						group[[ current_group ]] = i

					} else {
						# if previous columns are numeric
						range2 = range(df[[i]], c(0.1, 0.9))
						intersected_range = c(max(current_range[1], range2[1]), min(current_range[2], range2[2]))

						l = df[[i]] >= intersected_range[1] & df[[i]] <= intersected_range[2]
						l2 = df[[i-1]] >= intersected_range[1] & df[[i-1]] <= intersected_range[2]
						if(sum(l)/length(l) > overlap && sum(l2)/length(l2) > overlap) {
							group[[ current_group ]] = c(group[[ current_group ]], i)

						} else {
							# current column is not under same measurement as previous columns
							current_range = range2
							current_group = current_group + 1
							group[[ current_group ]] = i
						}
					}

				} else {
					current_range = NULL
					if(length(unique(df[[i]])) < nlevel) {
						current_group = current_group + 1
						group[[ current_group ]] = i
					}
				}
			}
		}

		if(is.null(group_name)) {
			for(i in seq_along(group)) {
				if(length(group[[i]]) > 1) {
					group_name[i] = paste0("matrix_", i)
				} else if(length(group[[i]]) == 1) {
					group_name[i] = cn[ group[[i]] ]
				}
			}
		}

		i_max = max(unlist(group))
		for(i in seq_along(group)) {
			ci = group[[i]]
			if(i == 1) {
				if(i == i_max) {
					ht_list = Heatmap(df[, ci, drop = FALSE], name = group_name[i], cluster_rows = cluster_rows, cluster_columns = cluster_columns, show_row_names = show_row_names)
				} else {
					ht_list = Heatmap(df[, ci, drop = FALSE], name = group_name[i], cluster_rows = cluster_rows, cluster_columns = cluster_columns, show_row_names = FALSE)
				}
			} else {
				if(i == i_max) {
					ht_list = ht_list + Heatmap(df[, ci, drop = FALSE], name = group_name[i], cluster_rows = cluster_rows, cluster_columns = cluster_columns, show_row_names = show_row_names)	
				} else {
					ht_list = ht_list + Heatmap(df[, ci, drop = FALSE], name = group_name[i], cluster_rows = cluster_rows, cluster_columns = cluster_columns, show_row_names = FALSE)
				}
			}
		}

	} else {
		stop("`table` can only be a matrix or a data frame.")
	}

	return(ht_list)
}
