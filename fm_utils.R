# Sort file names
fm_sort_files_by_date <- function(files, separator = "[ ]+")
{
	dates = c()
	orig_dates = c()
	for (i in 1:length(files)) {
		f_name = files[i]
		f_date = strsplit(f_name, separator)[[1]][1]
		orig_dates = append(orig_dates, f_date)
		dates = append(dates, as.Date(f_date, format("%m_%d_%Y")))
	}
        	
	sorted_dates = sort(dates)

	d_idxs = c()
	for (date in dates) {
		d_idxs = append(d_idxs, which(sorted_dates == date))
	}
	
	orig_sorted = c()
	orig_sorted[d_idxs] = files	
	return(orig_sorted)
}

fm_utils_mkuniq <- function(table, colnm)
{
	blank_rows = which(table[, colnm] == "")
	dup_rows = which(duplicated(table[, colnm]))
	
	dup_rows = append(dup_rows, blank_rows)
	dup_rows = unique(dup_rows)
	
	all_rows = seq(1, nrow(table))
	valid_rows = setdiff(all_rows, dup_rows)
	table[valid_rows,]
}

# Set given column (index) as last column
fm_utils_set_last_col <- function(table, src_col_idx)
{
	table = cbind(table, table[, src_col_idx])
	src_col_name = colnames(table)[src_col_idx]
	dst_col_idx = ncol(table)
	colnames(table)[dst_col_idx] = src_col_name
	table[, src_col_idx] = NULL
	
	table
}
