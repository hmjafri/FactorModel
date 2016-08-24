# 
fm_prune <- function(cur_data)
{
	# Take out stocks with market cap less than 300M
	idxs_1 = which(cur_data[, "mktcap"] > 300)
	idxs_2 = which(cur_data[, "price"] > 5)
	idxs = intersect(idxs_1, idxs_2)
	rownames(cur_data[idxs,])
}

# Use top 2000 large caps
fm_use_large_cap <- function(all_data, result_log_file = NULL)
{
	for (i in 1:length(all_data)) {
		data = all_data[[i]]
		
		n_tickers = nrow(data)
		
		order_mktcap = order(data[, "mktcap"], decreasing = TRUE)
		order_mktcap = head(order_mktcap, min(n_tickers, 2000))
		data = data[order_mktcap, ]
		
		all_data[[i]] = data
	}
	
	all_data
}

# Reorder columns if necessary
fm_reorder_columns <- function(all_data, result_log_file = NULL)
{
	ref_cols = colnames(all_data[[1]])	
	ref_cols = ref_cols[1:(length(ref_cols) - 1)]
	
	for (i in 2:length(all_data)) {
		cur_data = all_data[[i]]
		col_names = colnames(cur_data)[1:length(ref_cols)]
	
		diff_col = which(col_names != ref_cols)
		
		if (length(diff_col) > 0) {
			
			if (length(ref_cols) != length(col_names)) {
				cat("Number of cols: #1", length(ref_cols), ", #", i, length(col_names)) 
				stop("ERROR: Different number of columns. Exiting")
			}
			
			cat("WARNING: Different column names at ", i, "\n\t", 
			     head(col_names[diff_col]), "\n\t", 
				 head(ref_cols[diff_col]), "\n")
				 
			cur_data = cur_data[, c(ref_cols, "return")] 
			
			all_data[[i]] = cur_data
		}
	}
	
	all_data
}

# Remove outliers from data
fm_remove_outliers <- function(all_data, result_log_file = "", outlier_thresh)
{
	report_str = paste("\nOUTLIERS REMOVAL:", outlier_thresh, "S.D", "\n")
	cat(report_str, append = TRUE)
	
	for (i in 1:length(all_data)) {	
		data_table = all_data[[i]]
		outlier_idxs = c()

		report_str = paste(i, ".", "# of rows before removal", nrow(data_table), "\n")
		cat(report_str, append = TRUE)
		
		last_idx = which(colnames(data_table) == "volatility")
		
		for (j in 1:last_idx) {
			col_data = data_table[, j]
			col_mean = mean(col_data)
			col_sd = sd(col_data)
			
			upper_bound = col_mean + col_sd * outlier_thresh
			lower_bound = col_mean - col_sd * outlier_thresh
			
			outlier_idxs = append(outlier_idxs, which(col_data > upper_bound))
			outlier_idxs = append(outlier_idxs, which(col_data < lower_bound))
		}
		
		all_rows = seq(1:nrow(data_table))
		row_subset = setdiff(all_rows, outlier_idxs)
		all_data[[i]] = data_table[row_subset, ]
		
		report_str = paste(i, ".", "# of after before removal", nrow(all_data[[i]]), "\n")
		cat(report_str, append = TRUE)
	}

	all_data
}

# Ensure sames tickers are present in each month's data
fm_homogenize_tickers <- function(all_data, out_file = NULL) 
{
	n_months = length(all_data)
	excluded_tickers = c()
	
	current_month_tickers = rownames(all_data[[n_months]])
	
	original_length = length(current_month_tickers)
	
	for (i in 1:(n_months - 1)) {
		# Extract old month data table
		old_data = all_data[[i]]
		old_month_tickers = rownames(old_data)
		
		# Separate out additions/subtractions in tickers
		diff = setdiff(current_month_tickers, old_month_tickers)
		current_month_tickers = setdiff(current_month_tickers, diff)
	}
	
	for (i in 1:n_months) {
		all_data[[i]] = all_data[[i]][current_month_tickers, ]
	}
	
	new_length = length(current_month_tickers)
	
	all_data
}

# Handle missing values in data set
fm_handle_missing <- function(all_data, result_log_file = NULL, mis_val_thresh = NA)
{	
	# For each data set
	for (i in 1:length(all_data)) {
		
		data_set = all_data[[i]]	
		n_total_items = nrow(data_set)
		
		na_fractions = c()
		
		# for each column in the data set
		for (j in 1:ncol(data_set)) {
			# Cache these values
			cur_col = data_set[, j]
			cur_col_name = colnames(data_set)[j]			
			na_result = is.na(cur_col)
			
			if (all(na_result)) {
				data_set[, j] = rep(1, n_total_items)
			}
			# Replace NA with mean of the column
			else if (any(na_result)) {
				# Get mean of available values
				non_na_values = na.omit(cur_col)
				column_mean = mean(non_na_values)
				
				# Assign mean to unavailable values
				na_idxs = which(is.na(cur_col)) 
				cur_col[na_idxs] = column_mean
				
				# 
				n_na_items = length(na_idxs)
				na_fractions[j] = (n_na_items/n_total_items) * 100
				
				# Update the data set
				data_set[, j] = cur_col
			}
			else
				na_fractions[j] = NA
		}
			
		if (!exists("na_fractions_table")) {
			na_fractions_table = data.frame(rbind(na_fractions), row.names = i)
			colnames(na_fractions_table) = colnames(data_set)
		}
		else {
			print(rownames(na_fractions_table))
			na_fractions_table = rbind(na_fractions_table, na_fractions)
			rownames(na_fractions_table)[i] = i
		}
		
		# Update the data set list
		all_data[[i]] = data_set
		
		if (any(is.na(data_set))) {
			cat("Missing values not handled for idx", i, "\n")
			stop("ERROR: Not all missing values handled")	
		}
	}
	
	# Log factors with missing values over a percentage threshold
	mis_val_thresh_factors = c()
	na_thresh_table = na_fractions_table > mis_val_thresh
	
	for (i in 1:ncol(na_thresh_table)) {
		
		factor_mis_frac = na.omit(na_thresh_table[, i])
		
		if (length(factor_mis_frac > 0) && any(factor_mis_frac)) {
			mis_val_thresh_factors = append(mis_val_thresh_factors, i)	
		}
	}
	
	cat("\nMISSING VALUES STATISTICS\n", append = TRUE)
	
	if (length(mis_val_thresh_factors) > 0)
		print(na_fractions_table[,mis_val_thresh_factors])
	else
		print("No factor beyond threshold")
	
	all_data
}

# Apply box cox on all month simualtaneously
fm_transform_boxcox_siml <- function(all_data, flag, r_file)
{
	if (!flag)
		return(all_data)

	single_table = all_data[[1]]
	
	n_tickers_months = c(nrow(single_table))
	
	# Create a single table of data
	for (i in 2:length(all_data)) {
		colnames(all_data[[i]]) = colnames(single_table)
		single_table = rbind(single_table, all_data[[i]])
		n_tickers_months[i] = nrow(all_data[[i]])
	}
	
	cur_data = single_table
	
	# We do not want to transform the sector columns and return column
	n_cols = which(colnames(cur_data) == "volatility")
	
	for (j in 1:n_cols) {
		
		if (all(cur_data[,j] == 1))
			next
		
		# Make all values positive
		factor_min = abs(min(cur_data[, j]))
		
		if (factor_min > 0)
			new_factor = cur_data[, j] + factor_min * 1.1
		else 
			new_factor = cur_data[, j] + 1
			
		bct = box.cox.powers(new_factor)
		lambda = bct$lambda
	
		new_factor = box.cox(new_factor, lambda)

		# Now do box-cox transformation
		cur_data[, j] = new_factor
	}
	
	end = 0
	for (i in 1:length(all_data)) {
		start = end + 1
		end = start + n_tickers_months[i] - 1
		
		month_data = cur_data[start:end, ]
		rownames(month_data) = rownames(all_data[[i]])
		all_data[[i]] = month_data
	}
	
	all_data
}

# Apply box cox on each month separately
fm_transform_boxcox_sep <- function(all_data, flag, result_log_file)
{
	if (!flag)
		return(all_data)
	
	all_lambdas = matrix(nrow = length(all_data), ncol = ncol(all_data[[1]])) 
	
	col_names = colnames(all_data[[1]])
	
	# error object
	err = simpleError("Box-Cox Error")
	
	for (i in 1:length(all_data)) {
		cur_data = all_data[[i]]	
		n_cols = ncol(cur_data)
		
		# We do not want to transform the sector columns and return column
		n_cols = which(colnames(cur_data) == "volatility")
		
		for (j in 1:n_cols) {
			if (all(cur_data[,j] == 1))
				next
		
			# Make all values positive
			factor_min = abs(min(cur_data[, j]))
			
			if (factor_min > 0)
				new_factor = cur_data[, j] + factor_min * 1.1
			else 
				new_factor = cur_data[, j] + 1

			
			# Lets get box-cox lambda
			bct = tryCatch(box.cox.powers(new_factor), error = function(err) err)
			
			lambda = bct$lambda
			 
			# Null lambda mean box-cox could not be calculated
			if (is.null(lambda)) {
				cat("No box-cox for", colnames(cur_data)[j], "\n")
				next
			}
			
			#if (abs(lambda) < 2) {
			new_factor = box.cox(new_factor, lambda)
			#}
				
			all_lambdas[i, j] = lambda
			
			# Now do box-cox transformation
			cur_data[, j] = new_factor
		}
		
		all_data[[i]] = cur_data
	}
	
	cat("\nBOX-COX Transformation: Lambdas\n")
	print(all_lambdas)
	
	all_data
}
