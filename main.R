#########################
# FM Stock v1.0         #   
# Author: Hassan Jafri  #
#########################

# Load required packages
fm_load_packages <- function()
{
	source("fm_data_transform.R")
	source("fm_utils.R")
	library(car)
}

# Sanity check: names and order of columns
fm_parse_config <- function()
{
	c_file = scan("config.txt", what = "character", sep = "\n")
	
	i = 1
	
	while (i <= length(c_file)) {
			
		str = c_file[i]
		
		str_parts = strsplit(str, "[ ]+")[[1]]

		if ((str_parts[1] != "#") || (length(str_parts) == 1) ){
			i + 1
			next
		}
				
		if (length(str_parts) >= 3 &&
		    length(agrep(str_parts[2], "Data", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "Directory", value = TRUE)) > 0) {

			data_dir = c_file[i+1]
		
		  	i = i + 2
			next
		}
		
    	if (length(str_parts) >= 3 &&
		    length(agrep(str_parts[2], "Data", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "Files", value = TRUE)) > 0) {
		
			i = i + 1
			
			data_files = c()
			
			while (strsplit(c_file[i], "[ ]+")[[1]][1] == "#") {
				i = i + 1
			}
				
			while (strsplit(c_file[i], "[ ]+")[[1]][1] != "#") {
				data_files = append(data_files, c_file[i])
				i = i + 1
			}
			
			next
		}
		
		if (length(str_parts) >= 3 &&
			length(agrep(str_parts[2], "Results", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "Prefix", value = TRUE)) > 0) {
			
			result_file_prefix = c_file[i+1]
			i = i + 2
			next
		}
		
		if (length(str_parts) >= 3 &&
			length(agrep(str_parts[2], "Missing", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "Values", value = TRUE)) > 0) {
				
			mis_val_thresh = as.numeric(c_file[i+1])
			i = i + 2
			next
		}

		if (length(str_parts) >= 3 &&
			length(agrep(str_parts[2], "Outlier", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "threshold", value = TRUE)) > 0) {
				
			outlier_thresh = as.numeric(c_file[i+1])
			i = i + 2
			next
		}

		if (length(str_parts) >= 3 &&
			length(agrep(str_parts[2], "Box-Cox", value = TRUE)) > 0 &&
		    length(agrep(str_parts[3], "Transformation", value = TRUE)) > 0) {
				
			is_box_cox = as.numeric(c_file[i+1])
			i = i + 2
			next
		}
		
		i = i + 1
	}
	
	if (exists("data_dir")) {
		for (i in 1:length(data_files)) {
			data_files[i] = paste(data_dir, '/', data_files[i], sep = "")	
		}
	}
	
	ret = list()
	data_files_list = list()
	
	# Setup historical data files
	all_files = list.files(data_dir, "^[0-9]+.+[csvCSV]$")
	all_files = fm_sort_files_by_date(all_files, ".CSV$")
	all_files = paste(data_dir, '/', all_files, sep = "")
	
	for (i in 1:length(data_files)) {
		data_file = data_files[i]
		
		target_file_idx = which(all_files == data_file) 
		if (target_file_idx < 13) { stop("ERROR: 12 months data not available")}
		
		f = all_files[(target_file_idx - 12):target_file_idx]
		data_files_list[[i]] = f
	}

	ret$data_files_list = data_files_list
	
	if (exists("result_file_prefix"))
		ret$result_file_prefix = paste(data_dir, "/", result_file_prefix, sep ="")
	else
		ret$result_file_prefix = NA
	
	if (exists("mis_val_thresh")) 
		ret$mis_val_thresh = mis_val_thresh	
	else
		ret$mis_val_thresh = NA
	
	if (exists("outlier_thresh")) 
		ret$outlier_thresh = outlier_thresh
	else
		ret$outlier_thresh = NA
	
	if (exists("is_box_cox")) 
		ret$is_box_cox = as.logical(is_box_cox)
	else
		ret$is_box_cox = FALSE
	
	return(ret)
}

# Read data files
fm_read_data <- function(file_names)
{
	all_data = list()
	
	for (i in 1:length(file_names)) {
		f_name = file_names[i]
		data = read.csv(f_name, row.names = 1)
		cols = colnames(data)
		
		if ("company" %in% cols)
			data$company = NULL
		if ("ticker" %in% cols)
			data$ticker = NULL
		if ("return_quarter" %in% cols) {
			data$return_quarter = NULL
		}
		
		all_data[[i]] = data
	}
	
	all_data
}

fm_factor_rank <- function(models) 
{	
	for (i in 1:length(models)) {
		
		coef_table = summary(models[[i]])$coef
		t_vals = coef_table[, "t value"]
		
		sorted_idxs = order(t_vals, decreasing = TRUE)
		
		if (exists("sorted_idx_table")) 
			sorted_idx_table = cbind(sorted_idx_table, sorted_idxs)
		else
			sorted_idx_table = data.frame(sorted_idxs, row.names = rownames(coef_table)) 
	}
	
	print(sorted_idx_table)
}

# Create regression model for each data set
fm_model <- function(all_data)
{
	model_list = list()
	
	# Run regression on each data set
	for (i in 1:length(all_data)) {
		cur_data = all_data[[i]]
		
		n_cols = ncol(cur_data)
		
		# Assuming last column is the response. All others are predictors
		response_name = colnames(cur_data)[n_cols]
		predictor_names = colnames(cur_data)[1:(n_cols - 1)]
		
		# Setup the regression forumula
		predictor_form = paste(predictor_names, collapse = "+")
		reg_form = paste(response_name, "~", predictor_form)
		
		# Get a regression model
		model = lm(reg_form, cur_data, x = TRUE, y = TRUE)
					
		# Save regression model
		model_list[[i]] = model
	}
	
	model_list
}

# Computes the coefficients that are arithmatic mean of coefficients of 
# all models passed
fm_nmonth_coefficients <- function(all_models)
{
	nmonth_coefs = all_models[[1]]$coefficients
	na_idxs = which(is.na(nmonth_coefs))
	nmonth_coefs[na_idxs] = 0

	n_models = length(all_models)
	
	if (n_models == 1)
		return(nmonth_coefs)
	
	for (i in 2:n_models) {
		
		# Extract a model
		mdl = all_models[[i]]
		
		for (j in 1:length(mdl$coefficients)) {
			if (is.na(mdl$coefficients[j]))
				val = 0
			else
				val = mdl$coefficients[j]
			
			nmonth_coefs[j] = nmonth_coefs[j] + val
		}
	}
	
	nmonth_coefs = nmonth_coefs/n_models
	nmonth_coefs
}

# Separate training data from test
fm_split_data <- function(all_data)
{
	ret = list()
	
	n_months = length(all_data)
	if (n_months == 1) {
		stop("ERROR: More than one month data available. Aborting..")
	}

	ret$historical_data = all_data[1:(n_months - 1)]
	ret$current_data = all_data[[n_months]]
	
	ret
}

# Write table of stock rankings
fm_output_ranking <- function(current_data, result_file)
{
	n_col = ncol(current_data)
	
	ordered_idxs = order(current_data[, n_col], decreasing = TRUE)
	
	predicted_returns = current_data[ordered_idxs, n_col]
	
	tickers = rownames(current_data)[ordered_idxs]
	
	result = data.frame(tickers, predicted_returns, row.names = 1)

	out_file = result_file   
	write.csv(result, out_file)
	result
}

# Predict using factor model
fm_predict <- function(data, coefs)
{
	# Split predictors from response. Assuming last column contains the reponse
	n_col = ncol(data)
	predictors = data[, 1:(n_col- 1)]
	
	# Append one to predictor matrix. 
	ones = as.matrix(rep(1, nrow(predictors)))
	colnames(ones) = "ones"
	predictors = as.matrix(cbind(ones, predictors))
	coefs = as.matrix(coefs)
	exp_rets = predictors %*% coefs
	
	result = data.frame(exp_rets, row.names = rownames(data))
	colnames(result) = "exp ret"
	 
	result
}

# Calculate significance of payoffs
fm_comparison_pars <- function(real, expected, betas, predictors)
{
	size = length(real)
	sse = sum((real - expected)^2)	
	mse = sse/size
	se_betas = c()
	
	for (i in 1:ncol(predictors)) {
		x = predictors[, i]
		mean_x = mean(x)
		
		sse_x = sum((x - mean_x)^2)
		se_betas[i] = sqrt(mse/sse_x)
	}

	t_values = betas/se_betas	
	ordered_idxs = order(t_values, decreasing = TRUE)
	
	print("t-values of payoffs:")
	print(t_values[ordered_idxs])
}

# Compare expected returns with actual returns
fm_comparison <- function(data, exp_rets, nmonth_coefs, result_log_file)
{
	# First table has data factored into 10 levels, each level 
	# with same number of data points. 
	# Second table has regular deciles
	exp_rets_ewdec = cbind(exp_rets, cut(seq(1, nrow(exp_rets)), 10, labels = 10:1))
	exp_rets_dec =  cbind(exp_rets, cut(exp_rets[, 1], 10, labels = 10:1))
	
	exp_rets = exp_rets_ewdec
	
	colnames(exp_rets) = c("exp_ret", "decile")
	
	# Extract realized returns, order by order of expected returns	
	realized_rets = data[rownames(exp_rets), ncol(data)]
	
	if (any(is.na(realized_rets))) {
		return(NULL)
	}
	
	# t-values of payoff
	fm_comparison_pars(realized_rets, exp_rets[,1], nmonth_coefs[2:length(nmonth_coefs)], 
					   data[rownames(exp_rets), 1:(ncol(data) - 1)])

	# Setup table of realized returns and deciles from expected returns
	#dec_rr_table = as.data.frame(cbind(realized_rets, exp_rets[, ncol(exp_rets)]), row.names = rownames(exp_rets))
	dec_rr_table = data.frame(realized_rets, exp_rets[, ncol(exp_rets)], row.names = rownames(exp_rets))
	
	colnames(dec_rr_table) = c("real_ret", "decile")
	
	plot(dec_rr_table[,2], dec_rr_table[,1], type = 'p', main = "Realized Return vs. Expected Return Deciles")
		
	reg_mdl = lm(real_ret ~ decile, data = dec_rr_table)
	
	# Write regression result to file
	out_file = result_log_file
	
	cat("\nREGRESSION OF REALIZED RETURNS OVER DECILES RANKINGS\n", append = TRUE)
	print(summary(reg_mdl))
	
	cat("Top Decile Mean:", mean(dec_rr_table[which(dec_rr_table[,2] == 10), 1]), "\n", append = TRUE)
	cat("Bottom Decile Mean:", mean(dec_rr_table[which(dec_rr_table[,2] == 1), 1]), "\n", append = TRUE)
		
	reg_mdl
}

# Make necessary transformation to the data
fm_transform_data <- function(all_data, result_log_file, data_files, input_params)
{	
	all_data = fm_reorder_columns(all_data, result_log_file)
	all_data = fm_use_large_cap(all_data, result_log_file)
	all_data = fm_handle_missing(all_data, result_log_file, input_params$mis_val_thresh)	
	print("HERE 2")
	all_data = fm_transform_boxcox_siml(all_data, input_params$is_box_cox, result_log_file)	
	all_data = fm_remove_outliers(all_data, result_log_file, input_params$outlier_thresh)
	
	all_data
}

# Call this function to run factor model
fm_main <- function()
{
	# First load required packages
	fm_load_packages()
	
	# Turn off warnings
	options(warn = -1)
	
	# Parse the configuration file
	ret = fm_parse_config()
	data_files_list = ret$data_files_list
	result_file_prefix = ret$result_file_prefix
	mis_val_thresh = ret$mis_val_thresh
	print(data_files_list)
	
	for (data_files in data_files_list) {
		# Setup file names for results/logs
		time_stamp = gsub(":", "", Sys.time())
		time_stamp = gsub(" ", "_", time_stamp)
		time_stamp = gsub(".csv", "", data_files[length(data_files)])
		time_stamp = gsub("^.+\/", "", time_stamp)
		result_data_file = paste(result_file_prefix, "_", time_stamp, ".csv", sep = "")
		result_log_file = paste(result_file_prefix, "_log_", time_stamp, ".txt", sep = "")
	
		# Basic checks on input
		if (length(result_file_prefix) == 0) {
			stop("ERROR: No data files provided")
		}
	
		if (is.na(result_file_prefix)) {
			tmp_strs = strsplit(data_files[1], "/")
			result_file_prefix = tmp_strs[length(tmp_strs)]	
		}
	
		# Output from 'print' will now go to file
		#	sink(result_log_file, append = TRUE)
		
		cat("\nFM STOCK RUNTIME LOG\n")
		report_str = paste("\nInput data files:", data_files, collapse = ",")
		report_str = paste(report_str, "\nResult File:", result_data_file)
		report_str = paste(report_str, "\nMissing Values Threshold:", ret$mis_val_thresh)
		report_str = paste(report_str, "\nOutlier Threshold:", ret$outlier_thresh)
		report_str = paste(report_str, "\nBox-Cox Transformation:", ret$is_box_cox)
		cat(report_str, append = TRUE)
	
		# Read data from data directory
		all_data = fm_read_data(data_files)	
	
		# Will use only following tickers in the results
		tickers_to_use = fm_prune(all_data[[length(all_data)]])
		
		# Make necessary transformations
		all_data = fm_transform_data(all_data, result_log_file, data_files, ret)
		
		# Separate historical data tables from current one. We will predict the 
		# response variable in current variable
		ret_split = fm_split_data(all_data)
		historical_data = ret_split$historical_data
		current_data = ret_split$current_data
	
		# Run the regression model
		historical_models = fm_model(historical_data)
		
		# Display sorted factors
		#	fm_factor_rank(historical_models)
	
		# Get final coefficient of regression that will be used for prediction
		nmonth_coefs = fm_nmonth_coefficients(historical_models)
	
		# Make prediction	
		exp_rets = fm_predict(current_data, nmonth_coefs)
		
		# tickers_to_use might contain tickers not in current_data
		tickers_to_use = intersect(rownames(current_data), tickers_to_use)
		
		exp_rets = as.data.frame(exp_rets[tickers_to_use,], row.names = tickers_to_use)
		
		# Stocks ranked by expected returns
		exp_rets = fm_output_ranking(exp_rets, result_data_file)
		
		comp_res = fm_comparison(current_data[tickers_to_use,], exp_rets, nmonth_coefs, result_log_file)
		
		# Cancel redirection output to file
		#	sink(NULL)
	    
		print("Done. See results folder for output files")
	}
	
	comp_res
}

fm_data <- function()
{
	source("dm.R")
	fm_setup_data()	
}

fm_backtest <- function(type = "simple", period = "monthly")
{
	source("fm_utils.R")
	source("backtest.R")
	fm_bt(type, period)	
}