library("fImport")

# Sectors dummy variables
fm_setup_sectors <- function(all_data, all_files, write_folder)
{
        if ("img_desc" %in% colnames(all_data[[1]]))
          sector_col = "img_desc"
        else if ("ind_3_dig" %in% colnames(all_data[[1]]))
          sector_col = "ind_3_dig"
        else
          stop("ERROR: fm_setup_sectors: sector column not found")
        
	if (!(length(all_data) >= 1))
		stop("ERROR: fm_setup_sectors")
	
	# Populate list of unique factors
	unique_sectors = unique(all_data[[1]][, sector_col])
	unique_sectors = gsub("^[0-9]+ - ", "", unique_sectors)
	
	for (i in 1:length(all_data)) {
		data = all_data[[i]]
		num_rows = nrow(data)
	
		# Initialize sector table
		sector_table = data.frame(rep(0, num_rows))
	
		# Construct complete sector table
		for (j in 2:length(unique_sectors)) {
			col = rep(0, num_rows) 
			sector_table = cbind(sector_table, col)
		}
		
		colnames(sector_table) = unique_sectors
		rownames(sector_table) = rownames(data)
		
		# Attach dummy columns to the table
		data = cbind(data, sector_table)
	
		# Set sector dummy variables
		for (j in 1:nrow(data)) {
			stock_sector = as.character(data[j, sector_col])
			stock_sector = gsub("^[0-9]+ - ", "", stock_sector) 
			data[j, stock_sector] = 1
		}
	
		# Remove sector names column
		data[, sector_col] = NULL
		data$ticker = NULL
		
		# Returns column must be the last columns
		if ("return_quarter" %in% colnames(data))
                  data = fm_utils_set_last_col(data, which(colnames(data) == "return_quarter"))
                if ("return" %in% colnames(data))
		  data = fm_utils_set_last_col(data, which(colnames(data) == "return"))
		
		all_data[[i]] = data
		out_file_name = strsplit(all_files[i], "/")[[1]][2]		
                out_file_name = strsplit(out_file_name, "\\.")[[1]][1]
        	out_file_name = paste(write_folder, "/", out_file_name, ".csv", sep = "")

		write.csv(data, out_file_name)
		
		cat("out_file_name", "...Finished" , i, "files\n")
	}
	
	all_data
}

# Create table of 4 week returns
fm_get_4wk_rets <- function(all_data)
{
	rs4_table = data.frame(all_data[[1]]$rs_04w, row.names = rownames(all_data[[1]]))
	rs4_str = "rs_04w"
	
	if (length(all_data) < 2){
		return (rs4_table)
	}
	
	for (i in 2:length(all_data)) {
		data = all_data[[i]]
		rnames_cur = rownames(rs4_table)
		rnames_next = rownames(data)
		
		common = intersect(rnames_cur, rnames_next)
		diff_1 = setdiff(rnames_cur, rnames_next)
		diff_2 = setdiff(rnames_next, rnames_cur)
		
		tmp_common = cbind(as.data.frame(rs4_table[common, ], row.names = common), data[common, rs4_str])
		colnames(tmp_common) = seq(1, ncol(tmp_common))
		
		tmp_d1 = cbind(as.data.frame(rs4_table[diff_1, ], row.names = diff_1), rep(NA, length(diff_1)))
		names(tmp_d1) = names(tmp_common)
		
		rs4_table = rbind(tmp_common, tmp_d1)
		
		# tickers present in current month only
		tmp_d2 = matrix(nrow = length(diff_2), ncol = ncol(rs4_table))	
		tmp_d2[, ncol(tmp_d2)] = data[diff_2, rs4_str]
		tmp_d2 = as.data.frame(tmp_d2, row.names = diff_2)
		
		names(tmp_d2) = names(rs4_table)
		
		rs4_table = rbind(rs4_table, tmp_d2)
	}
	
	rs4_table
}

# Append variance of monthly returns to data
fm_append_var <- function(all_data, vol_months = 24)
{
	print("Appending Variance")
	vol_col_name = "volatility"

	# Ger table of 4 week returns
	rets_4_week = fm_get_4wk_rets(all_data)
    
	for (i in 1:length(all_data)) {
	
		data = all_data[[i]]
		
		if (is.null(data$volatility)) { 
		
			if (i < vol_months) {
				data = cbind(data, rep(1, nrow(data)))
			}
			
			else {
				e = i; s = i - vol_months + 1
				
				vols = as.data.frame(apply(rets_4_week[rownames(data), s:e], 1, sd))
			
				data = cbind(data, vols)
				
			}
			
			colnames(data)[ncol(data)] = vol_col_name
		}

		all_data[[i]] = data
	}
	
	all_data
}

# Calculate residuals returns and appends to data
fm_append_resret <- function(all_data, mrkt_rets)
{
	print("Appending Residual Returns")
	
	for (i in 1:length(all_data)) {
		data = all_data[[i]]
		
		betas = data[, "beta"]
		res_rets = c()
		exs_rets = data[, "rs_04w"]
		mrkt_ret = mrkt_rets[i, 1]
		
		for (j in 1:length(betas)) {
			beta = betas[j]
			exs_ret = exs_rets[j]
			
			if (is.na(beta) || is.na(exs_ret)) {
				res_rets[j] = NA
			}
			else {
				res_rets[j] = exs_ret - beta * mrkt_ret
			}
		}
		
		all_data[[i]] = cbind(data, res_rets)
	}
	
	all_data
}

# Acquire index time series
fm_index_data <- function(dates, ind_ticker = "^GSPC", ofile = "sp500.csv")
{
	trans_dates = c()
       	
	# First, appropriately setup dates of interest
	for (date in dates) {
		date = as.Date(date, format("%m_%d_%Y"))
		trans_dates = append(trans_dates, date)
	}
	
	# Put in asending order
	trans_dates = sort(trans_dates)
	
	# Need an extra month to calculare the return
	trans_dates = append(trans_dates[1] - 30, trans_dates)
	
	trans_dates = as.character(trans_dates)
	
	# Read index file if already exists
	if (file.exists(ofile)) {
		series = read.csv(ofile, row.names = 1)
	
		if (any(rownames(series) != trans_dates[2:length(trans_dates)]))
			stop("ERROR: Index file does not have required data")
	}
	
	# Otherwise, get from yahoo
	else {
		series = as.data.frame(yahooSeries(ind_ticker, from = trans_dates[1], to = trans_dates[length(trans_dates)]))
		series = series[trans_dates, ncol(series)]
	        cat ("fuck me")	
		# Calculate returns
		idxs = seq(2, length(series))
		series = 100 * (series[idxs] - series[idxs - 1])/series[idxs - 1]
		series = data.frame(series, row.names = trans_dates[2:length(trans_dates)])
		write.csv(series, ofile)
	}
	
	series
}

# Read data files
fm_read_data_raw <- function(file_names)
{
	all_data = list()
	i = 0
	
	while (i < length(file_names)) {
		file_name_start = file_names[i + 1]
		file_name_end = file_names[i + 2]
		file_name_end_quarter = file_names[i + 3]
		
		# Read "start" file
		data_start = read.csv(file_name_start)
		data_start = fm_utils_mkuniq(data_start, "ticker")
		rownames(data_start) = data_start[, "ticker"]
		
		# Read "end" file
		data_end = read.csv(file_name_end)
		data_end = fm_utils_mkuniq(data_end, "ticker")
		rownames(data_end) = data_end[,"ticker"]

		# Separate out common tickers
		common_tickers = intersect(rownames(data_start), rownames(data_end))

		# Using only common tickers now
		data_start = data_start[common_tickers, ]
		data_end = data_end[common_tickers, ]

		# Make sure same tickers appear in same order in both files
		if (any(rownames(data_start) != rownames(data_end))) {
			stop("ERROR: start and end file do not match")	
		}

		# Calculate return from start and end prices
		rets = (data_end[, "price"] - data_start[, "price"])/data_start[, "price"]

		if (!is.na(file_name_end_quarter)) {
			# Read quarter "end" file
			data_end_quarter = read.csv(file_name_end_quarter)
			data_end_quarter = fm_utils_mkuniq(data_end_quarter, "ticker")
			rownames(data_end_quarter) = data_end_quarter[,"ticker"]
	 
			data_end_quarter = data_end_quarter[common_tickers,] 
			unavailable_ticker_idxs = which(is.na(data_end_quarter[, "price"]))
			
			rownames(data_end_quarter)[unavailable_ticker_idxs] = rownames(data_end)[unavailable_ticker_idxs]
			
			# For tickers with missing quarter end prices, put the same price as month-end price
			data_end_quarter[unavailable_ticker_idxs, "price"] = data_end[unavailable_ticker_idxs, "price"]
				
			rets_quarter = (data_end_quarter[, "price"] - data_start[, "price"])/data_start[, "price"]
		}
		else 
			rets_quarter = rep(NA, nrow(data_start))
		
		# Insert quarter end return
		data_start = cbind(data_start, rets_quarter)
		colnames(data_start)[ncol(data_start)] = "return_quarter"
		
		# Insert month end return
		data_start = cbind(data_start, rets)
		colnames(data_start)[ncol(data_start)] = "return"
		
		# We are done
		all_data[[i/3 + 1]] = data_start
		
		i = i + 3
	}
	
	all_data
}

fm_matching_date_idx <- function(splitted_files, date_to_match)
{
	# Find matching end file
	for (j in 1:length(splitted_files)) {
				
		if (splitted_files[[j]][1] == date_to_match) { 		
			match_found = TRUE
			break
		}
	}	
	
	stopifnot(match_found)
	return(j)
}

# For files in old format, i.e., start and end files for each period
fm_setup_old <- function(read_folder, write_folder)
{
        # Get names of raw data files
        if (file.exists(read_folder)) {
                data_files_start = list.files(read_folder, "START.CSV$")
                data_files_start = fm_sort_files_by_date(data_files_start)
                data_files_end = list.files(read_folder, "END.CSV$")
        }
        else
                stop("ERROR: Raw files folder does not exist")

        # Dates for which formatted table already exist 
        existing_dates = c()

        # Get names of formatted data files
        if (file.exists(write_folder)) {
                data_file_form = list.files(read_folder, ".csv$")
                if (length(data_file_form) > 0)
                        existing_dates = unlist(strsplit(data_file_form, ".csv"))
        }

        # Separate out dates in "end files"
        splitted_files_end = strsplit(data_files_end, "[ ]+")

        # All final file go here
        all_files = c()

        # Dates for all files, as given in file name
        all_dates = c()

        # Populate all_file such that start file is follwed by
        # corresponding end file
        for (i in 1:length(data_files_start)) {
                f_name = data_files_start[i]
                f_name_quarter = data_files_start[i + 3]

                # Put in start file name 
                all_files = append(all_files, paste(read_folder, "/", f_name, sep = ""))
                match_found = FALSE

                # Extract start date
                f_date = strsplit(f_name, "[ ]+")[[1]][1]
                f_date_quarter = strsplit(f_name_quarter, "[ ]+")[[1]][1]

                # Update dates
                all_dates = append(all_dates, f_date)

                # Find matching files for f_date and f_date_quarter
                idx = fm_matching_date_idx(splitted_files_end, f_date)
                # Put in end file name
                all_files = append(all_files, paste(read_folder, "/", data_files_end[idx], sep = ""))

                if (!is.na(f_date_quarter)) {
                        idx = fm_matching_date_idx(splitted_files_end, f_date_quarter)
                        # Put in end file name
                        all_files = append(all_files, paste(read_folder, "/", data_files_end[idx], sep = ""))
                }
                else
                        all_files = append(all_files, NA)
        }

        # Read all data
        all_data = fm_read_data_raw(all_files)

        # Get index data
        idx_series = fm_index_data(all_dates)

        # Residual return
        all_data = fm_append_resret(all_data, idx_series)

        # Append variance to data
        all_data = fm_append_var(all_data, 24)

        # Remove dates for formatted files that already exist
        dates_to_remove = c()
        for (i in 1:length(all_files)) {
                if (i %% 2 == 0)
                        next

                cur_date = strsplit(all_files[i], "/")[[1]][2]
                cur_date = strsplit(cur_date, "[ ]+")[[1]][1]

                if (cur_date %in% existing_dates) {
                        dates_to_remove = append(dates_to_remove, c(i, i + 1))
                }
        }

        if (length(dates_to_remove) > 0)
                all_files = all_files[-dates_to_remove]

        # Setup sector columns
        all_data = fm_setup_sectors(all_data, all_files, write_folder)

        all_data
}

# Looks like SQL has added additional underscores to column names
FM_FACTORS_ = c("ticker", "company","price","rs_04_w","rs_13_w","rs_26_w","rs_52_w",
              "pr_prh_52_w","epsdmp_ey0","epsdmp_ey1","epsdmp_eq0",
              "epsdmp_eq1","eyield_12_m","udef0_e","udef0_f","udef10",
              "udef02","udef00","udef08","udef09","udef0_d","yield",
              "udef05","roic_a5_y","roe_12_m","roe_a5_y","roa_12_m",
              "ta_trn_12_m","epsdc_g1_t","epsdc_g1_q5","udef12","sales_g1_q5",
              "opm_12_m","opm_a5_y","invtrn_12_m","ltd_tc_q1","curr_q1",
              "tie_12_m","shrinsd","ins_pr_shr","shrinst","udef0_c",
              "udef06","udef0_a","mktcap","beta", "ind_3_dig")

# Our output files would not contain the additional underscores in column names
FM_FACTORS = c("ticker", "company","price","rs_04w","rs_13w","rs_26w","rs_52w",
              "pr_prh_52w","epsdmp_ey0","epsdmp_ey1","epsdmp_eq0",
              "epsdmp_eq1","eyield_12m","udef0e","udef0f","udef10",
              "udef02","udef00","udef08","udef09","udef0d","yield",
              "udef05","roic_a5y","roe_12m","roe_a5y","roa_12m",
              "ta_trn_12m","epsdc_g1t","epsdc_g1q5","udef12","sales_g1q5",
              "opm_12m","opm_a5y","invtrn_12m","ltd_tc_q1","curr_q1",
              "tie_12m","shrinsd","ins_pr_shr","shrinst","udef0c",
              "udef06","udef0a","mktcap","beta", "ind_3_dig")

fm_append_returns <- function(all_files)
{
  n_files = length(all_files)

  all_data = list()
  if (n_files > 1) {
    for (i in 1:(n_files - 1)) {
      print(all_files[i])
	  
	  tryCatch ({
		  data_current = read.csv(all_files[i])
		  data_current = data_current[, FM_FACTORS_]
		  colnames(data_current) = FM_FACTORS
		  data_month = read.csv(all_files[i + 1])
		  data_month = data_month[, FM_FACTORS_]
		  colnames(data_month) = FM_FACTORS
	  }, error = function(ex) {
		  cat("An error was detected for ", all_files[i], "\n", all_files[i + 1], "\n")
		  print(ex)
	  }
	  )

      data_current = fm_utils_mkuniq(data_current, "ticker")
      rownames(data_current) = data_current[, "ticker"]

      data_month = fm_utils_mkuniq(data_month, "ticker")
      rownames(data_month) = data_month[,"ticker"]

      # Separate out common tickers
      common_tickers = intersect(rownames(data_current), rownames(data_month))

      # Using only common tickers now
      data_current = data_current[common_tickers, ]
      data_month = data_month[common_tickers, ]

      # Make sure same tickers appear in same order in both files
      if (any(rownames(data_current) != rownames(data_month))) {
        stop("ERROR: start and end file do not match")
      }

      # Calculate return from start and end prices
      rets = (data_month[, "price"] - data_current[, "price"])/data_month[, "price"]

      # Insert month end return
      data_current = cbind(data_current, rets)
      colnames(data_current)[ncol(data_current)] = "return"

      all_data[[i]] = data_current
    }
  }

  # For the last file, can't compute the returns  
  data_last = read.csv(all_files[n_files])
  data_last = data_last[, FM_FACTORS_]
  colnames(data_last) = FM_FACTORS
  rets = rep(NA, nrow(data_last))
  data_last = cbind(data_last, rets)
  colnames(data_last)[ncol(data_last)] = "return"
  rownames(data_last) = data_last[, "ticker"]
  all_data[[length(all_data) + 1]] = data_last
 
  all_data
}

# For files in new format. No separate start and end files
fm_setup_new <- function(read_folder, write_folder)
{  
        # Get names of raw data files
        if (file.exists(read_folder)) {
                data_files = list.files(read_folder, "raw.csv$")
                data_files = fm_sort_files_by_date(data_files, "\\.")
        }
        else
                stop("ERROR: Raw files folder does not exist")

        # Dates for which formatted table already exist 
        existing_dates = c()

        # Get names of formatted data files
        if (file.exists(write_folder)) {
                data_file_form = list.files(write_folder, ".csv$")
                if (length(data_file_form) > 0)
                        existing_dates = unlist(strsplit(data_file_form, ".csv"))
        }
        else {
          dir.create(write_folder)
        }  
 
        # All final file go here
        all_files = data_files

        # Remove dates for formatted files that already exist
        dates_to_remove = c()
        for (i in 1:length(all_files)) {
                cur_date = strsplit(all_files[i], "\\.")[[1]][1]
               
                if (cur_date %in% existing_dates) {
                        dates_to_remove = append(dates_to_remove, i)
                }
        }

        if (length(dates_to_remove) > 0)
                all_files = all_files[-dates_to_remove]
         
        if (length(all_files) == 0) {
           stop("No files to generate")    
        }   
 
        # Extract all dates
        all_dates = c()
        for (file in all_files) {
          new_date = strsplit(strsplit(file, "/")[[1]][1], ".raw.csv")
          all_dates = c(all_dates, new_date)
        }
        
        all_files = paste(read_folder, all_files, sep = "/")
        print(all_files)
        all_data = fm_append_returns(all_files)

        # Get index data
        idx_series = fm_index_data(all_dates)
      
        # Residual return
        all_data = fm_append_resret(all_data, idx_series)
        
        # Append variance to data
        all_data = fm_append_var(all_data, 24)

        # Setup sector columns
        all_data = fm_setup_sectors(all_data, all_files, write_folder)

        all_data
}

# Convert raw CSV data into formatted data
fm_setup_data <- function(old = FALSE)
{
	# First load required packages
	fm_load_packages()
	
	# Read config file for data
	c_file = scan("data_config.txt", what = "character", sep = "\n")

	folder_names = c()
	
	# Read folder names
	i = 1

	while (i <= length(c_file)) {
		str = c_file[i]
		
		str_parts = strsplit(str, "[ ]+")[[1]]

		if (str_parts[1] == "#") {
			i = i + 1
			next
		}
		
		folder_names = append(folder_names, str_parts)
		i = i + 1
	}

	# Setup folder names
	read_folder = folder_names[1]
	write_folder = folder_names[2]

        if (old == TRUE)
          all_data = fm_setup_old(read_folder, write_folder)	
        else
          all_data = fm_setup_new(read_folder, write_folder)

        all_data
}
