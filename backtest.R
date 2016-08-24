fm_tickers_and_rets <- function(all_files, period = "monthly")
{ 
	if (period == "monthly")
		ret_colname = "return"
	else
		ret_colname = "return_quarter"
	
	# Obtain corresponding results files
	for (file_path in all_files) {
		print(file_path)
		file = strsplit(file_path, '/')
		# Data directory
		r_dir = file[[1]][1]
		
		# result file
		r_date = file[[1]][2]
		r_date = tolower(r_date)
		r_file = paste(r_dir, "/result_", r_date, sep = "")

		# read top 25 and bottom 25 tickers
		exp_ret = read.csv(r_file, row.names = 1)
		tickers = rownames(exp_ret)
		
		if (!exists("ticker_table")) {
			ticker_table = as.ts(tickers)
		}
		else {
			ticker_table = cbind(ticker_table, as.ts(tickers))
		}
		
		# Read returns for tickers
		data_table = read.csv(file_path, row.names = 1)
		if (!exists("rets_table"))
			rets_table = as.ts(data_table[tickers, ret_colname])
		else
			rets_table = cbind(rets_table, as.ts(data_table[tickers, ret_colname]))
	}
	
	retval = list()
	colnames(ticker_table) = seq(1, ncol(ticker_table))
	colnames(rets_table) = seq(1, ncol(rets_table))
	retval$tickers = as.data.frame(ticker_table)
	retval$rets = as.data.frame(rets_table)
	retval
}

fm_pfolio_elems <- function(longs, shrts)
{
	if (is.null(longs) || is.null(shrts))
		return(NULL)
	
	retval = list()
	
	# Separate out positive tickers from negative tickers
	retval$win_ret_long_idxs = win_ret_long_idxs = which(longs[,1] >= 0)
	retval$win_ret_shrt_idxs = win_ret_shrt_idxs = which(shrts[,1] < 0)
	retval$los_ret_long_idxs = los_ret_long_idxs = which(longs[,1] < 0)
	retval$los_ret_shrt_idxs = los_ret_shrt_idxs = which(shrts[,1] >= 0)
	
	retval$win_tickers_long = rownames(longs)[win_ret_long_idxs]
	retval$win_tickers_shrt = rownames(shrts)[win_ret_shrt_idxs]
	retval$los_tickers_long = rownames(longs)[los_ret_long_idxs]
	retval$los_tickers_shrt = rownames(shrts)[los_ret_shrt_idxs]
	
	# Calculate profit and loss
	retval$win_ret_long = longs[win_ret_long_idxs, 1]
	retval$win_ret_shrt = shrts[win_ret_shrt_idxs, 1]
	retval$los_ret_long = longs[los_ret_long_idxs, 1]
	retval$los_ret_shrt = shrts[los_ret_shrt_idxs, 1]
	
	retval$win_wt_long = longs[win_ret_long_idxs, 2] 
	retval$win_wt_shrt = shrts[win_ret_shrt_idxs, 2] 
	retval$los_wt_long = longs[los_ret_long_idxs, 2] 
	retval$los_wt_shrt = shrts[los_ret_shrt_idxs, 2] 
	
	retval
}

# Update carryover portfolio for longs
fm_update_pfolio <- function(olds, old_elems, new_elems, old_netval, new_netval)
{
	flag = sign(new_netval)
	
	if (flag > 0) {
		old_los_tickers = old_elems$los_tickers_long
		new_los_tickers = new_elems$los_tickers_long
		old_los_wt = old_elems$los_wt_long
		new_los_wt = new_elems$los_wt_long
		new_los_ret = new_elems$los_ret_long
		old_los_ret = old_elems$los_ret_long
	}
	else {
		old_los_tickers = old_elems$los_tickers_shrt
		new_los_tickers = new_elems$los_tickers_shrt
		old_los_wt = old_elems$los_wt_shrt
		new_los_wt = new_elems$los_wt_shrt
		new_los_ret = new_elems$los_ret_shrt
		old_los_ret = old_elems$los_ret_shrt
	}
	
	new_los_val = (new_los_wt/sum(new_los_wt)) * new_netval
	old_los_val = (old_los_wt/sum(old_los_wt)) * old_netval
	com_idxs_old = which(old_los_tickers %in% new_los_tickers)
	com_idxs_new = which(new_los_tickers %in% old_los_tickers)

	# Merge common tickers between old and new ticker sets
	if (length(com_idxs_old) > 0) {
		new_los_val_adj = new_los_val[com_idxs_new]/(1 + old_los_ret[com_idxs_old])
			
		old_los_val[com_idxs_old] = old_los_val[com_idxs_old] + new_los_val_adj
	
		# 'new' now will not have any data already in 'old'
		new_los_val = new_los_val[-com_idxs_new]
		new_los_tickers = new_los_tickers[-com_idxs_new]
		new_los_ret = new_los_ret[-com_idxs_new]
	}
	
	# Following two values now don't have any common components
	old_netval = sum(old_los_val)
	new_netval = sum(new_los_val)
			
	total_val = old_netval + new_netval 
	all_tickers = c(old_los_tickers, new_los_tickers)
	all_rets = c(old_los_ret, new_los_ret)
	
	olds = fm_make_pfolio(all_tickers, all_rets, flag * c(old_los_val, new_los_val)/total_val) 
	
	olds
}

fm_make_pfolio <- function(tickers, ret, wts)
{
	if (is.null(tickers))
		return(NULL)
		
	data.frame(ret, wts, row.names = tickers)
}

# Take out tickers not available any more
fm_reset_carryover <- function(all_tickers, pfolio, netval)
{
	if (is.null(pfolio))
		return (NULL)
	
	idxs_unavailable = which(!(rownames(pfolio) %in% all_tickers))
	idxs_available = which(rownames(pfolio) %in% all_tickers)
	
	adjusted_wts_to_remove = pfolio[idxs_unavailable,2]/sum(pfolio[idxs_unavailable,2]) 
	
	realized_ret = sum(adjusted_wts_to_remove * pfolio[idxs_unavailable,1])
	
	val_to_remove = sum(pfolio[idxs_unavailable,2]) * netval * sign(netval)
	
	if (netval > 0)
		realized_val = val_to_remove * (1 + realized_ret)
	else
		realized_val = val_to_remove * realized_ret
	
	# Updated total value of carryover
	netval = netval - val_to_remove
	
	# Make weights add to 1/-1
	pfolio = pfolio[idxs_available, ]
	pfolio[,2] = pfolio[,2]/sum(pfolio[,2]) * sign(netval)
	
	retval = list()
	retval$realized_val = realized_val
	retval$netval = netval
	retval$pfolio = pfolio
	retval
}

# Given current return and next return, generates new current return
fm_update_ret <- function(current_ret, next_ret) 
{
	current_ret + next_ret + current_ret * next_ret	
}

fm_bt_carryover <- function(monthly_tickers, monthly_rets, cur_cash, cur_libl, init_wt, n_tickers_to_use)
{
	cur_eqty = cur_shrt = 0
	old_longs = old_shrts = NULL
	add_to_cash = 0
	last_val = cur_cash
	
	for (i in 1:ncol(monthly_rets)) {
		# Table of returns and weights for current month
		new_longs = fm_make_pfolio(monthly_tickers[1:n_tickers_to_use, i], 
		                           monthly_rets[1:n_tickers_to_use, i], 
								   init_wt[1:n_tickers_to_use])
								   
		tickers = tail(na.omit(monthly_tickers[,i]), n_tickers_to_use)
		n_tickers = length(na.omit(monthly_tickers[,i]))
		new_shrts = fm_make_pfolio(tickers, 
		                           monthly_rets[(n_tickers - n_tickers_to_use + 1):n_tickers, i], 
								   init_wt[(n_tickers_to_use + 1):(n_tickers_to_use * 2)])
		
		# Preliminary action to create old long portfilio
		retval_long = fm_reset_carryover(monthly_tickers[, i], old_longs, cur_eqty)
		old_longs = retval_long$pfolio 
		if (!is.null(retval_long)) { 
			cur_eqty = retval_long$netval 
			add_to_cash = retval_long$realized_val
		}	
				
		# Preliminary action to create old long and short portfilio
		retval_shrt = fm_reset_carryover(monthly_tickers[, i], old_shrts, cur_shrt)
		old_shrts = retval_shrt$pfolio
		if (!is.null(retval_shrt)) {
			cur_shrt = retval_shrt$netval
			add_to_cash = add_to_cash + retval_shrt$realized_val
		}
		
		long_idxs = which(monthly_tickers[,i] %in% rownames(old_longs)) 
		shrt_idxs = which(monthly_tickers[,i] %in% rownames(old_shrts))
		
		# Table of returns and weights of carryover. Note returns are being updated
		old_longs = fm_make_pfolio(rownames(old_longs), 
		                           fm_update_ret(old_longs[,1], monthly_rets[long_idxs, i]), 
								   old_longs[,2])
		
		old_shrts = fm_make_pfolio(rownames(old_shrts), 
		                           fm_update_ret(old_shrts[,1], monthly_rets[shrt_idxs, i]), 
								   old_shrts[,2])
								
		new_elems = fm_pfolio_elems(new_longs, new_shrts)
		old_elems = fm_pfolio_elems(old_longs, old_shrts)
		
		# Losers (longs)
		new_eqty = cur_eqty * sum(old_elems$los_wt_long) + cur_cash * sum(new_elems$los_wt_long)
		
		# Losers (shorts)
		new_libl = (cur_shrt * sum(old_elems$los_wt_shrt) + cur_libl * sum(new_elems$los_wt_shrt)) * (-1)  
		
		cur_cash = cur_cash * sum(new_elems$win_wt_long) * (1 + sum(new_elems$win_wt_long * new_elems$win_ret_long)) - 
		           cur_libl * sum(new_elems$win_ret_shrt * new_elems$win_wt_shrt) +
				   cur_eqty * sum(old_elems$win_wt_long) * (1 + sum(old_elems$win_wt_long * old_elems$win_ret_long)) -
				   cur_shrt * sum(old_elems$win_ret_shrt * old_elems$win_wt_shrt) +
				   add_to_cash
	    
	    # Update carry-over (longs)
		old_longs = fm_update_pfolio(old_longs, old_elems, new_elems, cur_eqty, new_eqty)
		
		# Update carry-over (short)
		old_shrts = fm_update_pfolio(old_shrts, old_elems, new_elems, cur_shrt, new_libl)
		
		cur_libl = -cur_cash
		cur_eqty = new_eqty
		cur_shrt = new_libl
		
		verbose = 0
		if (verbose) {
			#cat("Cash:" , cur_cash, "\n")
			#cat("eqty", cur_eqty, "\n")
			#cat("shrt", cur_shrt, "\n")	
		}
		
		#Display new net value		
		new_val = cur_cash + cur_eqty * (1 + sum(old_longs[,1] * old_longs[,2])) - cur_shrt * sum(old_shrts[,1] * old_shrts[,2])
		perc = (new_val - last_val)/last_val
		cat(perc, ",", new_val, "\n")
		last_val = new_val
	}
}

fm_bt_simple <- function(monthly_tickers, monthly_rets, cur_cash, init_wt, n_tickers_to_use)
{
	ncol_monthly_rets = ncol(monthly_tickers)
	
	for (i in 1:ncol_monthly_rets) {
		# Table of returns and weights for current month
		new_longs = fm_make_pfolio(monthly_tickers[1:n_tickers_to_use, i], 
		                           monthly_rets[1:n_tickers_to_use, i], 
								   init_wt[1:n_tickers_to_use])
								   
		tickers = tail(na.omit(monthly_tickers[,i]), n_tickers_to_use)
		n_tickers = length(na.omit(monthly_tickers[,i]))
		new_shrts = fm_make_pfolio(tickers, 
		                           monthly_rets[(n_tickers - n_tickers_to_use + 1):n_tickers, i], 
								   init_wt[(n_tickers_to_use + 1):(n_tickers_to_use * 2)])
		
		ret = mean(new_longs[,1]) - mean(new_shrts[,1])

		cur_cash = cur_cash * (1 + ret)
		cat(ret, ",", cur_cash, "\n")
	}
	
	cat("Simply Backtest", cur_cash, "\n")
}

fm_bt <- function(type = "simple", period = "monthly")
{
	conf = fm_parse_config()
	all_files = conf$data_files_list[[length(conf$data_files_list)]]
	num_tickers = 25

	if (period != "monthly") {
		mask = c(2,3,5,6,8,9,11,12)
		all_files = all_files[-mask]
		print(all_files)
	}
	
	retval = fm_tickers_and_rets(all_files, period)
	
	monthly_tickers = retval$tickers
	monthly_rets = retval$rets
	
	cur_cash = 1000
	cur_libl = -1000
	
	init_wt = append(rep(1/num_tickers, num_tickers), rep(-1/num_tickers, num_tickers))
	
	if (type == "simple") {
		fm_bt_simple(monthly_tickers, monthly_rets, cur_cash, init_wt, num_tickers)
	}
	else{
		fm_bt_carryover(monthly_tickers, monthly_rets, cur_cash, cur_libl, init_wt, num_tickers) 							
	}
}