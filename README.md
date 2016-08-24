FM Stocks v0.1
--------------
This is an implementation of Expected Return Factor Model in R. 

USAGE (Windows)
===============

PRELIMINARY STEPS
-----------------
1. Launch R

2. Go to File -> Change dir ...
   Select the FM Stocks folder

3. Type source("main.R") on R console. This will make
   FM Stocks available to use in R

GENERATING DATA FILES
---------------------
1. Specify name of the folder containing raw data
   files in data_config.txt. This must be specified
   on the first line following the instructions
   lines.

2. Specify name of the folder where formatted
   data files will be placed on the second 
   line of data_config.txt

3. On R prompt, type fm_data(). This will convert
   generate formatted data files and place them 
   in the specified folder

GENERATING EXPECTED RETURNS
---------------------------
1. Make necessary changes to config.txt file

   See default file in FM Stocks folder 
   and make any changes if needed

   "# Data Directory" specifies name of the folder
   where factor data files are located. Name of
   the data directory is specified below this 
   header
   
   "# Data Files" specifies the names of the data 
   files, one per line. Names of data files are 
   specified below this header. You can specify
   one or more files per line. Expected returns 
   will be generated for each of the files 
   specfied.  

   "# Result Prefix" specifies the file prefix 
   that will be used for files containing result
   of factor model analysis

2. Type fm_main() on R console to launch FM Stocks

3. See result file in data directory for results. 
   The file named <result prefix>_<time stamp>.csv
   contains rankings of the stocks based on expected
   returns. This file can be view in Excel

   The file names <result prefix>_<comparison>_<time_stamp>
   contains results of regression of realized returns 
   over deciles of ranked stocks. This file will not 
   exist if realized returns are not available

EXECUTING BACKTESTS
-------------------
1. On R prompt, just type fm_backtest(). The backtest
   system will generate a 12 month backtest ending 
   with the date of the last file in "Data Files"
   section of config.txt file. This is the default
   backtest and the results correpond to monthly
   rebalancing of portfolio.

2. Optionally, the backtest system accepts two 
   parameters. One is the 'type' specified, 
   which takes the values "simple" and "hold",
   e.g.,

       fm_backtest(type = "simple")
   
   "simple" is the default type. You can use 
   "hold" to keep losers across rebalancing periods.

   The other argument is 'period', that takes values
   "monthly" and "quarterly". "monthly" is the default
   where portfolio is rebalanced every month. When 
   "quarterly" is specified, portfolio will be rebalanced
   every 3 months. 

