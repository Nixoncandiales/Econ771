## @knitr Analysis

# Meta --------------------------------------------------------------------

# Title:  Combine ACS and Medicaid Expansion Data
# Author: Nixon Candiales
# Date Created: 9/7/2022
# Date Edited:  ----


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, here, descriptr, haven)

# Read Data -----------------------------------------------------------

data_hcris <- read.delim(here("Assigments", "As 1", "Output", "HCRIS", "HCRIS_Data.txt"))
data_pos <- read_stata(here("Assigments", "As 1", "Output", "POS", "pos_lastyear.v12.dta"))

ds_screener(data_hcris)
ds_screener(data_pos)

# Analysis -----------------------------------------------------------

ds_summary_stats(data_hcris, tot_uncomp_care_charges, tot_pat_rev)

#To do:

# Mean
# Standard Deviation
# Min
# Max

#Over two variables (Hospital Total Reveneus, Uncompensated Care Over Time - tot_uncomp_care_charges)


#tidytable(data, info_cols = list(), calc_cols = list(`#missing` =
 # function(x) sum(is.na(x))), num_cols = list(mean = mean, sd = sd),
 # custom_vars = c(), custom_cols = list(), col_order = c(),
 # row_order = list(), digits = 2, add_cat_header_row = TRUE)