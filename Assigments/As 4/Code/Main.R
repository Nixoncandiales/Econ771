# Meta --------------------------------------------------------------------

## Author:        Nixon Candiales
## Date Created:  11/14/2022
## Date Edited:   11/14/2022
## Description:   Analysis file for empirical exercise 4

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, readxl)

source(here("Assigments", "AS 3", "Code", "set.mydir.R"))
set.mydir(as=4)

# Load the data -----------------------------------------------------------

dat_market <- readRDS("Data/hospital_markets.rds")
dat_county <- vroom("Data/zcta-to-county.csv", skip=1, col_names=TRUE)
dat_zip <- read_excel("Data/ZipHsaHrr15.xls")

# Part 1 replication -------------------------------------------------------

# Part 2 replication -------------------------------------------------------

# Part 3 replication -------------------------------------------------------

# Part 4 replication -------------------------------------------------------

# Part 5 replication -------------------------------------------------------

# Part 6 replication -------------------------------------------------------

# Part 7 replication -------------------------------------------------------

# Part 8 replication -------------------------------------------------------

# Part 9 replication -------------------------------------------------------

# Part 10 replication -------------------------------------------------------
