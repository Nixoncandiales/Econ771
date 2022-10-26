# Meta --------------------------------------------------------------------

## Author:        Nixon Candiales
## Date Created:  10/19/2022
## Date Edited:   10/19/2022
## Description:   Analysis file for empirical exercise 3

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, haven, stargazer)

source(here("Assigments", "AS 3", "Code", "set.mydir.R"))

# Load the data -----------------------------------------------------------
dat <- vroom("Data/Data.csv")

# Part 1 replication -------------------------------------------------------
source("Code/table1.R")

# Part 2 replication -------------------------------------------------------
# Replicated in Stata. Run main.do
# It will run the do file fig3 and save the graph on disk on Output/fig/Figure3.png

# Part 3 replication -------------------------------------------------------