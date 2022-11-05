# Meta --------------------------------------------------------------------

## Author:        Nixon Candiales
## Date Created:  10/19/2022
## Date Edited:   11/04/2022
## Description:   Analysis file for empirical exercise 3

# Preliminaries -----------------------------------------------------------
rm(list=ls())
gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, haven, stargazer,
               rdrobust, rddensity, modelsummary, fixest, purrr)

source(here("Assigments", "AS 3", "Code", "set.mydir.R"))

# Load the data -----------------------------------------------------------

dat <- vroom("Data/Data.csv")

lnS <- dat  %>% pull(lnS) 
LISPremium <- dat %>%  pull(LISPremium) 

# Part 1 replication -------------------------------------------------------
source("Code/Q1.R")

# Part 2 replication -------------------------------------------------------
# Replicated in Stata. Run main.do
# It will run the do file Q2.do and save the graph on disk on Output/fig/Q2.png

# Part 3 replication -------------------------------------------------------
source("Code/Q3.R")

# Part 4 replication -------------------------------------------------------
source("Code/Q4.R")

# Part 5 replication -------------------------------------------------------
source("Code/Q5.R")

# Part 6 replication -------------------------------------------------------
source("Code/Q6.2.R")

# Part 7 replication -------------------------------------------------------
source("Code/Q7.R")

# Part 8 replication -------------------------------------------------------
source("Code/Q8.R")

