# Meta --------------------------------------------------------------------

## Author:        Nixon Candiales
## Date Created:  10/19/2022
## Date Edited:   10/19/2022
## Description:   Analysis file for empirical exercise 3

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, haven)

source(here("Assigments", "AS 3", "Code", "set.mydir.R"))

# Load the data -----------------------------------------------------------

dat <- read_dta("Data/Data_main.dta")

# Pat 1 replication -------------------------------------------------------
dat %>% group_by(orgParentCode) %>% mutate(firmID = cur_group_id())

dat %>% group_by(uniqueID) %>% mutate(uniqueIDNum = cur_group_id())

prueba <- function(var, name){

dat %>% group_by({var}) %>% transmute({name} = cur_group_id())

}