# Meta --------------------------------------------------------------------

## Author:        Nixon Candiales
## Date Created:  10/19/2022
## Date Edited:   10/19/2022
## Description:   Analysis file for empirical exercise 3

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, haven, stargazer,
               rdrobust, rddensity, modelsummary, fixest)

source(here("Assigments", "AS 3", "Code", "set.mydir.R"))

# Load the data -----------------------------------------------------------
dat <- vroom("Data/Data.csv")

# Part 1 replication -------------------------------------------------------
source("Code/Q1.R")

# Part 2 replication -------------------------------------------------------
# Replicated in Stata. Run main.do
# It will run the do file fig3 and save the graph on disk on Output/fig/Figure3.png

# Part 3 replication -------------------------------------------------------

dat %>% filter(year == 2006) %>% summarise(prueba = mean(LISPremium, na.rm=T))

hist(dat$LISPremium)

# Part 4 replication -------------------------------------------------------
lnS <- dat %>% pull(lnS) 
LISPremium <- dat %>% pull(LISPremium) 

fig4.a <- rdplot(y = lnS, 
                 x = LISPremium,
                 title = "RD Plot",
                 x.label = "Running Variable",
                 y.label = "Outcome")
fig4.a

# Part B missing.

# Part 5 replication -------------------------------------------------------

rddensity(lnS, LISPremium)


# Part 6 replication -------------------------------------------------------
source("Code/Q6.R")
panel.a
panel.b

# Part 7 replication -------------------------------------------------------
mod.rd <- rdrobust(lnS, LISPremium)
summary(mod.rd)
# Part 8 replication -------------------------------------------------------

# Part 9 replication -------------------------------------------------------

# Part 10 replication -------------------------------------------------------
