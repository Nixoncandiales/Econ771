## ----setup2, include=FALSE---------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(include = TRUE)
knitr::opts_chunk$set(cache = TRUE)

# Import the required packages and set the working directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, sqldf, ggthemes, fixest, modelsummary, plm, ivmodel, xtable)
#setwd("GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")


## ----Q1, include=TRUE, echo=FALSE, results='asis', comment=NA, warning=FALSE, message=FALSE----
source('Code/Q1.R')
table1


## ----Q2, , include=TRUE, echo=FALSE, comment=NA, warning=FALSE, message=FALSE----
source("Code/Q2.R")
plot1


## ----Q3, eval=TRUE,results='asis', include=TRUE, comment=NA, warning=FALSE, message=FALSE, echo=FALSE----
source("Code/Q3.R")
etable(mod.ols,
       tex=T, style.tex=style.tex('aer'))


## ----Q4, eval=TRUE,results='asis', include=TRUE, comment=NA, warning=FALSE, message=FALSE, echo=FALSE----
source('Code/Q4.R')
tab4
gc()


## ----Q5, eval=TRUE,results='asis', include=TRUE, comment=NA, warning=FALSE, message=FALSE, echo=FALSE----
source('Code/Q5.R')
reg.dat.2sls <- reg.dat.2sls %>% filter(!is.na(int)) %>% filter(!is.na(practice_rev_change))

reg_1S <- feols(int ~ practice_rev_change | npi+Year, data=reg.dat.2sls)
reg.dat.2sls$INThat <- reg_1S$fitted.values
reg_2S <- feols(log_y ~ INThat + average_submitted_chrg_amt + 
                         average_medicare_payment_amt| npi+Year, data=reg.dat.2sls)
reg_RF <- feols(log_y ~ practice_rev_change + average_submitted_chrg_amt + 
                         average_medicare_payment_amt | npi+Year, data=reg.dat.2sls)

#etable(reg_1S,reg_RF,reg_2S)
etable(reg_1S, reg_RF, reg_2S,
       tex=T, style.tex=style.tex('aer'))



## ----Q6, eval=TRUE,results='asis', include=TRUE, comment=NA, warning=FALSE, message=FALSE, echo=FALSE----
source('Code/Q6.R')
etable(mod.DWH,
       tex=T, style.tex=style.tex('aer'))


## ----Q7, eval=TRUE,results='asis', include=TRUE , comment=NA, warning=FALSE, message=FALSE----
source('Code/Q7.R')
table7


## ----Q8, eval=TRUE,results='asis', include=TRUE , comment=NA, warning=FALSE, message=FALSE----
source('Code/Q8.R')
tab8

