## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(include = TRUE)
knitr::opts_chunk$set(cache = F)


## ----load-pack, include=FALSE-----------------------------------------
# Import the required packages and set the working directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, sqldf, ggthemes, fixest, modelsummary, plm, GGally, ivreg, aod)
setwd("~/Documents/GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")
gc()
rm(list=ls())


## ----Merge_V2, include=TRUE, echo=TRUE--------------------------------
#-----------------------------------------------------------------------------
## Merge the data and select the variables required for the analysis.
#-----------------------------------------------------------------------------
# source(here("Code", "Merge2.R"))

# Read the merged data
dat <- vroom(here("Output", "dat.csv"))


## ----Q1, include=TRUE, echo=TRUE--------------------------------------
#table 1
  dat %>% 
        ungroup()
        summarise_at(c('Total_Spending', 'Total_Claims', 'Total_Patients'),
                     list(Mean = mean, Std.Dev. = sd, Min = min, Max = max), na.rm=T) %>%
        pivot_longer(cols = everything(),
                     names_to = c("colNames", ".value"), 
                     names_sep = "_",
                     names_prefix = "Total_") -> table1
  table1


## ----Q2---------------------------------------------------------------
# Note from the documentation page 14 https://resdac.org/sites/datadocumentation.resdac.org/files/MD-PPAS%20User%20Guide%20-%20Version%202.4.pdf 
# pos_asc -> % of line items delivered in ambulatory surgery center (asc)
# pos_office ->  % of line items delivered in office
# pos_opd -> % of items delivered in hospital outpatient department (opd)
# This variables where used in the creation of INT in the Merge2.R file.

# plot
dat  %>% 
  filter(!is.na(int)) %>%
  group_by(Year, int) %>%
  summarise(mean_claims_count = mean(Total_Claims, na.rm = TRUE)) %>%
  ggplot(aes(y=mean_claims_count , x=Year, 
             group=factor(int), color=factor(int))) +
  geom_line() +
  theme_tufte()+ 
  labs(x="Years", y="Number of Claims", 
       title = "Mean of physician-level claims for integrated versus non-integrated physicians over time") -> plot1
  plot1


## ----Q3---------------------------------------------------------------
# Drop phys that were integrated as of 2012 and run the regression
reg.dat <- dat %>%
               group_by(npi) %>%
               mutate(sum_int = sum(int)) %>%
               filter(!(is.na(sum_int) & sum_int < 6)) %>% ## Drop ALL doctor that where integrated since 2012! thanks onejune.
               select(c("Year", "npi", "Total_Claims", 
                        "int", "average_submitted_chrg_amt", 
                        "average_medicare_payment_amt", "group1")) %>%
               mutate(
                        log_y = log(Total_Claims),
                        y = Total_Claims,
                        npi = as.character(npi)
                      )

mod.ols <- feols(log_y ~ average_submitted_chrg_amt + 
                         average_medicare_payment_amt + int | 
                         npi + Year, dat = reg.dat)

mod.fe <- modelsummary(mod.ols, output = "modelsummary_list", stars = TRUE) #store the result is a modelsummary_list to reduce memory space
mod.fe


## ----Q4---------------------------------------------------------------
#-----------------------------------------------------------------------------
## Run the regression manually
#-----------------------------------------------------------------------------

mod.ols <- feols(log_y ~ average_submitted_chrg_amt + 
                         average_medicare_payment_amt + 
                         int 
                       | npi + Year, dat = reg.dat)

R2_dx <- r2(mod.ols, type = "ar2")
delta_dx <- mod.ols[["coefficients"]][["int"]]

rm(mod.ols)

mod.ols <- feols(log_y ~ int, dat = reg.dat)
R2_d <- r2(mod.ols, type = "ar2")
delta_d <- mod.ols[["coefficients"]][["int"]]
rm(mod.ols)

rho <- seq(0,2,0.5)
R2_max <- seq(0.5,1,0.1)

delta_star <- c()
k = 1
for (i in seq(0,2,0.5)){
  for (j in seq(0.5,1,0.1)) {
      delta_star[k] = (delta_dx - i*(delta_d - delta_dx)*((j - R2_dx)/ (R2_dx - R2_d)))
      k = k + 1
  }
}


interval <- cbind(delta_dx, delta_star)
interval

rm(i,j,k, delta_d, delta_dx, delta_star, R2_d, R2_dx, R2_max, rho)

# #-----------------------------------------------------------------------------
# ## Robomit package
# #-----------------------------------------------------------------------------
# require(robomit)
# 
# #-----------------------------------------------------------------------------
# ## For loop to loop trough \rho (i) and R^2_max (j) -> interested in Delta*
# #-----------------------------------------------------------------------------
# 
# table2 <- data.frame()
# for (i in seq(0,2,0.5)){
#   for (j in seq(0.5,1,0.1)) {
#     # estimate delta*
#     a <- o_beta(y = "log_y", # dependent variable
#             x = "int", # independent treatment variable
#             #id = "npi",
#             #time = "Year",
#             con = "average_submitted_chrg_amt + average_medicare_payment_amt + npi + Year", # related control variables
#             delta = i, # beta
#             R2max = j, # maximum R-square
#             type = "lm", # model type
#             data = reg.dat) # data set
#     a <- cbind(a[1,], j, i)
#     table2 <- rbind(table2, a)
#   }
# }
# #----
# # Clean memory and auxiliary objects
# #----
# rm(a, i, j)
# gc()
# 
# 
# table2
# 
# #To do: tidy the table present it in a better format.
# 
# #-----------------------------------------------------------------------------
# ## Nice graph Just for fun ---> grid for all 
# ##                                \rho (i) and R^2_max (j) combinations
# #-----------------------------------------------------------------------------
# k=1
# plotlist <- list()
# for (i in seq(0,2,0.5)){
#   for (j in seq(0.5,1,0.1)) {
#          plotlist[[k]] <- o_delta_boot_viz(y = "log_y",# dependent variable
#                                           x = "int",# independent treatment variable
#                                           con = "average_submitted_chrg_amt + average_medicare_payment_amt + npi + Year",# related control variables
#                                           beta = i,# beta for which delta* should be estimated
#                                           R2max = j,# maximum R-square
#                                           type = "lm",# model type
#                                           data = reg.dat,
#                                           sim = 10,
#                                           rep = FALSE,
#                                           obs = 10,
#                                           CI = 95,
#                                           bin = 15) # dataset
#           k = k + 1
#             }
# }
# #----
# # Plot the grid
# #----
# plot2 <- ggmatrix(
#                   plotlist, nrow = 5, ncol = 6,
#                   yAxisLabels = c(seq(0,2,0.5)),
#                   xAxisLabels = c(seq(0.5,1,0.1)),
#                   title = "Visualization of bootstrapped delta*s",
#                   showStrips = FALSE
#                  )
# plot2
# #----
# # Clean memory and auxiliary objects
# #----
# rm(plotlist, k, i , j)
# gc()
# 
# ### Re do this!!!!!! program and use FEOLS. That command actually works on my machine yujuuu!


## ----Q5---------------------------------------------------------------
#source(here("Code", "Instrument.R"))
price.shock <- vroom(here("Output", "instrument.csv"))

# Append the data
reg.dat.2sls <- left_join(reg.dat, price.shock, by=c("group1" = "tax_id", "Year" = "Year"))

#mod.2sls <- ivreg(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + factor(npi) + factor(Year) | int | practice_rev_change, data = reg.dat.2sls) ## Too computational ineficient

mod.2sls.plm <- plm(log_y ~ int + average_submitted_chrg_amt + 
                            average_medicare_payment_amt  | 
                            average_submitted_chrg_amt + average_medicare_payment_amt + 
                            practice_rev_change, 
                            model = "within", effect = "twoways", 
                            index = c("npi","Year"), data = reg.dat.2sls
                    )

mod.2sls.feols <- feols(log_y ~ average_submitted_chrg_amt + 
                          average_medicare_payment_amt  | 
                          npi + Year | int ~ practice_rev_change, 
                          data = reg.dat.2sls
                        )

mod.2sls <- modelsummary(summary(mod.2sls.feols, stage = 1:2),
                         stars = TRUE, output = "modelsummary_list")

modelsummary(mod.2sls.plm, output = "modelsummary_list", stars = TRUE)
modelsummary(mod.2sls.feols, output = "modelsummary_list", stars = TRUE)
# Both plm and feols give me the same coeff, different se


## ----Q6---------------------------------------------------------------

reg.dat.2sls <- reg.dat.2sls %>% 
                filter(!(is.na(practice_rev_change)))

reg.dat.2sls$v_hat <- feols(int ~ average_submitted_chrg_amt + 
                              average_medicare_payment_amt + 
                              practice_rev_change | 
                              npi + Year, data = reg.dat.2sls
                            )$residuals

mod.DWH <- feols(log_y ~ int + average_submitted_chrg_amt + 
                         average_medicare_payment_amt + v_hat | 
                         npi + Year, data = reg.dat.2sls)

modelsummary(mod.DWH, output = "markdown", stars = TRUE)



## ----Q7-a-------------------------------------------------------------
#perform Wald Test to determine if int predictor variables is  zero
wald.test(Sigma = vcov(mod.ols), b = coef(mod.ols), Term = 3)

wald.test(Sigma = vcov(mod.2sls.feols), b = coef(mod.2sls.feols), Term = 1)

#------
# Lee (2021)
#------

reg <- feols(int~ practice_rev_change | npi+Year, data=reg.dat.2sls)
t_1S <- reg$coefficients[['practice_rev_change']]/reg$se[['practice_rev_change']]
F_1S <- t_1S^2
print(paste0('First stage F is ',round(F_1S)))

lower <- reg$coefficients[['practice_rev_change']] - 1.96*reg$se[['practice_rev_change']]*1
# see 1S F statistics -> Find F statistics in Table3 -> Read the bottom line


## ----Q8---------------------------------------------------------------
# #-----------------------------------------------------------------------------
# ## Re-center the instrument
# #-----------------------------------------------------------------------------

allocations <- reg.dat.2sls$practice_rev_change  
conterfactuals <-  data.frame(replicate(100,sample(allocations))) # Conterfactuals 

reg.dat.2sls.uncentered <- cbind(reg.dat.2sls,                
                                conterfactuals %>%
                                #Take the mean across the conterfactuals  
                                transmute(miu = rowMeans(across(everything()), na.rm=TRUE))) %>%
  mutate(
    delta_price = practice_rev_change - miu
  )
       

## I am getting crazy numbers!

# I am not sure if I fully understood how to create the conterfactuals.... is this randomization across all physicians or is this randomization just among the physician that were affected in our 2019 base line.... not sure yet

# --- Better approach

#source("Code/conterfactuals.R)

mu <- vroom(here(dir_root,'temp','pseudoIV.csv'))

df_temp <- df_temp %>% 
  left_join(mu, by=c('Year','group1'='tax_id')) %>%
  mutate(PCcentered = PriceChange - mu)
gc()

reg <- feols(INT~PCcentered|npi+Year, data=df_temp)
df_temp$INThat <- reg$fitted.values
reg <- feols(log_claims~INThat|npi+Year, data=df_temp)
etable(reg,
       tex=T, style.tex=style.tex('aer'),
       file=here(dir_root,'tex','tab_BH.tex'),
       replace=T)
print('created tab_BH.tex')
rm(list=c('mu','piv','reg'))
gc()

# #-----------------------------------------------------------------------------
# ## 2sls using de demeaned instrument
# #-----------------------------------------------------------------------------

mod.2sls.feols.uncentered <- feols(log_y ~ average_submitted_chrg_amt + 
                          average_medicare_payment_amt  | 
                          npi + Year | int ~ delta_price, 
                          data = reg.dat.2sls.uncentered
                        )

mod_8 <- modelsummary(mod.2sls.feols.uncentered, output = "modelsummary_list")
modelsummary(mod.2sls.feols, output = "modelsummary_list")

