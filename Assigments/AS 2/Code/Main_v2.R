## ----setup, include=FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(include = TRUE)
knitr::opts_chunk$set(cache = F)


## ----load-pack, include=FALSE----------------------------------------------------------------------------------------
# Import the required packages and set the working directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, sqldf, ggthemes, fixest, modelsummary, plm, GGally, ivreg)
setwd("~/Documents/GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")


## ----Merge_V2, include=TRUE, echo=TRUE-------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## Merge the data and select the variables required for the analysis.
#-----------------------------------------------------------------------------
# source(here("Code", "Merge2.R"))

# Read the merged data
dat <- vroom(here("Output", "dat.csv"))


## ----Q1, include=TRUE, echo=TRUE-------------------------------------------------------------------------------------
#table 1
  dat %>% 
        ungroup() %>%
        group_by(Year) %>%
        summarise_at(c('Total_Spending', 'Total_Claims', 'Total_Patients'),
                     list(Mean = mean, Std.Dev. = sd, Min = min, Max = max), na.rm=T) %>%
        pivot_longer(cols = everything(),
                     names_to = c("colNames", ".value"), 
                     names_sep = "_",
                     names_prefix = "Total_") -> table1
  table1


## ----Q2--------------------------------------------------------------------------------------------------------------
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


## ----Q3--------------------------------------------------------------------------------------------------------------
# Drop phys that were integrated as of 2012 and run the regression
reg.dat <- dat %>% 
               filter(!(Year==2012 & int==1)) %>%
               select(c("Year", "npi", "Total_Claims", "int", "average_submitted_chrg_amt", "average_medicare_payment_amt", "group1")) %>%
               mutate(
                        log_y = log(Total_Claims),
                        y = Total_Claims,
                        npi = as.character(npi)
                      ) #%>%
               # group_by(Year,npi) %>%
                #summarize_all(mean, na.rm = TRUE)

mod.ols <- feols(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + int | npi + Year, dat = reg.dat)
mod.fe <- modelsummary(mod.ols, output = "modelsummary_list", stars = TRUE) #store the result is a modelsummary_list to reduce memory space
rm(mod.ols)
mod.fe


## ----Q4--------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## load robomit
#-----------------------------------------------------------------------------
require(robomit)

#-----------------------------------------------------------------------------
## For loop to loop trough \rho (i) and R^2_max (j) -> interested in Delta*
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

#delta = delta_dx - rho * (delta_d - delta_dx) *[(R2_max - R2_dx)/ (R2_dx - R2_d)]

delta_star <- c()
k = 1
for (i in seq(0,2,0.5)){
  for (j in seq(0.5,1,0.1)) {
      delta_star[k] = (delta_dx - i*(delta_d - delta_dx)*((j - R2_dx)/ (R2_dx - R2_d)))
      k = k + 1
  }
}
rm(i,j,k)

interval <- cbind(delta_dx, delta_star)
interval

#----
# Clean memory and auxiliary objects
#----
rm(a, i, j)
gc()
table2

#To do: tidy the table present it in a better format.

#-----------------------------------------------------------------------------
## Nice graph Just for fun ---> grid for all 
##                                \rho (i) and R^2_max (j) combinations
#-----------------------------------------------------------------------------
k=1
plotlist <- list()
for (i in seq(0,2,0.5)){
  for (j in seq(0.5,1,0.1)) {
         plotlist[[k]] <- o_delta_boot_viz(y = "log_y",# dependent variable
                                          x = "int",# independent treatment variable
                                          con = "average_submitted_chrg_amt + average_medicare_payment_amt + npi + Year",# related control variables
                                          beta = i,# beta for which delta* should be estimated
                                          R2max = j,# maximum R-square
                                          type = "lm",# model type
                                          data = reg.dat,
                                          sim = 10,
                                          rep = FALSE,
                                          obs = 10,
                                          CI = 95,
                                          bin = 15) # dataset
          k = k + 1
            }
}
#----
# Plot the grid
#----
plot2 <- ggmatrix(
                  plotlist, nrow = 5, ncol = 6,
                  yAxisLabels = c(seq(0,2,0.5)),
                  xAxisLabels = c(seq(0.5,1,0.1)),
                  title = "Visualization of bootstrapped delta*s",
                  showStrips = FALSE
                 )
plot2
#----
# Clean memory and auxiliary objects
#----
rm(plotlist, k, i , j)
gc()

### Re do this!!!!!! program and use FEOLS. That command actually works on my machine yujuuu!


## ----Q5--------------------------------------------------------------------------------------------------------------
#source(here("Code", "Instrument.R"))

# Append the data
reg.dat.2sls <- left_join(reg.dat, price.shock, by=c("group1" = "tax_id", "Year" = "Year"))

#mod.2sls <- ivreg(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + factor(npi) + factor(Year) | int | practice_rev_change, data = reg.dat.2sls) ## Too computational ineficient

mod.2sls.plm <- plm(log_y ~ int + average_submitted_chrg_amt + average_medicare_payment_amt  | average_submitted_chrg_amt + average_medicare_payment_amt + practice_rev_change, model = "within", effect = "twoways", index = c("npi","Year"), data = reg.dat.2sls)
modelsummary(mod.2sls.plm, output = "modelsummary_list", stars = TRUE)

mod.2sls.feols <- feols(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt  | npi + Year | int ~ practice_rev_change, data = reg.dat.2sls)
mod.2sls <- modelsummary(summary(mod.2sls.feols, stage = 1:2),stars = TRUE, output = "modelsummary_list")
# Both plm and feols give me the same coeff, different se


## ----Q6--------------------------------------------------------------------------------------------------------------

reg.dat.2sls <- reg.dat.2sls %>% 
                filter(!(is.na(reg.dat.2sls$practice_rev_change) 
                       | is.na(reg.dat.2sls$int)))

reg.dat.2sls$v_hat <- feols(int ~ average_submitted_chrg_amt + average_medicare_payment_amt + practice_rev_change | npi + Year, data = reg.dat.2sls)$residuals

mod.DWH <- feols(log_y ~ int + average_submitted_chrg_amt + average_medicare_payment_amt + v_hat | npi + Year, data = reg.dat.2sls)

modelsummary(mod.DWH, output = "markdown", stars = TRUE)

