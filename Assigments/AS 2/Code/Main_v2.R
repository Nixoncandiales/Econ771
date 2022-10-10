## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(include = TRUE)
knitr::opts_chunk$set(cache = TRUE)

# Import the required packages and set the working directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, here, sqldf, ggthemes, fixest, modelsummary, plm)
setwd("~/Documents/GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")


## ----Merge_V2, include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------
# Merge the data and select the varuables required for the analisys.
# source(here("Code", "Merge2.R"))

# Append the Data and Read it.
   list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Output/Merge"),
             recursive = TRUE,
             pattern = "\\.csv$",
             full.names = TRUE) -> dir
   dat <- vroom(dir)
   rm(dir)



## ----Q1, include=TRUE, echo=TRUE-------------------------------------------------------------------------------------------------
#table 1
  dat %>% 
        ungroup() %>%
        transmute( #Create temporal variables just to speed the coding. (Check how to calculate the actual variables later)
               Total_Spending= average_medicare_allowed_amt*line_srvc_cnt, 
               Total_Claims= line_srvc_cnt, 
               Total_Patients= bene_unique_cnt
          ) %>%
        summarise_at(c('Total_Spending', 'Total_Claims', 'Total_Patients'),
                     list(Mean = mean, Std.Dev. = sd, Min = min, Max = max), na.rm=T) %>%
        pivot_longer(cols = everything(),
                     names_to = c("colNames", ".value"), 
                     names_sep = "_",
                     names_prefix = "Total_") -> table1
  table1


## ----Q2--------------------------------------------------------------------------------------------------------------------------
## Note from the documentation page 14 https://resdac.org/sites/datadocumentation.resdac.org/files/MD-PPAS%20User%20Guide%20-%20Version%202.4.pdf 
#pos_asc -> % of line items delivered in ambulatory surgery center (asc)
#pos_office ->  % of line items delivered in office
#pos_opd -> % of items delivered in hospital outpatient department (opd)

# plot
dat  %>%  
  group_by(Year, int) %>%
  summarise(mean_claims_count = mean(line_srvc_cnt, na.rm = TRUE)) %>%
  ggplot(aes(y=mean_claims_count , x=Year, 
             group=factor(int), color=factor(int))) +
  geom_line() +
  theme_tufte()+ 
  labs(x="Years", y="Number of Claims", 
       title = "Mean of physician-level claims for integrated versus non-integrated physicians over time") -> plot1
  plot1  


## ----Q3--------------------------------------------------------------------------------------------------------------------------
# Drop phys that were integrated as of 2012. Also drop observation where int=NA
reg.dat <- dat %>% 
               filter(!(Year==2012 & int==1) & !(is.na(int))) %>%
               select(c("Year", "npi", "line_srvc_cnt", "int", "average_submitted_chrg_amt", "average_medicare_payment_amt")) %>%
               mutate(
                        log_y = log(line_srvc_cnt),
                        y = line_srvc_cnt
                      ) %>%
                group_by(Year,npi) %>%
                summarize_all(mean, na.rm = TRUE)

mod.ols <- feols(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + int | npi + Year, dat = reg.dat)
modelsummary(mod.ols, output = "markdown")


## ----Q4--------------------------------------------------------------------------------------------------------------------------
# load robomit
require(robomit)

# estimate delta*
for (i in seq(0,2,0.5)){
  for (j in seq(0.5,1,0.1)) {
    print(o_delta(y = "log_y", # dependent variable
            x = "int", # independent treatment variable
            # id = "npi",
            # time = "Year",
            con = "average_submitted_chrg_amt + average_medicare_payment_amt + npi + Year", # related control variables
            beta = i, # beta
            R2max = j, # maximum R-square
            type = "lm", # model type
            data = reg.dat)) # dataset
    # estimate and visualize bootstrapped beta*s
  }
}
# Include the Fixed effects and controls

# Nice graph Just for fun.
for (i in seq(0,2,0.5)){
  for (j in seq(0.5,1,0.1)) {
o_beta_boot_viz(y = "log_y", # dependent variable
                x = "int", # independent treatment variable
                con = "average_submitted_chrg_amt + average_medicare_payment_amt +npi + Year", # related control variables
                delta = 1, # delta
                R2max = 0.9, # maximum R-square
                type = "lm", # model type
                data = reg.dat,
                sim = 10,
                rep = FALSE,
                obs = 10,
                CI = 95,
                bin = 15) # dataset
  }
}

#To do:
#how to present this results in a table... and most important what to include?
# Ask!!!

### Idea plot the coefficients and stored them in a loop!!


## ---- eval=FALSE, echo=TRUE------------------------------------------------------------------------------------------------------
##   price.shock <- medicare.puf %>% inner_join(taxid.base, by="npi") %>%
##     inner_join(pfs.yearly %>%
##                  select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007),
##                by=c("hcpcs_code"="hcpcs")) %>%
##     mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
##     mutate(price_shock = case_when(
##             i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
##             i>2013  ~ dprice_rel_2010),
##           denom = line_srvc_cnt*price_nonfac_orig_2010,
##           numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
##     group_by(npi) %>%
##     summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
##     ungroup() %>%
##     mutate(phy_rev_change=phy_numer/phy_denom) %>%
##     group_by(tax_id) %>%
##     summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
##     ungroup()


## ----Save------------------------------------------------------------------------------------------------------------------------
knitr::purl("Main.Rmd", documentation = 1, output="Code/Main_V2.R")

