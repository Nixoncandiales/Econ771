## ----Q3---------------------------------------------------------------
#Load
dat <- vroom(here("Output", "dat.csv"))

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

rm(dat)