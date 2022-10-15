## ----Q5---------------------------------------------------------------
#source(here("Code", "Instrument.R"))
price.shock <- vroom(here("Output", "instrument.csv"))

# Append the data
reg.dat.2sls <- left_join(reg.dat, price.shock, by=c("group1" = "tax_id", "Year" = "Year")) %>% filter(!is.na(int)) %>% filter(!is.na(practice_rev_change))

#mod.2sls <- ivreg(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + factor(npi) + factor(Year) | int | practice_rev_change, data = reg.dat.2sls) ## Too computational ineficient

# mod.2sls.plm <- plm(log_y ~ int + average_submitted_chrg_amt + 
#                             average_medicare_payment_amt  | 
#                             average_submitted_chrg_amt + average_medicare_payment_amt + 
#                             practice_rev_change, 
#                             model = "within", effect = "twoways", 
#                             index = c("npi","Year"), data = reg.dat.2sls
#                     )

mod.2sls.feols <- feols(log_y ~ average_submitted_chrg_amt + 
                          average_medicare_payment_amt  | 
                          npi + Year | int ~ practice_rev_change, 
                        data = reg.dat.2sls
)

# mod.2sls <- modelsummary(summary(mod.2sls.feols, stage = 1:2),
#                          stars = TRUE, output = "modelsummary_list")
# 
# modelsummary(mod.2sls.plm, output = "modelsummary_list", stars = TRUE)
# modelsummary(mod.2sls.feols, output = "modelsummary_list", stars = TRUE)
# Both plm and feols give me the same coeff, different se


rm(price.shock, reg.dat)