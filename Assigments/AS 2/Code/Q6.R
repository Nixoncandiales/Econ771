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
