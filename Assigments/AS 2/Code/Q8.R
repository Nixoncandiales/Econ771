# --- Better approach

#source("Code/conterfactuals.R)
#Will call the updated version of Instruments.R and instrument_centered.R
#Do the shuflle and simulations... took a bit on my pc!

mu <- vroom(here('Output','pseudoIV.csv'))
df_temp <- reg.dat.2sls %>% filter(!is.na(int)) %>% filter(!is.na(practice_rev_change))
df_temp <- df_temp %>% 
  left_join(mu, by=c('Year','group1'='tax_id')) %>%
  mutate(PCcentered = practice_rev_change - mu)
gc()

reg <- feols(int~PCcentered|npi+Year, data=df_temp)
df_temp$INThat <- reg$fitted.values
reg <- feols(log_y~INThat|npi+Year, data=df_temp)
tab8 <- etable(reg,
               tex=T, style.tex=style.tex('aer'))
print('created tab8')
rm(list=c('mu','piv','reg'))
gc()