df.tmp <- dat  %>%
  filter(benefit == "B")

mod <- ivreg(lnPremium ~ lnS | LIS + Dyear2 + Dyear3 + Dyear4 + Dyear5 , data = df.tmp)

modelsummary(mod, vcov = ~firmID, 
             estimate ="{estimate}{stars}", coef_omit = "Intercept",
             gof_map = c("nobs", "r.squared"))

dat.tmp <- dat %>% filter( benefit=="B")
a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
b <- dat.tmp %>% filter(Dyear2==1) %>% select(uniqueID, lnPremium)

df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
mod <- rdrobust(df.tmp$lnPremium, df.tmp$LISPremium, fuzzy=df.tmp$LIS)
b <- mod[["Estimate"]][1] %>% round(.,3)
ci_l <- mod[["ci"]][2,1] %>% round(.,3)
ci_u <- mod[["ci"]][2,2] %>% round(.,3)
ci <- paste0("[",ci_l,",",ci_u, "]")
n <- dim(df.tmp)[1]

rbind(b,ci,n)

#---------------------------------------------------------------
# Clear Memory and delete aux objects 
#---------------------------------------------------------------
rm()
gc()          

library(ivreg)
