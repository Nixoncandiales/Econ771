df.tmp <- dat  %>%
  filter(benefit == "B")

mod <- feols(lnPremium ~  Dyear2 + Dyear3 + Dyear4 + Dyear5 | lnS ~ LIS , data = df.tmp)

tab8.tex <- modelsummary(mod, "latex", vcov = ~firmID, 
             estimate ="{estimate}{stars}", coef_omit = "Intercept",
             gof_map = c("nobs", "r.squared"))

cat(tab8.tex, file = 'Output/tab/table8.tex') # Write the table on disk
print("table 8 was written on Disk on 'Output/tab/table8.tex'")
### Recreate fig 4 ... But i do not think it is necessary.
#dat.tmp <- dat %>% filter( benefit=="B")
#a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
#b <- dat.tmp %>% filter(Dyear2==1) %>% select(uniqueID, lnPremium)

#df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
#mod <- rdrobust(df.tmp$lnPremium, df.tmp$LISPremium, fuzzy=df.tmp$LIS)
#b <- mod[["Estimate"]][1] %>% round(.,3)
#ci_l <- mod[["ci"]][2,1] %>% round(.,3)
#ci_u <- mod[["ci"]][2,2] %>% round(.,3)
#ci <- paste0("[",ci_l,",",ci_u, "]")
#n <- dim(df.tmp)[1]
#rbind(b,ci,n)

#---------------------------------------------------------------
# Clear Memory and delete aux objects 
#---------------------------------------------------------------
gc()          

