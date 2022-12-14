#---------------------------------------------------------------
# Write Table 8 
#---------------------------------------------------------------
df.tmp <- dat  %>%
            filter(benefit == "B")

mod <- feols(lnPremium ~  1 | state + year | lnS ~ LIS , data = df.tmp, cluster="firmID")

#---------------------------------------------------------------
# Format Table 8 
#---------------------------------------------------------------
tab8.tex <- modelsummary(mod, "latex", vcov = ~firmID, 
             estimate ="{estimate}{stars}", coef_omit = "Intercept",
             gof_map = c("nobs", "r.squared"))

cat(tab8.tex, file = 'Output/tab/table8.tex') # Write the table on disk
print("table 8 was written on Disk on 'Output/tab/table8.tex'")

#---------------------------------------------------------------
# Recreate Table A4 ... But I do not think it is necessary.
#---------------------------------------------------------------
### I can definitely write this chunk nicer annd more efficiently... 
### Not sure however is this is needed. I think no.

dat.tmp <- dat %>% filter( benefit=="B")
a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
b <- dat.tmp %>% filter(year==2007) %>% select(uniqueID, LISPremium)
df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
mod <- rdrobust(df.tmp$LISPremium.y, df.tmp$LISPremium.x, fuzzy=df.tmp$LIS)
b <- mod[["Estimate"]][1] %>% round(.,3)
ci_l <- mod[["ci"]][2,1] %>% round(.,3)
ci_u <- mod[["ci"]][2,2] %>% round(.,3)
ci <- paste0("[",ci_l,",",ci_u, "]")
n <- dim(df.tmp)[1]
c1 = rbind(b,ci,n)

dat.tmp <- dat %>% filter( benefit=="B")
a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
b <- dat.tmp %>% filter(year==2008) %>% select(uniqueID, LISPremium)
df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
mod <- rdrobust(df.tmp$LISPremium.y, df.tmp$LISPremium.x, fuzzy=df.tmp$LIS)
b <- mod[["Estimate"]][1] %>% round(.,3)
ci_l <- mod[["ci"]][2,1] %>% round(.,3)
ci_u <- mod[["ci"]][2,2] %>% round(.,3)
ci <- paste0("[",ci_l,",",ci_u, "]")
n <- dim(df.tmp)[1]
c2 = rbind(b,ci,n)

dat.tmp <- dat %>% filter( benefit=="B")
a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
b <- dat.tmp %>% filter(year==2009) %>% select(uniqueID, LISPremium)
df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
mod <- rdrobust(df.tmp$LISPremium.y, df.tmp$LISPremium.x, fuzzy=df.tmp$LIS)
b <- mod[["Estimate"]][1] %>% round(.,3)
ci_l <- mod[["ci"]][2,1] %>% round(.,3)
ci_u <- mod[["ci"]][2,2] %>% round(.,3)
ci <- paste0("[",ci_l,",",ci_u, "]")
n <- dim(df.tmp)[1]
c3 = rbind(b,ci,n)

dat.tmp <- dat %>% filter( benefit=="B")
a <- dat.tmp %>% filter(year==2006) %>% select(uniqueID, LISPremium, LIS)
b <- dat.tmp %>% filter(year==2010) %>% select(uniqueID, LISPremium)
df.tmp <- drop_na(inner_join(a,b, by="uniqueID"))
mod <- rdrobust(df.tmp$LISPremium.y, df.tmp$LISPremium.x, fuzzy=df.tmp$LIS)
b <- mod[["Estimate"]][1] %>% round(.,3)
ci_l <- mod[["ci"]][2,1] %>% round(.,3)
ci_u <- mod[["ci"]][2,2] %>% round(.,3)
ci <- paste0("[",ci_l,",",ci_u, "]")
n <- dim(df.tmp)[1]
c4 = rbind(b,ci,n)

tab82 <- as_tibble(cbind(c1,c2,c3,c4))
names(tab82) <- seq(2007,2010,1)
tab82 <- knitr::kable(tab82, "latex", booktabs = T, align = "lccc",
                   caption = " Effect of LIS Benchmark Status in 2006 on Premiums in Later Year")

cat(tab82, file = 'Output/tab/table8.2.tex') # Write the table on disk
print("Table 8.2 was written in Disk on 'Output/tab/table8.2.tex'")

#---------------------------------------------------------------
# Clear Memory and delete aux objects 
#---------------------------------------------------------------
rm(dat.tmp, a, b, mod, ci_l, ci_u, n, ci, c1, c2, c3, c4, tab82, df.tmp)
gc()          

