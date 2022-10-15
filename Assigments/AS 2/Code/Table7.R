#Table7

# residualize
df_temp <- reg.dat.2sls %>% mutate(PriceChange = practice_rev_change)

reg <- feols(log_y ~ 1 | npi+Year, data=df_temp)
df_temp$Yres <- reg$residuals
reg <- feols(int ~ 1 | npi+Year, data=df_temp)
df_temp$Dres <- reg$residuals
reg <- feols(PriceChange ~ 1 | npi+Year, data=df_temp)
df_temp$Zres <- reg$residuals

# Anderson-Rubin CI
reg <- ivmodel(Y=df_temp$Yres, D=df_temp$Dres, Z=df_temp$Zres)
AR <- AR.test(reg)
CI_AR <- AR$ci

# Lee (2021) tF
reg <- feols(int~PriceChange | npi+Year, data=df_temp)
F_1S <- (reg$coefficients[['PriceChange']]/reg$se[['PriceChange']])^2
print(paste0('First stage F is ',round(F_1S)))
cf <- 1  # correction factor: the third line of Table 3
df_temp$INThat <- reg$fitted.values
reg <- feols(log_y~INThat | npi+Year, data=df_temp)
lower <- reg$coefficients[['INThat']]-1.96*reg$se[['INThat']]*cf
upper <- reg$coefficients[['INThat']]+1.96*reg$se[['INThat']]*cf
CI_Lee <- data.frame('lower'=lower,'upper'=upper)

tab <- rbind(CI_AR, CI_Lee)
row.names(tab) <- c('Anderson-Rubin','Lee')

table7 <- xtable(tab,
                 align = c("c","c","c"),
                 caption = "Confidence intervals",
                 label = "tab:ci")
rm(df_temp, reg, tab, CI_Lee, upper, lower, cf, F_1S, CI_AR, AR)