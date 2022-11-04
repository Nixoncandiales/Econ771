#######################################
# Same as Q6 but using functions
#######################################


source("Code/my_functions.R") 

years<-2006:2010
lags<-0:4
panel_a <- list()
panel_b <- list()

# Iterative store the regressions in a list
for (i in 1:5){
    dat.temp <- My_lag(years[i],lags[i])

    t=years[i]
    list_temp <- My_reg()

    panel_a<-append(panel_a,list_temp[1])
    panel_b<-append(panel_b,list_temp[2])
}
rm(dat.temp,years,lags)

# Rename lists to years
names(panel_a) <- seq(2006,2010,1)
names(panel_b) <- seq(2006,2010,1)

# Write the tables
options("modelsummary_format_numeric_latex" = "plain")

print(modelsummary(panel_a, 
            "markdown", stars = TRUE, drop = "Int", 
            gof_map = c("nobs", "r.squared"), 
            coef_map = c("belowBench2006"="Below benchmark, 2006", 
                         "LISPremiumNeg"="Below benchmark", 
                         "LISPremiumPos"="Above benchmark"),
            title="Effect of LIS Benchmark Status in 2006 on Plan Enrollment",
            booktabs=TRUE) %>% 
            kableExtra::pack_rows("Premium—subsidy, 2006", 3, 6))

print("Table6.panel.a was written on Disk on Output/tab/tab6.a.tex")

print(modelsummary(panel_b,
          "markdown", stars = TRUE, keep = "belowBench2006",
          coef_map = c("belowBench2006"="Below benchmark, 2006"),
          gof_map = c("nobs", "r.squared"), title = "Effect of LIS Benchmark Status in 2006 on Plan Enrollment"))


print("Table6.panel.b was written on Disk on Output/tab/tab6.b.tex")

#---------------------------------------------------------------
# Clear Memory and delete aux objects
#---------------------------------------------------------------
rm(panel_a,panel_b, i, t, list_temp, My_reg, My_lag)
gc()
