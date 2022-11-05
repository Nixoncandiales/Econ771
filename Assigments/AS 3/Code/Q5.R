# Run the test
rd.test <- rddensity(dat$LISPremium)
#Create the plot
plotQ5 <- rdplotdensity(rd.test, dat$LISPremium)

# Write in Disk
png(file="Output/fig/Q5.png",
     width=14, height=7, units="in", res=500)
  plotQ5  
  dev.off()

print("Figure Q5.png has been written in Disk on Output/fig/Q5.png")

print(summary(rdbwdensity(X = dat$LISPremium, vce="jackknife")))
print(summary(rd.test))

rd.test$bino[["LeftWindow"]] %>% round(.,2)-> window
rd.test$bino[["pval"]] %>% round(.,2) -> p_val

tab5 <- as_tibble(cbind(window, p_val))
knitr::kable(tab5, "latex", booktabs = T, align = "c",
            caption = "Rddensity Test for Different Windows")

#---------------------------------------------------------------
# Clear Memory
#---------------------------------------------------------------
rm(rd.test, plotQ5, window, p_val)
gc()

names(rd.test)
names(summary(rd.test))
