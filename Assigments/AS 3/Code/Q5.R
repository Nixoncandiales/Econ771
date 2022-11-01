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

summary(rdbwdensity(X = dat$LISPremium, vce="jackknife"))
