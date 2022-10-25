# Pat 1 replication -------------------------------------------------------

# Subset the data used in table 1
dat.tab1 <- dat %>% 
    filter(yearOfPlan == 1) %>% 
    group_by(cohort)

# Replicate the first 4 rows
tab1 <- dat.tab1  %>% 
    summarise(premium.mean = mean(premium), 
              premium.sd = sd(premium),
              deductible.mean = mean(deductible),
              deductible.sd = sd(deductible)) %>%
              round()

# Replicate rows 5 to 7
tab1 <- cbind(tab1, (dat.tab1  %>% 
                    summarise(benefit.mean = mean(EBene),
                            fraction.plan.US = mean(thisPlansExist_F0),
                            fraction.plan.State = mean(thisPlansExistState_F0)) %>%
                    round(2))[,2:4])

# Replicate row 8
tab1 <- cbind(tab1, (dat.tab1  %>% 
                    distinct(firmID) %>% 
                    count())[,2])
# Replicate row 9
tab1 <- cbind(tab1, (dat.tab1  %>% 
                    count())[,2])

# Transpose to give the format of table 1
tab1 <- tab1 %>% t(.)

# Set the names of the colums
colnames(tab1) <- (seq(2006,2010,1))
tab1 <- tab1[2:10,]

# Set the names of the rows
rownames(tab1) <- c("Mean monthly premium", "",
                    "Mean deductible", "",
                    "Fraction enhanced benefit",
                    " ... in the United States",
                    " ... in the same state",
                    "Number of unique firms",
                    "Number of plans")



# Delete auxiliary objects
rm(dat.tab1)
gc()

print(tab1)
print(knitr::kable(tab1))