# Pat 1 replication -------------------------------------------------------

#---------------------------------------------------------------
# Subset the data used in table 1
#---------------------------------------------------------------

dat.tab1 <- dat %>% 
    filter(yearOfPlan == 1) %>% 
    group_by(cohort)
#---------------------------------------------------------------
# Subset the data used in table 1
#---------------------------------------------------------------

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

#---------------------------------------------------------------
# Format
#---------------------------------------------------------------

# Add a dollar sign to display in latex
format.dollar <- function(x, ...) paste0( "$", unclass(x))

for (i in c(1,3)){
    tab1[i,] <- format.dollar(tab1[i,])
}

# Add a parenthesis to SE
addparentheses <- function(x){paste0("(",x,")")}
for (i in c(2,4)){
    tab1[i,] <- addparentheses(tab1[i,])
}

# Create a Kable and latex format
tab1.tex <- knitr::kable(tab1, "latex", booktabs = T, align = "c",
                   caption = "Descriptive Statistics of Medicare Part D Plans") %>%
            kableExtra::add_header_above(c(" ", "Cohort (Year of plan introduction)" = 5)) %>% 
            kableExtra::pack_rows("Fraction of plans offered by firms already offering a plan ... ", 6, 7)
#    kableExtra::footnote(general = "Plan characteristics are taken from the year the plan was introduced (e.g., premium in plan's first year). Standard deviations in parentheses.")

cat(tab1.tex, file = 'Output/tab/table1.tex') # Write the table on disk

print("Table 1 was written on Disk in latex format in Output/tab/table1.tex")
print(tab1)

#---------------------------------------------------------------
# Clear Memory
#---------------------------------------------------------------
rm(tab1, tab1.tex, addparentheses, format.dollar, i)
gc()
