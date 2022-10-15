#Q1
# Read the merged data
dat <- vroom(here("Output", "dat.csv"))


## ----Q1, include=TRUE, echo=TRUE--------------------------------------
#table 1
dat %>% 
  ungroup() %>%
  summarise_at(c('Total_Spending', 'Total_Claims', 'Total_Patients'),
               list(Mean = mean, Std.Dev. = sd, Min = min, Max = max), na.rm=T) %>%
  pivot_longer(cols = everything(),
               names_to = c("colNames", ".value"), 
               names_sep = "_",
               names_prefix = "Total_") -> table1
table1

table1 <- xtable(table1,
              align = c("c","c","c","c","c","c"),
              caption = "Summary statistics",
              label = "tab:sum_stat")
table1
rm(tab, dat)