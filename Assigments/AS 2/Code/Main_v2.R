if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, vroom, ggthemes, fixest, modelsummary, robomit)

setwd("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")

#Create a character vector containing the ath to all files.
list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Data/"),
           recursive = TRUE,
            pattern = "\\.txt$|\\.csv$",
            full.names = TRUE) -> dir

j=1 # counter to index list position
list.dat.fr = list() #Create an empty list to save the dataframe
for (i in 2012:2017) {
  #Get the path for the specific year i.
  dir.mdppas = dir[(grepl(i, dir, ignore.case = TRUE) & grepl("MDPPAS", dir, ignore.case = TRUE))]
  dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & grepl("PUF", dir, ignore.case = TRUE))]

  #Read the MDPPAS and PUF data for the Year i.
  a <- vroom(dir.mdppas)
  a$npi = as.character(a$npi) #Make sure npi has the same type character on both data frames
  b <- vroom(dir.puf)
  names(b) = tolower(names(b))

  #Kepp all MD observations
  b <- b %>% filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE))

  #Store the inner join of MDPPAS and PUF in a list -> gives just the cases that we observe in both dataser
  list.dat.fr[[j]] <- inner_join(a,b, by="npi")
#  list.dat.fr[[j]] <-  list.dat.fr[[j]] %>% 
#                            slice_sample(prop=0.01)
  j = j + 1 #increase the counter fot the next list index
}


# Select Commom variables 
list.dat.fr <- lapply(list.dat.fr, function(x) x %>% select(!(starts_with("STDE") | contains("standard"))))

# Merge all data sets inside the list into 1 data frame
 dat.fr <- data.table::rbindlist(list.dat.fr)

#Remove from memory not neccesary objects
 rm(dir, dir.mdppas, dir.puf, i, j, a, b, list.dat.fr)


# Q1 ---------------------------------------------------------------------------------------------------------
  dat.fr %>% 
  ungroup() %>%
      transmute( #Create temporal variables just to speed the coding. (Check how to calculate the actual variables later.)
             Total_Spending= average_medicare_allowed_amt*line_srvc_cnt, 
             Total_Claims= line_srvc_cnt, 
             Total_Patients= bene_unique_cnt
        ) %>%
      summarise_at(c('Total_Spending', 'Total_Claims', 'Total_Patients'),
                   list(Mean = mean, Std.Dev. = sd, Min = min, Max = max), na.rm=T) %>%
      pivot_longer(cols = everything(),
                   names_to = c("colNames", ".value"), 
                   names_sep = "_",
                 names_prefix = "Total_")

 # Q2 ---------------------------------------------------------------------------------------------------------
dat.fr <- dat.fr %>%
  group_by(Year, npi) %>%
  mutate(
    int = ifelse( pos_opd / (pos_opd + pos_office + pos_asc) >= 0.75,1,0),
#    tot_claims_count = rowSums(across(starts_with("claim_")), na.rm = TRUE),
#    log_y = log(tot_claims_count)
  )


# Create plot
dat.fr  %>%  
  group_by(Year, int) %>%
  filter(!is.na(int)) %>%
  summarise(mean_claims_count = mean(line_srvc_cnt, na.rm = TRUE)) %>%
  ggplot(aes(y=mean_claims_count , x=Year, 
             group=factor(int), color=factor(int))) +
  geom_line() +
  theme_tufte()+ 
  labs(x="Years", y="Number of Claims", 
       title = "Mean of physician-level claims for integrated versus non-integrated physicians over time") -> plot1
 
 plot1

# Q3 ---------------------------------------------------------------------------------------------------------
# Drop phys that were integrated as of 2012.
dat.fr <- dat.fr %>% 
      filter(!(Year==2012 & int==1))

 reg.dat <- dat.fr %>% 
   filter(!(is.na(int))) %>%
 select(c("Year", "npi", "line_srvc_cnt", "int", "average_submitted_chrg_amt", "average_medicare_payment_amt")) %>%
 mutate(
  log_y = log(line_srvc_cnt),
  y = line_srvc_cnt
  ) %>%
  group_by(Year,npi) %>%
  summarize_all(mean, na.rm = TRUE)

mod.ols <- feols(log_y ~ average_submitted_chrg_amt + average_medicare_payment_amt + int | npi + Year, dat = reg.dat)
modelsummary(mod.ols, output = "markdown")
# create proxy for integration ratio and total claims count

# Q4 ---------------------------------------------------------------------------------------------------------




