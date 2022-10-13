#--------------------------------------------------------------------------------------------------------------
# Merge and Random Sample Version 1, (define a function to load, merge and sample... to costly in memory (took forever))
#--------------------------------------------------------------------------------------------------------------
#----All Data
# source(here("Code", "Merge.R")) # Just need to run it once for ever !!!!! #Version 1 of the merging code
# Function to load a 1% sample of the Data Set... to speed coding.
#----Random Sample.
# source(here("Code", "Rdsample.R")) # Works Really nice!!!!!!!
# if (!exists("dat.MDPPAS.sample")) dat.MDPPAS.sample <- read_my_sample("MDPPAS")
# if (!exists("dat.PUF.sample")) dat.PUF.sample <- read_my_sample("PUF")
# rm(read_my_sample)
# Read the Sample1 output # -----> I forgot to include year in PUF data!!! do not work with this
# load(here("Output", "As2.Rdata"))

#--------------------------------------------------------------------------------------------------------------
# Merge and Random Sample Version 2, (script does it iterative not in a function) 
#--------------------------------------------------------------------------------------------------------------

#Create a character vector containing the path to all files.
list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Data/"),
           recursive = TRUE,
           pattern = "\\.txt$|\\.csv$",
           full.names = TRUE) -> dir

#------
#----- data bases needed for the instrument, keep them out of the loop
#------

    taxid.base <- vroom(here("Data", "PhysicianData_2009.csv")) %>% 
                  select(npi, tax_id_2009 = group1)

    pfs <- vroom(here("Data", "pfs.txt")) 

#------
#----- For loop to read the data in iterations by each year, clean and append
#------

for (i in 2012:2017) {
  #Get the path for the specific year i.
  dir.mdppas = dir[(grepl(i, dir, ignore.case = TRUE) & 
                    grepl("MDPPAS", dir, ignore.case = TRUE))]
  dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & 
                 grepl("PUF", dir, ignore.case = TRUE))]
  
  #------
  #Read the MDPPAS data for the Year i. create int as in eq 1
  #------
  a <- vroom(dir.mdppas)
  a$npi = as.character(a$npi) #Make sure npi has the same type 'character' on both data frames
  a <- a %>%
    select(npi, Year, pos_asc, pos_opd, pos_office, group1, group2) %>%
    group_by(Year, npi) %>%
    mutate(
      int = ifelse( pos_opd / (pos_opd + pos_office + pos_asc) >= 0.75,1,0), #Create int variable for Q2
      tax_id = group1 + group2   
    ) %>%
    select(Year,npi,int, group1, group2)
  
  #------
  #Read the PUF data for the Year i. Keep all MD observations. Collapse to Physician level; 1 observation per \{npi ~ Year\}
  #------
  b <- vroom(dir.puf)
  names(b) = tolower(names(b))
  b <-   b %>%
            select(npi, nppes_credentials, average_medicare_allowed_amt, average_submitted_chrg_amt, 
                   average_medicare_payment_amt,line_srvc_cnt, bene_unique_cnt) %>%
            filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) %>%
            mutate( #Create temporal variables just to speed the coding. (Check how to calculate the actual variables later)
              Total_Spending= average_medicare_allowed_amt*line_srvc_cnt, 
              Total_Claims= line_srvc_cnt, 
              Total_Patients= bene_unique_cnt
            ) #%>%
#            group_by(npi) %>%
#            summarise(
#                  Total_Spending                = sum(Total_Spending, na.rm = TRUE),
#                  Total_Claims                  = sum(Total_Claims, na.rm = TRUE),
#                  Total_Patients                = sum(Total_Patients, na.rm = TRUE),
#                  average_submitted_chrg_amt    = sum(average_submitted_chrg_amt, na.rm = TRUE),
#                  average_medicare_payment_amt  = sum(average_medicare_payment_amt, na.rm = TRUE),
#                  average_medicare_allowed_amt  = sum(average_medicare_allowed_amt, na.rm = TRUE),
#                  line_srvc_cnt                 = sum(line_srvc_cnt, na.rm = TRUE),
#                  bene_unique_cnt               = sum(bene_unique_cnt, na.rm = TRUE)
#                )

  #------
  # Create the instrument for int -> price.shock
  #------
    price.shock <- b %>% inner_join(taxid.base, by="npi") %>%
                 inner_join(pfs %>% filter(year == i) %>%
                              select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007),
                            by=c("hcpcs_code"="hcpcs")) %>%
                 mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
                 mutate(price_shock = case_when(
                         i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
                         i>2013  ~ dprice_rel_2010),
                       denom = line_srvc_cnt*price_nonfac_orig_2010,
                       numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) %>%
                 group_by(npi) %>%
                 summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
                 ungroup() %>%
                 mutate(phy_rev_change=phy_numer/phy_denom) %>%
                 group_by(tax_id) %>%
                 summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
                 ungroup()

  # Write the inner join of MDPPAS and PUF in Disk in rectangular form for Year i 
  # -> gives just the cases that we observe in both data sets
  dat <- inner_join(a,b, by="npi")
  vroom_write(dat, paste0(here("Output", "Merge","dat_"),i,".csv"), delim = ",", col_names = TRUE)
}

#------
# Clean memory
#------ 

  #Remove from memory not auxiliary objects
  rm(dir, dir.mdppas, dir.puf, i, a, b, dat)
  #Clean memory
  gc()

#------
# Append all the data.frames created in the loop in rectangular form
#------
  list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Output/Merge"),
             recursive = TRUE,
             pattern = "\\.csv$",
             full.names = TRUE) -> dir
  dat <- vroom(dir)
  vroom_write(dat, file = here("Output", "dat.csv"), delim = ",")
  rm(dir, dat)
  message("MDPPAS and PUF were merged and the resulting Data.Frame is written in disk ~/Output/dat.csv")
