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
# Merge and Random Sample Version 2, (does it iterative not in a function) (Take each proces and append)
#--------------------------------------------------------------------------------------------------------------

#Create a character vector containing the path to all files.
list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Data/"),
           recursive = TRUE,
           pattern = "\\.txt$|\\.csv$",
           full.names = TRUE) -> dir

#------
#----- For loop to read the data in iteration and store them in a list
#------

for (i in 2012:2017) {
  #Get the path for the specific year i.
  dir.mdppas = dir[(grepl(i, dir, ignore.case = TRUE) & grepl("MDPPAS", dir, ignore.case = TRUE))]
  dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & grepl("PUF", dir, ignore.case = TRUE))]
  
  #------
  #Read the MDPPAS data for the Year i. filter out int=NA
  #------
  a <- vroom(dir.mdppas)
  a$npi = as.character(a$npi) #Make sure npi has the same type character on both data frames
  a <- a %>%
    select(npi, Year, pos_asc, pos_opd, pos_office, group1, group2) %>%
    group_by(Year, npi) %>%
    mutate(
      int = ifelse( pos_opd / (pos_opd + pos_office + pos_asc) >= 0.75,1,0), #Create int variable for Q2
    ) %>%
    filter(!(is.na(int)))
  
  #------
  #Read the PUF data for the Year i. Keep all MD observations
  #------
  b <- vroom(dir.puf)
  names(b) = tolower(names(b))
  b <- b %>%
    select(npi, nppes_credentials, average_medicare_allowed_amt, average_submitted_chrg_amt, 
           average_medicare_payment_amt,line_srvc_cnt, bene_unique_cnt) %>%
    filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE))
  
  #Store the inner join of MDPPAS and PUF in a list -> gives just the cases that we observe in both dataser
  dat <- inner_join(a,b, by="npi")
  vroom_write(dat, paste0(here("Output", "Merge","dat_"),i,".csv"), delim = ",", col_names = TRUE)
}
#Remove from memory not necessary objects
rm(dir, dir.mdppas, dir.puf, i, j, a, b, list.dat.fr, dat)
#Clean memory
gc()