## ----import-data, include=TRUE, echo=TRUE-------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, vroom)

setwd("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2")
here::i_am("Main.Rmd")

read_dat <- function (folder) { # Own function to read the data and merge it in a unique data.frame
  list.files(path = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS 2/Data/", folder),
             recursive = TRUE,
             pattern = "\\.txt$|\\.csv$",
             full.names = TRUE) -> list_files #Create a vector with the path name of the txt or cvs files inside all the subfolders in the main folder. 
  if (folder == "PUF") {
    x <- lapply(list_files, vroom::vroom) ## read all the files and store them in a list. each entry of the list is a data.frame
    return(x)
  }
  else if (folder=="MDPPAS") {
    return(vroom(list_files))
  }  
  else message("Choose  either 'MDPPAS' or 'PUF'.")
}

if (!exists("dat.PUF")) { # Read PUF data
      dat.PUF <- read_dat("PUF")
      dat.PUF <- lapply(dat.PUF, function(x) x %>% select(!(starts_with("STDE") | contains("standard")))) ##  Select the commom variables in each data frame
      dat.PUF <- lapply(dat.PUF, function(x) setNames(x, tolower(names(x)))) #Set the Var names all to lower case
      dat.PUF <- lapply(dat.PUF, function(x) x %>% filter(grepl("MD|M.D.", nppes_credentials))) #filter for MD base on nppes_credentials
      dat.PUF <- data.table::rbindlist(dat.PUF) # Marge all data sets into one data.frame
}

if (!exists("dat.MDPPAS")) dat.MDPPAS <- read_dat("MDPPAS") #Read MDPPAS data
if (!exists("dat.PFS")) dat.PFS <- vroom(here("Data", "PFS.txt")) #Read PFS data


# Write Output in a .CVS file
vroom_write(dat.MDPPAS, here("Output", "dat.MDPPAS.csv"), "," )
vroom_write(dat.PUF, here("Output", "dat.PUF.csv"), "," )
vroom_write(dat.PFS, here("Output", "dat.PFS.csv"), "," )

#Save .Rdata environment
#save.image(here("Output", "Output_As2.Rdata"))
