## ----rd-sample, include=TRUE, echo=TRUE-------------------------------------------------------

# Shell code to create a random sample of the data. I am taking ~ 1% of the data. 
# EXTREMELY FAST!!!!

# brew install coreutils
# shuf -n 5349 dat.MDPPAS.csv > dat.MDPPAS.sample.csv 
# shuf -n 40151 dat.PUF.csv > dat.PUF.sample.csv

# Recall to either cd the correct path or use relative directories. 

# Load a 1% Random Sample of the total DataSets PUF and MDPPAS
read_my_sample <- function (x) {
  dir <- paste0("Output/dat.", x, ".csv")
  col_names <- readLines(here(dir), n = 1)
  tc <- textConnection(col_names)
  col_names <- scan(tc, sep = ",", what = character())
  close(tc)
  
  dir <- paste0("Output/dat.", x, ".sample.csv")
  dat <- vroom(here(dir))
  names(dat) <- col_names
  return(dat)
}

