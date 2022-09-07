
# Meta --------------------------------------------------------------------

## Title:  Medicaid Expansion
## Author: Ian McCarthy
## Date Created: 12/9/2019
## Date Edited:  12/9/2019


# Preliminaries -----------------------------------------------------------

kff.dat <- read_csv(here("Assigments", "AS 1", "Data","ACA","expansion_status.csv"))

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.final$splitvar <- kff.final %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.final %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(State, expanded, date_adopted)

write_tsv(kff.final,here("Assigments", "AS 1","Data","ACA","medicaid_expansion.txt"))
