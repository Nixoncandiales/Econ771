#-- -----------------------------------------------------------------------------------------------------------
# Instrument
#--------------------------------------------------------------------------------------------------------------
#------
#----- data bases needed for the instrument, keep them out of the loop
#------

#Create a character vector containing paths to all files.

list.files(path = here('Data'),
           recursive = TRUE,
           pattern = "\\.txt$|\\.csv$",
           full.names = TRUE) -> dir

message("Reading MDPPAS_2009 and PFS")
taxid.base <- vroom(here("Data", "PhysicianData_2009.csv")) %>% 
  select(npi, tax_id = group1) %>%
  mutate(npi = as.character(npi))

pfs <- vroom(here("Data", "pfs.txt")) 

price.shock <- data.frame()

#------
# Create the instrument for int -> price.shock
#------


for (i in 2012:2017){
  message(paste0("creating instrument year ", i, "..."))

            dir.puf = dir[(grepl(i, dir, ignore.case = TRUE) & 
                     grepl("PUF", dir, ignore.case = TRUE))]
            
            # Read the PUF data
            b <- vroom(dir.puf)
            names(b) = tolower(names(b))
            b <- b %>%
              select(npi, nppes_credentials, hcpcs_code, average_medicare_allowed_amt, average_submitted_chrg_amt, 
                     average_medicare_payment_amt,line_srvc_cnt, bene_unique_cnt) %>%
              filter(grepl("MD|M.D.", nppes_credentials, ignore.case = TRUE)) 
            
            pfs.year <- pfs %>% filter(year == min(i,2013))
            
            # Run Ian's snippet code tax_id is group1 from MDPPAS 2009
              price.shock.temp <- b %>% inner_join(taxid.base, by="npi") %>%
              inner_join(pfs.year %>%
                           select(hcpcs, dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007),
                           by=c("hcpcs_code"="hcpcs")) %>%
              mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
              mutate(price_shock = case_when(
                      i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
                      i>2013  ~ dprice_rel_2010
                     ),
              denom = line_srvc_cnt*price_nonfac_orig_2010,
              numer = price_shock*line_srvc_cnt*price_nonfac_orig_2010) 
              
              vroom_write(price.shock.temp, file = 
                            here('Output', 'price.shock', paste0('price.shock.temp_', i, '.csv')) , delim = ",")
              
              
            price.shock.temp <- price.shock.temp %>%
              group_by(npi) %>%
              summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(tax_id)) %>%
              ungroup() %>%
              mutate(phy_rev_change=phy_numer/phy_denom) %>%
              group_by(tax_id) %>%
              summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
              ungroup() %>%
              mutate(Year = i)
            
          price.shock <- bind_rows(price.shock, price.shock.temp)
            
}

# Remove Auxiliary object and clear memory
rm(i, dir, dir.puf, price.shock.temp, b, pfs, taxid.base)

# Clear memory
gc()
vroom_write(price.shock, path = here("Output", "instrument.csv"))
