 rep=100
 for (i in 1:rep) {
   source('Code/instrument_centered.R')
   if (i==1) {
     mu <- piv %>% 
       mutate(practice_rev_change = practice_rev_change/rep) %>%
       rename(mu = practice_rev_change)
   } else {
     mu$mu <- mu$mu + piv$practice_rev_change/rep
     gc()
   }
   print('generated mu')
 }
 vroom_write(mu, file=here('Output','pseudoIV.csv'))