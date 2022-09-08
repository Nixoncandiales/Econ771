data1 <- data_hcris %>%
  select(pn=provider_number, year=year, unc_care=tot_uncomp_care_charges, hos_rev=tot_pat_rev) %>%
  drop_na(unc_care) %>%
  as_tibble()

data2 <-
  data_pos %>% 
  select(pn, nonprofit, forprofit, govt) %>%
  mutate_at('pn', as.integer) %>%
  as_tibble()

data_merged <- 
  left_join(data1, data2, by="pn")

data_merged %>%
  filter(year<=2018, nonprofit==1) %>%
  select(pn, year, unc_care, nonprofit) %>%
  group_by(year) %>%
  summarise(Mean = mean(unc_care, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Mean)) + 
  geom_line() -> plot3
plot3

data_merged %>%
  filter(year<=2018, forprofit==1) %>%
  select(pn, year, unc_care, nonprofit) %>%
  group_by(year) %>%
  summarise(Mean = mean(unc_care, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Mean)) + 
  geom_line() -> plot4
plot4