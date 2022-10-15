#Load
dat <- vroom(here("Output", "dat.csv"))

#plot
dat  %>% 
  filter(!is.na(int)) %>%
  group_by(Year, int) %>%
  summarise(mean_claims_count = mean(Total_Claims, na.rm = TRUE)) %>%
  ggplot(aes(y=mean_claims_count , x=Year, 
             group=factor(int), color=factor(int))) +
  geom_line() +
  theme_tufte()+ 
  labs(x="Years", y="Number of Claims", 
       title = "Mean of physician-level claims for integrated versus non-integrated physicians over time") -> plot1
rm(dat)
gc()