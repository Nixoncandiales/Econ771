##### Example Graph

df %>%
  filter(!(own_typ=='other')) %>%
  group_by(year, own_typ) %>%
  summarise_at(c('unc_care'), list(unc_care_mean = mean), na.rm=T) -> data.plot2


ggplot(data = data.plot2, 
       aes(x=year, y=unc_care_mean, color=own_typ, 
           group=own_typ, line=own_typ)) +
  geom_line() +
  geom_point(size = 1) +
  #geom_smooth(aes(fill = own_typ), size = 1) +
  theme_tufte() + 
  geom_vline( xintercept = 2014, color="black") +
  geom_text(data = data.plot2 %>% filter(year==2016),
            aes(label = c("Forprofit", "Nonprofit"), 
                x = year + 1,
                y = unc_care_mean)) +
  guides(linetype="none") +
  labs(x="Years", y="Total Uncompensated Care", title = "Mean of Hospital Uncompensated Care by Ownership Type")-> prueba

prueba


 ####################### Event Study

## Fill the data 

df <- df %>% group_by(state) %>% mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>% fill(starts_with("exp"), .direction = "up") 


# filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever))

df %>% group_by(state) %>% mutate(prueba = replace(case_when()))

modelsummary(feols(unc_care~ i() | state + year,
                   cluster=~state,
                   data=df))

reg.dat <- mcaid.data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
mod.twfe <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.dat)

esttable( feols(unc_care ~ i(year, d, ref=2014) | state + year, data=df, cluster = ~state))



##### Diferential timing treatment


reg.dat <- df %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat = post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -3, -3, time_to_treat))
mod.twfe <- feols(unc_care~i(time_to_treat, expand_ever, ref=-1) | state + year,
                  cluster=~state,
                  data=reg.dat)
esttable(mod.twfe)


### Common treatment timing

reg.dat <- df %>% 
  #filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever)
mod.twfe <- feols(unc_care~i(year, expand_ever, ref=2013) | state + year,
                  cluster=~state,
                  data=reg.dat)
esttable(mod.twfe)
