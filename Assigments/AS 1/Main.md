Assigment 1
================

## Downloading the Raw Data

We start by downloading and processing the
[HCRIS](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/HCRIS),
[POS](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/POS),
and
[ACA](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/ACA)
raw data sets. The processed data sets are located in the
[**Output**]((https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Output))
folder under `HCRIS_Data.txt`, `pos_lastyear.v12.dta`, and
`acs_medicare.txt` . We import those data sets in our file and inspect
them as follows.

``` r
ds_screener(data_hcris)
```

    -----------------------------------------------------------------------------------
    |        Column Name         |  Data Type  |  Levels  |  Missing  |  Missing (%)  |
    -----------------------------------------------------------------------------------
    |      provider_number       |   integer   |    NA    |     0     |       0       |
    |          fy_start          |  character  |    NA    |     0     |       0       |
    |           fy_end           |  character  |    NA    |     0     |       0       |
    |       date_processed       |  character  |    NA    |     0     |       0       |
    |        date_created        |  character  |    NA    |     0     |       0       |
    |            beds            |   integer   |    NA    |   2060    |     1.45      |
    |        tot_charges         |   numeric   |    NA    |   5701    |       4       |
    |       tot_discounts        |   numeric   |    NA    |   7961    |     5.59      |
    |     tot_operating_exp      |   numeric   |    NA    |   2686    |     1.88      |
    |         ip_charges         |   numeric   |    NA    |   6283    |     4.41      |
    |        icu_charges         |   numeric   |    NA    |   66719   |     46.82     |
    |     ancillary_charges      |   numeric   |    NA    |   12677   |      8.9      |
    |       tot_discharges       |   numeric   |    NA    |   2300    |     1.61      |
    |      mcare_discharges      |   numeric   |    NA    |   3002    |     2.11      |
    |      mcaid_discharges      |   numeric   |    NA    |   17059   |     11.97     |
    |     tot_mcare_payment      |   numeric   |    NA    |   55548   |     38.98     |
    |  secondary_mcare_payment   |   numeric   |    NA    |   78080   |     54.79     |
    |           street           |  character  |    NA    |    713    |      0.5      |
    |            city            |  character  |    NA    |    286    |      0.2      |
    |           state            |  character  |    NA    |    284    |      0.2      |
    |            zip             |  character  |    NA    |    320    |     0.22      |
    |           county           |  character  |    NA    |   6756    |     4.74      |
    |        uncomp_care         |   numeric   |    NA    |  121914   |     85.55     |
    |       cost_to_charge       |   numeric   |    NA    |   50563   |     35.48     |
    |        new_cap_ass         |   numeric   |    NA    |   25149   |     17.65     |
    |            cash            |   numeric   |    NA    |   10786   |     7.57      |
    |        tot_pat_rev         |   numeric   |    NA    |   5701    |       4       |
    |         allowance          |   numeric   |    NA    |   7961    |     5.59      |
    |        net_pat_rev         |   numeric   |    NA    |   5685    |     3.99      |
    |        hvbp_payment        |   numeric   |    NA    |  113346   |     79.54     |
    |        hrrp_payment        |   numeric   |    NA    |  116285   |     81.6      |
    |  tot_uncomp_care_charges   |   numeric   |    NA    |   95493   |     67.01     |
    |tot_uncomp_care_partial_pmts|   numeric   |    NA    |  117193   |     82.24     |
    |          bad_debt          |   numeric   |    NA    |   93693   |     65.75     |
    |            year            |   integer   |    NA    |     0     |       0       |
    |           source           |  character  |    NA    |     0     |       0       |
    -----------------------------------------------------------------------------------

     Overall Missing Values           1032204 
     Percentage of Missing Values     20.12 %
     Rows with Missing Values         142504 
     Columns With Missing Values      29 

After a quick screening of the HCRIS data we can see the missing values
are significantly high which suggest some variables are recorded
differently across time and forms. It is of particular interest the
variables `uncomp_care` and `tot_uncomp_care_charges` which are of our
main interest. After reviewing the codebook we confirmed in fact these
two variables are the same but coded different across forms.

``` r
ds_screener(data_pos)
```

    --------------------------------------------------------------------------------------------
    |  Column Name   |            Data Type             |  Levels  |  Missing  |  Missing (%)  |
    --------------------------------------------------------------------------------------------
    |       pn       |            character             |    NA    |     0     |       0       |
    |      year      |             numeric              |    NA    |     0     |       0       |
    |      name      |            character             |    NA    |     0     |       0       |
    |    address     |            character             |    NA    |     0     |       0       |
    |      city      |            character             |    NA    |     0     |       0       |
    |     state      |            character             |    NA    |     0     |       0       |
    |      zip       |             numeric              |    NA    |   6650    |     2.48      |
    |      tel       |            character             |    NA    |     0     |       0       |
    |     active     |             numeric              |    NA    |     0     |       0       |
    |    termcode    |haven_labelled, vctrs_vctr, double|    NA    |     0     |       0       |
    |    termdate    |               Date               |    NA    |  178452   |     66.42     |
    |    partdate    |               Date               |    NA    |   18421   |     6.86      |
    |    prev_pn     |            character             |    NA    |     0     |       0       |
    |    medaffil    |haven_labelled, vctrs_vctr, double|    NA    |   27485   |     10.23     |
    |  resprog_ada   |             numeric              |    NA    |   18802   |       7       |
    |  resprog_ama   |             numeric              |    NA    |   18802   |       7       |
    |  resprog_aoa   |             numeric              |    NA    |   18802   |       7       |
    |  resprog_oth   |             numeric              |    NA    |   18802   |       7       |
    |   residents    |             numeric              |    NA    |    414    |     0.15      |
    |   shortterm    |             numeric              |    NA    |     0     |       0       |
    |      cah       |             numeric              |    NA    |     0     |       0       |
    |provider_subtype|haven_labelled, vctrs_vctr, double|    NA    |   14723   |     5.48      |
    |  typ_control   |haven_labelled, vctrs_vctr, double|    NA    |   27452   |     10.22     |
    |   nonprofit    |             numeric              |    NA    |     0     |       0       |
    |   forprofit    |             numeric              |    NA    |     0     |       0       |
    |      govt      |             numeric              |    NA    |     0     |       0       |
    |    maryland    |             numeric              |    NA    |     0     |       0       |
    |    nonstate    |             numeric              |    NA    |     0     |       0       |
    |   urbancbsa    |             numeric              |    NA    |  184738   |     68.76     |
    |    beds_tot    |             numeric              |    NA    |    411    |     0.15      |
    |   beds_cert    |             numeric              |    NA    |    411    |     0.15      |
    --------------------------------------------------------------------------------------------

     Overall Missing Values           534365 
     Percentage of Missing Values     6.42 %
     Rows with Missing Values         234603 
     Columns With Missing Values      14 

From the provider of services data set we do not evidence missing data
problems. We can observe if a particular POS went out of the market by
either closing or merging and the respectively date of the event. It is
to note the identifier variable is `pn` which is recorded as a character
differs in the HCRIS data set `provider_number` which is coded as
numerical.

``` r
ds_screener(data_aca)
```

    ----------------------------------------------------------------------
    |  Column Name  |  Data Type  |  Levels  |  Missing  |  Missing (%)  |
    ----------------------------------------------------------------------
    |     State     |  character  |    NA    |     0     |       0       |
    |     year      |   integer   |    NA    |     0     |       0       |
    |   adult_pop   |   integer   |    NA    |     0     |       0       |
    | ins_employer  |   integer   |    NA    |     0     |       0       |
    |  ins_direct   |   integer   |    NA    |     0     |       0       |
    | ins_medicare  |   integer   |    NA    |     0     |       0       |
    | ins_medicaid  |   integer   |    NA    |     0     |       0       |
    |   uninsured   |   integer   |    NA    |     0     |       0       |
    |  expand_ever  |   logical   |    NA    |     8     |     1.92      |
    | date_adopted  |  character  |    NA    |    104    |      25       |
    |  expand_year  |   integer   |    NA    |    104    |      25       |
    |    expand     |   logical   |    NA    |     0     |       0       |
    ----------------------------------------------------------------------

     Overall Missing Values           216 
     Percentage of Missing Values     4.33 %
     Rows with Missing Values         104 
     Columns With Missing Values      3 

Finally, from the medicare data set we see the states that expanded the
mandate and the date of event. Also, it is to note that the state
identifier is not recorded in the same format across data sets.

## Merging the data

We start by left joining `HCRIS_data.txt` and `pos_lastyear.v12.dta`.
The key to merge these two data set is the indicator `pn`

``` r
#Merged the two data sets
df_1 <- data_hcris %>% #sum up the two variables uncompensated care variables
              filter(year >= 2003 & year <= 2019) %>%
              rowwise() %>% 
              mutate(hosp_rev = tot_pat_rev/1000000,
                     tot_uncomp_care_partial_pmts = tot_uncomp_care_partial_pmts * - 1,
                     tot_unc_care_v2010 = sum(tot_uncomp_care_charges, tot_uncomp_care_partial_pmts, bad_debt, na.rm=TRUE), #Correct way to calculate UNC_CARE after 2010.
                     unc_care = sum(tot_unc_care_v2010 ,uncomp_care, na.rm=TRUE)/1000000) %>%
              mutate_at(c('unc_care'), ~na_if(., 0)) %>%
              select(pn=provider_number, year, unc_care, hosp_rev, state)  %>%
              filter(!(is.na(unc_care) & is.na(hosp_rev))) #discard the observations NA observation for both unc_care and hosp_rev
          
df_2 <- data_pos %>% #coherce PN as integer and discard those facilities that are not hospitals
              filter(year >= 2003) %>%
              mutate_at('pn', as.integer) %>% 
              select(pn = pn, nonprofit, forprofit, active, State=state, year) %>% 
              mutate(own_typ = case_when(nonprofit == 0  & forprofit == 0  ~ 'other',
                                         nonprofit == 0  & forprofit == 1  ~ 'forprofit',
                                         nonprofit == 1  & forprofit == 0  ~ 'nonprofit')) %>%
              distinct(pn, own_typ, State, year, active)


df <- left_join(df_1, df_2, by='pn', 'year') %>%
              filter(!(unc_care == 'NA')) %>% # drop all observations that don't contain uncompensated care information
              mutate(state= coalesce(State, state), year= year.x) %>%
              distinct(pn, state, year, own_typ, unc_care, hosp_rev)
## I do not fully understand the behavior of left_join it creates multiple row, so I am just taking the unique values per PN and YEAR

df 
```

    # A tibble: 79,613 × 6
    # Rowwise: 
          pn unc_care hosp_rev state own_typ  year
       <int>    <dbl>    <dbl> <chr> <chr>   <int>
     1 10001     41.3     532. AL    other    2003
     2 10001     37.4     592. AL    other    2004
     3 10001     37.5     658. AL    other    2005
     4 10001     41.7     714. AL    other    2006
     5 10001     90.8    1117. AL    other    2010
     6 10001    109.     1208. AL    other    2011
     7 10001    119.     1263. AL    other    2012
     8 10001    116.     1306. AL    other    2013
     9 10001    129.     1451. AL    other    2014
    10 10001    111.     1551. AL    other    2015
    # … with 79,603 more rows

``` r
df_3 <- data_aca %>% # crosswalk the states names to states abbreviations and drop Puerto Rico from the analysis
              #filter(!(State=='Puerto Rico')) %>%
              mutate(state= encodefrom(., State, stcrosswalk, stname, stfips, stabbr)) %>%
              select(!State) %>%
              relocate(state) #make sure the ID variable has the same name on both data sets
```

    Warning in class(val_vec) <- class(cw[[clean]]): NAs introduced by coercion

``` r
df <- left_join(df,# %>% filter(!(state=="PR")), 
                df_3, by=c('state', 'year')) %>% # Filtering out PR since is not in df_3, to avoid future NA
  relocate(pn, year, state, own_typ ,expand_ever, expand, expand_year, unc_care)
```

## Summary Statistics

Provide and discuss a table of simple summary statistics showing the
mean, standard deviation, min, and max of hospital total revenues and
uncompensated care over time.

From the `HCRIS_data.txt` we select the variables `provider_number`,
`year`, `uncomp_care`, `tot_uncomp_care_charges`, `tot_pat_rev`. We
create a new variable that stores the uncompensated care records, then
we group by year and calculate the summary statistics as follows.

``` r
df %>% ungroup() %>%
  summarise_at(c("unc_care", "hosp_rev"), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)
```

    # A tibble: 1 × 8
      unc_care_mean hosp_rev_mean unc_care…¹ hosp_…² unc_c…³ hosp_…⁴ unc_c…⁵ hosp_…⁶
              <dbl>         <dbl>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    1          28.2          546.       124.    960.   -97.3   -177.  20406.  22001.
    # … with abbreviated variable names ¹​unc_care_sd, ²​hosp_rev_sd, ³​unc_care_min,
    #   ⁴​hosp_rev_min, ⁵​unc_care_max, ⁶​hosp_rev_max

``` r
df_1 %>%
  group_by(year) %>%
  summarise_at(c('unc_care', 'hosp_rev'),list(mean = mean, sd = sd, min = min, max = max), na.rm=T) 
```

    # A tibble: 17 × 9
        year unc_care_mean hosp_…¹ unc_c…² hosp_…³ unc_ca…⁴ hosp_r…⁵ unc_c…⁶ hosp_…⁷
       <int>         <dbl>   <dbl>   <dbl>   <dbl>    <dbl>    <dbl>   <dbl>   <dbl>
     1  2003          13.6    196.    32.0    339. -1.28e-1 -1.76e+0    778.   4723.
     2  2004          15.3    217.    36.7    379.  1   e-6  1.54e-1    820.   5526.
     3  2005          17.4    237.    37.8    419.  1   e-6  1   e-6    939.   6399.
     4  2006          21.0    262.    47.2    464. -2.67e+0 -1.04e-1   1075.   7784.
     5  2007          23.6    286.    51.3    508.  1   e-6  6.36e-2   1203.   8577.
     6  2008          26.4    311.    57.1    556.  1   e-6  4   e-6   1362.   9294.
     7  2009          27.4    342.    46.4    613.  1   e-6  1.19e-1    584.   9846.
     8  2010          29.9    365.    72.4    648.  1   e-6  3.07e-1   2794.   9858.
     9  2011          26.8    394.    63.1    712. -5.43e+1 -2.76e+1   2060.  10572.
    10  2012          29.8    418.    72.5    766. -7.44e+0 -1.18e+1   1883.  11865.
    11  2013          31.9    446.    72.6    834. -4.50e+0  9.49e-2   1653.  12752.
    12  2014          31.8    478.    77.4    905. -2.59e+1  6.62e-3   2025.  13376.
    13  2015          29.8    518.    74.7    971. -3.36e-2  9.37e-3   2054.  14144.
    14  2016          35.5    562.   310.    1070. -1.90e-2 -1.77e+2  20406.  15619.
    15  2017          33.4    603.    87.3   1168. -2.80e-2  1.25e-1   2734.  16863.
    16  2018          35.9    652.    90.5   1284. -6.41e-2  2.83e-1   2606.  18677.
    17  2019          39.8    706.    99.5   1420. -9.73e+1  3   e-6   2648.  22001.
    # … with abbreviated variable names ¹​hosp_rev_mean, ²​unc_care_sd, ³​hosp_rev_sd,
    #   ⁴​unc_care_min, ⁵​hosp_rev_min, ⁶​unc_care_max, ⁷​hosp_rev_max

``` r
df %>% filter(!(pn==151327 & year ==2016)) %>% ggplot(aes(x = year, y = unc_care, group=year)) +
  geom_boxplot() + 
  theme_tufte() +
    labs(x="Years", y="Uncompensated Care Millions", 
       title = "Distribution Hospital Uncompensated Care Over Time")-> plot1

df %>% ggplot(aes(x = year, y = hosp_rev, group=year)) + 
  geom_boxplot() + 
  theme_tufte() +
    labs(x="Years", y="Hospital Revenue Millions", 
       title = "Distribution Hospital Total Revenue Over Time") -> plot2

plot <- plot1  / plot2 
plot
```

![](Main_files/figure-gfm/plot-summary-stats-1.png)<!-- -->

``` r
### Evidence of extreme Outliers, are those misstipying> should I removed them? Ask.
```

## By Ownership Type

Create a figure showing the mean hospital uncompensated care from 2000
to 2018. Show this trend separately by hospital ownership type (private
not for profit and private for profit).

``` r
df %>%  filter(!(pn==151327 & year ==2016)) %>%
  filter(!(own_typ=='other')) %>%
  group_by(year, own_typ) %>%
  summarise_at(c('unc_care'), list(unc_care_mean = mean), na.rm=T) %>%
  ggplot(aes(x=year, y=unc_care_mean, color=own_typ)) +
  geom_point(size = 3) +
  #geom_line(size = 1) +
  geom_smooth(aes(fill = own_typ), size = 1) +
  geom_vline( xintercept = 2014, color="black") +
  theme_tufte()+ 
  labs(x="Years", y="Total Uncompensated Care", 
       title = "Mean of Hospital Uncompensated Care in Millions of Dollars by Ownership Type", 
       fill = "Ownership type", color = "Ownership type") -> plot3

plot3
```

![](Main_files/figure-gfm/plot-own-typ-1.png)<!-- -->

## DiD identification strategy

Using a simple DD identification strategy, estimate the effect of
Medicaid expansion on hospital uncompensated care using a traditional
two-way fixed effects (TWFE) estimation: $$
y_{it} = \alpha_{i} + \gamma_{t} + \delta D_{it} + \varepsilon_{it},
$$ where $D_{it}=1(E_{i}\leq t)$ in Equation 1 is an indicator set to 1
when a hospital is in a state that expanded as of year $t$ or earlier,
$\gamma_{t}$ denotes time fixed effects, $\alpha_{i}$ denotes hospital
fixed effects, and $y_{it}$ denotes the hospital’s amount of
uncompensated care in year $t$. Present four estimates from this
estimation in a table: one based on the full sample (regardless of
treatment timing); one when limiting to the 2014 treatment group (with
never treated as the control group); one when limiting to the 2015
treatment group (with never treated as the control group); and one when
limiting to the 2016 treatment group (with never treated as the control
group). Briefly explain any differences.

``` r
#Create dummies for the control groups
df %>% filter(!(pn==151327 & year ==2016) & unc_care > 0) %>%
  mutate(d = case_when(expand == TRUE ~ 1),
         d_14 = case_when((expand == TRUE & expand_year==2014) ~ 1),
         d_15 = case_when((expand == TRUE & expand_year==2015) ~ 1),
         d_16 = case_when((expand == TRUE & expand_year==2016) ~ 1)) %>%
  mutate(across(d:d_16, ~ifelse(is.na(.),0,.))) -> df
```

``` r
mod.twfe <- lapply(df %>% 
                select(d:d_16), #Select the treatments 
              function(Treatment) felm(unc_care ~ Treatment | pn + year | 0 | pn, df)) #Apply the specification across the different treatments and store the results in a list

stargazer(mod.twfe, type='text', note="1-4 representes d, d_14,d_15 and d_16 respectevely")
```


    ============================================================================
                                                 Dependent variable:            
                                     -------------------------------------------
                                                      unc_care                  
                                        (1)        (2)        (3)        (4)    
    ----------------------------------------------------------------------------
    Treatment                        -28.191*** -26.243*** -12.003*** -12.424***
                                      (1.883)    (1.795)    (1.811)    (1.543)  
                                                                                
    ----------------------------------------------------------------------------
    Observations                       79,557     79,557     79,557     79,557  
    R2                                 0.699      0.697      0.690      0.690   
    Adjusted R2                        0.675      0.673      0.665      0.665   
    Residual Std. Error (df = 73725)   39.701     39.829     40.304     40.313  
    ============================================================================
    Note:                                            *p<0.1; **p<0.05; ***p<0.01

    ==================================================
    1-4 representes d, d_14,d_15 and d_16 respectevely
    --------------------------------------------------

## Event Study

Estimate an “event study” version of the specification in part 3: $$
y_{it} = \alpha_{i} + \gamma_{t} +\sum_{\tau < -1} D_{it}^{\tau} \delta_{\tau} + \sum_{\tau>=0} D_{it}^{\tau} \delta_{\tau} + \varepsilon_{it},
$$ where $D_{it}^{\tau} = 1(t-E_{i}=\tau)$ in Equation 2 is essentially
an interaction between the treatment dummy and a relative time dummy. In
this notation and context, $\tau$ denotes years relative to Medicaid
expansion, so that $\tau=-1$ denotes the year before a state expanded
Medicaid, $\tau=0$ denotes the year of expansion, etc. Estimate with two
different samples: one based on the full sample and one based only on
those that expanded in 2014 (with never treated as the control group).

``` r
### Common treatment timing
dat.reg <- df %>% group_by(state) %>% 
            mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
            fill(starts_with("exp"), .direction = "up") %>%  # Fill the NA for the years that do not appear in the aca data set.
            mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
                   expand=ifelse(!is.na(expand),expand,FALSE),
                   treated=ifelse(expand_ever==TRUE,1,0),
                   post_treat=ifelse(expand==TRUE,1,0),
                   D = treated*post_treat) %>% 
            ungroup()

mod.esct <- feols(unc_care~i(year, treated, ref=2013) | pn + year,
               cluster=~pn,
               data=dat.reg)
esttable(mod.esct)
```

                                    mod.esct
    Dependent Var.:                 unc_care
                                            
    treated x year = 2003   19.49*** (3.164)
    treated x year = 2004   18.19*** (2.908)
    treated x year = 2005   17.17*** (2.965)
    treated x year = 2006   19.12*** (3.292)
    treated x year = 2007   18.78*** (3.478)
    treated x year = 2008   15.39*** (3.328)
    treated x year = 2009   12.43*** (2.711)
    treated x year = 2010   11.29*** (2.717)
    treated x year = 2011   4.376*** (1.106)
    treated x year = 2012     1.371 (0.8733)
    treated x year = 2014 -9.102*** (0.9868)
    treated x year = 2015  -15.89*** (1.431)
    treated x year = 2016  -19.50*** (1.585)
    treated x year = 2017  -25.38*** (1.959)
    treated x year = 2018  -29.14*** (2.155)
    treated x year = 2019  -35.39*** (2.603)
    Fixed-Effects:        ------------------
    pn                                   Yes
    year                                 Yes
    _____________________ __________________
    S.E.: Clustered                   by: pn
    Observations                      79,557
    R2                               0.70237
    Within R2                        0.04160
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
iplot(mod.esct)
```

![](Main_files/figure-gfm/event-study-common-1.png)<!-- -->

``` r
##### Differential timgin treatment
dat.reg <- df %>% group_by(state) %>% 
            mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
            fill(starts_with("exp"), .direction = "up") %>%  # Fill the NA for the years that do not appear in the aca data set.
            mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
                   expand=ifelse(!is.na(expand),expand,FALSE),
                   treated=ifelse(expand_ever==TRUE,1,0),
                   post_treat=ifelse(expand==TRUE,1,0),
                   dif_timing = ifelse(expand_ever==FALSE, 0, ifelse(expand_year==0,0,year-expand_year)),
                   time_to_treat = ifelse(dif_timing < -7, -7, dif_timing),
                   D = treated*post_treat) %>% 
            ungroup()

mod.esdt <- feols(unc_care~i(time_to_treat, treated, ref=-1) | pn + year,
                  cluster=~pn,
                  data=dat.reg)

modelsummary(mod.esdt, stars=TRUE)
```

|                              |    Model 1    |
|:-----------------------------|:-------------:|
| time_to_treat = -7 × treated |  5.137\*\*\*  |
|                              |    (1.412)    |
| time_to_treat = -6 × treated |   4.920\*\*   |
|                              |    (1.502)    |
| time_to_treat = -5 × treated |     2.254     |
|                              |    (1.551)    |
| time_to_treat = -4 × treated |    -1.602     |
|                              |    (1.308)    |
| time_to_treat = -3 × treated |   -2.416\*    |
|                              |    (1.027)    |
| time_to_treat = -2 × treated |    -0.141     |
|                              |    (0.578)    |
| time_to_treat = 0 × treated  |  8.151\*\*\*  |
|                              |    (1.599)    |
| time_to_treat = 1 × treated  | -16.830\*\*\* |
|                              |    (1.009)    |
| time_to_treat = 2 × treated  | -22.370\*\*\* |
|                              |    (1.256)    |
| time_to_treat = 3 × treated  | -27.793\*\*\* |
|                              |    (1.537)    |
| time_to_treat = 4 × treated  | -30.231\*\*\* |
|                              |    (1.738)    |
| time_to_treat = 5 × treated  | -33.524\*\*\* |
|                              |    (2.060)    |
| Num.Obs.                     |     79557     |
| AIC                          |   805085.8    |
| BIC                          |   805206.5    |
| RMSE                         |     38.12     |
| Std.Errors                   |    by: pn     |
| FE: pn                       |       X       |
| FE: year                     |       X       |

**Note:** ^^ + p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001

``` r
esttable(mod.esdt)
```

                                          mod.esdt
    Dependent Var.:                       unc_care
                                                  
    treated x time_to_treat = -7  5.137*** (1.412)
    treated x time_to_treat = -6   4.920** (1.502)
    treated x time_to_treat = -5     2.254 (1.551)
    treated x time_to_treat = -4    -1.602 (1.308)
    treated x time_to_treat = -3   -2.416* (1.027)
    treated x time_to_treat = -2  -0.1407 (0.5782)
    treated x time_to_treat = 0   8.151*** (1.599)
    treated x time_to_treat = 1  -16.83*** (1.009)
    treated x time_to_treat = 2  -22.37*** (1.256)
    treated x time_to_treat = 3  -27.79*** (1.537)
    treated x time_to_treat = 4  -30.23*** (1.738)
    treated x time_to_treat = 5  -33.52*** (2.060)
    Fixed-Effects:               -----------------
    pn                                         Yes
    year                                       Yes
    ____________________________ _________________
    S.E.: Clustered                         by: pn
    Observations                            79,557
    R2                                     0.70060
    Within R2                              0.03589
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
iplot(mod.esdt)
```

![](Main_files/figure-gfm/event-study-diff-1.png)<!-- -->

## SA specification

Sun and Abraham (SA) show that the $\delta_{\tau}$ coefficients in
Equation 2 can be written as a non-convex average of all other
group-time specific average treatment effects. They propose an
interaction weighted specification: $$
y_{it} = \alpha_{i} + \gamma_{t} +\sum_{e} \sum_{\tau \neq -1} \left(D_{it}^{\tau} \times 1(E_{i}=e)\right) \delta_{e, \tau} + \varepsilon_{it}.
$$ Re-estimate your event study using the SA specification in Equation
3. Show your results for $\hat{\delta}_{e, \tau}$ in a Table, focusing
on states with $E_{i}=2014$, $E_{i}=2015$, and $E_{i}=2016$.

``` r
reg.dat <- df %>% 
            group_by(state) %>% 
            mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
            fill(starts_with("exp"), .direction = "up")  %>%  # Fill the NA for the years that do not appear in the aca data set.
            mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
                   expand=ifelse(!is.na(expand),expand,FALSE))

sa <- function(data, i){
  
  data %>%
      mutate(post = (year>=i), 
      treat=post*expand_ever,
      expand_year = ifelse(expand_ever==FALSE, 10000, ifelse(expand_year>=i,expand_year,100000)),
      time_to_treat = ifelse(expand_ever==FALSE, -1, year-expand_year),
      time_to_treat = ifelse(time_to_treat < ifelse(i==2014,-5,
                                                    ifelse(i==2015, -6 , -7 )), 
                                             ifelse(i==2014,-5, ifelse(i==2015, -6 , -7 )) , 
                             time_to_treat)) -> x

   feols(unc_care~sunab(expand_year, time_to_treat) | pn + year,
                      cluster=~pn,
                      data=x)
}


mod.sa <- list(
  "mod.sa.2016" = sa(reg.dat, 2016),
  "mod.sa.2015" = sa(reg.dat, 2015),
  "mod.sa.2014" = sa(reg.dat, 2014)
)

modelsummary(mod.sa, stars = TRUE, output = "markdown")
```

|                    |  mod.sa.2016  |  mod.sa.2015  |  mod.sa.2014  |
|:-------------------|:-------------:|:-------------:|:-------------:|
| time_to_treat = -7 |  -5.298\*\*   |               |               |
|                    |    (1.806)    |               |               |
| time_to_treat = -6 |  -6.820\*\*   |  -6.301\*\*   |               |
|                    |    (2.306)    |    (2.025)    |               |
| time_to_treat = -5 |    -1.181     |    -1.023     |    -4.815+    |
|                    |    (3.792)    |    (3.790)    |    (2.575)    |
| time_to_treat = -4 |  -4.281\*\*   |  -4.591\*\*   | -7.985\*\*\*  |
|                    |    (1.474)    |    (5.908)    |    (1.733)    |
| time_to_treat = -3 |    -2.801     |  -4.501\*\*   | -6.371\*\*\*  |
|                    |    (2.155)    |    (2.097)    |    (1.529)    |
| time_to_treat = -2 |    -0.286     |   -3.207\*    | -6.684\*\*\*  |
|                    |    (2.473)    |    (1.384)    |    (1.369)    |
| time_to_treat = 0  |    -2.493     |  -5.394\*\*   | -16.855\*\*\* |
|                    |    (2.524)    |    (1.278)    |    (1.673)    |
| time_to_treat = 1  | -15.240\*\*\* | -12.623\*\*\* | -26.405\*\*\* |
|                    |    (1.783)    |    (1.921)    |    (1.890)    |
| time_to_treat = 2  | -19.497\*\*\* | -16.957\*\*\* | -30.859\*\*\* |
|                    |    (1.880)    |    (2.156)    |    (2.158)    |
| time_to_treat = 3  | -23.461\*\*\* | -20.513\*\*\* | -35.758\*\*\* |
|                    |    (2.161)    |    (2.128)    |    (2.431)    |
| time_to_treat = 4  |               | -19.194\*\*\* | -38.103\*\*\* |
|                    |               |    (5.484)    |    (2.538)    |
| time_to_treat = 5  |               |               | -42.934\*\*\* |
|                    |               |               |    (3.003)    |
| Num.Obs.           |     79557     |     79557     |     79557     |
| AIC                |   807862.7    |   807729.3    |   804897.4    |
| BIC                |   807964.8    |   807831.4    |   804999.5    |
| RMSE               |     38.79     |     38.76     |     38.07     |
| Std.Errors         |    by: pn     |    by: pn     |    by: pn     |
| FE: pn             |       X       |       X       |       X       |
| FE: year           |       X       |       X       |       X       |

**Note:** ^^ + p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001

## Event Study - SA specification

Present an event study graph based on the results in part 5. Hint: you
can do this automatically in `R` with the `fixest` package (using the
`sunab` syntax for interactions), or with `eventstudyinteract` in
`Stata`. These packages help to avoid mistakes compared to doing the
tables/figures manually and also help to get the standard errors
correct.

``` r
coefplot(mod.sa, main="Effect of Medicaid Eaxpansion on Uncompensated Care")
```

![](Main_files/figure-gfm/sa-plot-1.png)<!-- -->

## Callaway and Sant’Anna Specification (CS)

Callaway and Sant’Anna (CS) offer a non-parametric solution that
effectively calculates a set of group-time specific differences,
$ATT(g,t)= E[y_{it}(g) - y_{it}(\infty) | G_{i}=g]$, where $g$ reflects
treatment timing and $t$ denotes time. They show that under the standard
DD assumptions of parallel trends and no anticipation,
$ATT(g,t) = E[y_{it} - y_{i, g-1} | G_{i}=g] - E[y_{it} - y_{i,g-1} | G_{i} = \infty]$,
so that $\hat{ATT}(g,t)$ is directly estimable from sample analogs. CS
also propose aggregations of $\hat{ATT}(g,t)$ to form an overall ATT or
a time-specific ATT (e.g., ATTs for $\tau$ periods before/after
treatment). With this framework in mind, provide an alternative event
study using the CS estimator. Hint: check out the `did` package in `R`
or the `csdid` package in `Stata`.

``` r
reg.dat <- df%>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
  filter(!is.na(unc_care)) %>%
  group_by(state) %>%
  mutate(state_id=cur_group_id()) %>% ungroup()


mod.cs <- att_gt(yname="unc_care", 
                 tname="year", 
                 idname="state_id",
                 gname="expand_year",
                 data=reg.dat, 
                 panel=TRUE, 
                 est_method="dr",
                 #xformula= xformula,
                 cband=TRUE,
                 bstrap=TRUE,
                 allow_unbalanced_panel=TRUE,
                 base_period="universal")
                 #control_group="nevertreated")
mod.cs.event <- aggte(mod.cs, type="dynamic", min_e = -5, max_e = 5)

mod.cs
```


    Call:
    att_gt(yname = "unc_care", tname = "year", idname = "state_id", 
        gname = "expand_year", data = reg.dat, panel = TRUE, allow_unbalanced_panel = TRUE, 
        bstrap = TRUE, cband = TRUE, est_method = "dr", base_period = "universal")

    Reference: Callaway, Brantly and Pedro H.C. Sant'Anna.  "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230, 2021. <https://doi.org/10.1016/j.jeconom.2020.12.001>, <https://arxiv.org/abs/1803.09015> 

    Group-Time Average Treatment Effects:
     Group Time ATT(g,t) Std. Error [95% Simult.  Conf. Band]  
      2014 2012   0.5009     0.8127       -1.4418      2.4436  
      2014 2013   0.0000         NA            NA          NA  
      2014 2014 -10.9816     2.2766      -16.4240     -5.5393 *
      2014 2015 -19.8272     3.7070      -28.6890    -10.9655 *
      2014 2016 -22.7935     4.3441      -33.1784    -12.4086 *
      2014 2017 -27.6899     5.2573      -40.2578    -15.1219 *
      2014 2018 -30.2118     6.0017      -44.5593    -15.8643 *
      2014 2019 -35.7919     8.0876      -55.1258    -16.4580 *
      2015 2012   6.2440     2.3231        0.6905     11.7975 *
      2015 2013   5.7813     1.8514        1.3555     10.2071 *
      2015 2014   0.0000         NA            NA          NA  
      2015 2015  -5.4012     2.5340      -11.4589      0.6565  
      2015 2016  -9.5221     2.2307      -14.8546     -4.1895 *
      2015 2017 -16.5219     3.1497      -24.0514     -8.9923 *
      2015 2018 -20.6733     4.2786      -30.9016    -10.4450 *
      2015 2019 -25.5337     6.5736      -41.2482     -9.8193 *
      2016 2012   3.5234     4.7777       -7.8980     14.9447  
      2016 2013   2.5444     4.4038       -7.9830     13.0719  
      2016 2014   0.8790     1.5625       -2.8562      4.6141  
      2016 2015   0.0000         NA            NA          NA  
      2016 2016  -4.7519     1.2388       -7.7134     -1.7904 *
      2016 2017 -18.2490     4.8737      -29.8998     -6.5982 *
      2016 2018 -23.5455     6.0848      -38.0915     -8.9995 *
      2016 2019 -31.1904     7.6463      -49.4692    -12.9116 *
      2019 2012  -2.7114    12.2955      -32.1043     26.6815  
      2019 2013  -4.6228    12.0827      -33.5072     24.2615  
      2019 2014  -3.9781     9.4739      -26.6261     18.6699  
      2019 2015  -3.7113     7.0840      -20.6460     13.2234  
      2019 2016  -0.4415     5.0423      -12.4953     11.6124  
      2019 2017  -3.3307     3.5299      -11.7692      5.1078  
      2019 2018   0.0000         NA            NA          NA  
      2019 2019 -15.2665     3.3710      -23.3252     -7.2079 *
    ---
    Signif. codes: `*' confidence band does not cover 0

    Control Group:  Never Treated,  Anticipation Periods:  0
    Estimation Method:  Doubly Robust

``` r
mod.cs.event
```


    Call:
    aggte(MP = mod.cs, type = "dynamic", min_e = -5, max_e = 5)

    Reference: Callaway, Brantly and Pedro H.C. Sant'Anna.  "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230, 2021. <https://doi.org/10.1016/j.jeconom.2020.12.001>, <https://arxiv.org/abs/1803.09015> 


    Overall summary of ATT's based on event-study/dynamic aggregation:  
          ATT    Std. Error     [ 95%  Conf. Int.]  
     -24.0294        4.8022   -33.4416    -14.6172 *


    Dynamic Effects:
     Event time Estimate Std. Error [95% Simult.  Conf. Band]  
             -5  -3.9781     8.9294      -23.8477     15.8915  
             -4  -0.0940     4.8787      -10.9501     10.7622  
             -3   3.2769     2.9594       -3.3083      9.8620  
             -2   0.7637     0.8664       -1.1642      2.6916  
             -1   0.0000         NA            NA          NA  
              0 -10.3749     2.1342      -15.1238     -5.6259 *
              1 -18.7625     3.4905      -26.5295    -10.9955 *
              2 -22.2526     4.0857      -31.3440    -13.1611 *
              3 -27.2509     5.1321      -38.6708    -15.8310 *
              4 -29.7440     6.0708      -43.2527    -16.2353 *
              5 -35.7919     8.5935      -54.9140    -16.6697 *
    ---
    Signif. codes: `*' confidence band does not cover 0

    Control Group:  Never Treated,  Anticipation Periods:  0
    Estimation Method:  Doubly Robust

``` r
ggdid(mod.cs)
```

![](Main_files/figure-gfm/cs-1.png)<!-- -->

``` r
ggdid(mod.cs.event, 
      title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group")
```

![](Main_files/figure-gfm/cs-2.png)<!-- -->

``` r
CS_never_cond <- mod.cs #id::att_gt(yname="unc_care",
                  #           tname="year",
                  #           idname="state_id",
                  #           gname="expand_year",
                  #           xformla=~1,
                  #           #xformla = xformla,
                  #           control_group="nevertreated",
                  #           data = reg.dat,
                  #           panel = TRUE,
                  #           base_period="universal",
                  #           bstrap = TRUE,
                  #           cband = TRUE,
                  #           allow_unbalanced_panel=TRUE)

# Now, compute event study
CS_es_never_cond <- mod.cs.event #aggte(CS_never_cond, type = "dynamic",
                          #min_e = -5, max_e = 5)
#summary(CS_es_never_cond)
# Plot event study
fig_CS <- ggdid(CS_es_never_cond,
      title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group ")

fig_CS
```

![](Main_files/figure-gfm/cs-3.png)<!-- -->

## Rambachan and Roth (RR)

Rambachan and Roth (RR) show that traditional tests of parallel
pre-trends may be underpowered, and they provide an alternative
estimator that essentially bounds the treatment effects by the size of
an assumed violation in parallel trends. One such bound RR propose is to
limit the post-treatment violation of parallel trends to be no worse
than some multiple of the pre-treatment violation of parallel trends.
Assuming linear trends, such a violation is reflected by

$$ 
\Delta(\bar{M}) = { \delta : \forall t \geq 0, \lvert (\delta_{t+1} - \delta_{t}) - (\delta_{t} - \delta_{t-1}) \rvert \leq \bar{M} \times \max_{s<0} \lvert (\delta_{s+1} - \delta_{s}) - (\delta_{s} - \delta_{s-1}) \rvert }.
$$

Using the `HonestDiD` package in `R` or `Stata`, present a sensitivity
plot of your CS ATT estimates using $\bar{M} = \{0, 0.5, 1, 1.5, 2\}$.
Check out the GitHub repo [here](https://github.com/pedrohcgs/CS_RR) for
some help in combining the `HonestDiD` package with CS estimates. 1

``` r
# Install some packages
library(devtools)
install_github("bcallaway11/BMisc", dependencies = TRUE)
```

    Skipping install of 'BMisc' from a github remote, the SHA1 (70e7b615) has not changed since last install.
      Use `force = TRUE` to force installation

``` r
install_github("bcallaway11/did", dependencies = TRUE)
```

    Skipping install of 'did' from a github remote, the SHA1 (df953008) has not changed since last install.
      Use `force = TRUE` to force installation

``` r
install_github("asheshrambachan/HonestDiD", dependencies = TRUE)
```

    Skipping install of 'HonestDiD' from a github remote, the SHA1 (99b7e30c) has not changed since last install.
      Use `force = TRUE` to force installation

``` r
#--------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------
# Libraries
# Load libraries
library(ggplot2)
library(here)
library(foreign)
library(tidyverse)
library(dplyr)
library(did)
library(HonestDiD)

## -----------------------------------------------------------------------------

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#' @param es an event study
honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}


#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}

# Load data used in Callaway and Sant'Anna (2021) application
min_wage <- mpdta

# Formula for covariates 
#xformla <- ~ region + (medinc + pop ) + I(pop^2) + I(medinc^2)  + white + hs  + pov
#---------------------------------------------------------------------------
# Using covariates and DR DiD with never-treated as comparison group
# Fix the reference time periods
CS_never_cond <- did::att_gt(yname="lemp",
                             tname="year",
                             idname="countyreal",
                             gname="first.treat",
                             xformla=~1,
                             #xformla = xformla,
                             control_group="nevertreated",
                             data = min_wage,
                             panel = TRUE,
                             base_period="universal",
                             bstrap = TRUE,
                             cband = TRUE)
# Now, compute event study
CS_es_never_cond <- aggte(CS_never_cond, type = "dynamic",
                          min_e = -5, max_e = 5)
#summary(CS_es_never_cond)
# Plot event study
fig_CS <- ggdid(CS_es_never_cond,
      title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group ")

fig_CS
```

![](Main_files/figure-gfm/Aux-func-RR-1.png)<!-- -->

``` r
# code for running honest_did
hd_cs_smooth_never <- honest_did(CS_es_never_cond,
                           type="smoothness")
hd_cs_smooth_never
```

    $robust_ci
    # A tibble: 10 × 5
            lb       ub method Delta         M
         <dbl>    <dbl> <chr>  <chr>     <dbl>
     1 -0.0482 -0.00296 FLCI   DeltaSD 0      
     2 -0.0462  0.00630 FLCI   DeltaSD 0.00649
     3 -0.0524  0.0125  FLCI   DeltaSD 0.0130 
     4 -0.0588  0.0190  FLCI   DeltaSD 0.0195 
     5 -0.0653  0.0255  FLCI   DeltaSD 0.0260 
     6 -0.0718  0.0320  FLCI   DeltaSD 0.0325 
     7 -0.0783  0.0385  FLCI   DeltaSD 0.0390 
     8 -0.0848  0.0450  FLCI   DeltaSD 0.0454 
     9 -0.0913  0.0514  FLCI   DeltaSD 0.0519 
    10 -0.0978  0.0579  FLCI   DeltaSD 0.0584 

    $orig_ci
    # A tibble: 1 × 4
       lb[,1]  ub[,1] method   Delta
        <dbl>   <dbl> <chr>    <lgl>
    1 -0.0431 0.00325 Original NA   

    $type
    [1] "smoothness"

``` r
hd_cs_rm_never <- honest_did(CS_es_never_cond, type="relative_magnitude")
hd_cs_rm_never
```

    $robust_ci
    # A tibble: 10 × 5
            lb      ub method Delta    Mbar
         <dbl>   <dbl> <chr>  <chr>   <dbl>
     1 -0.0303 -0.0101 C-LF   DeltaRM 0    
     2 -0.0303 -0.0101 C-LF   DeltaRM 0.222
     3 -0.0505  0.0101 C-LF   DeltaRM 0.444
     4 -0.0505  0.0101 C-LF   DeltaRM 0.667
     5 -0.0505  0.0303 C-LF   DeltaRM 0.889
     6 -0.0707  0.0303 C-LF   DeltaRM 1.11 
     7 -0.0707  0.0505 C-LF   DeltaRM 1.33 
     8 -0.0909  0.0505 C-LF   DeltaRM 1.56 
     9 -0.0909  0.0707 C-LF   DeltaRM 1.78 
    10 -0.111   0.0707 C-LF   DeltaRM 2    

    $orig_ci
    # A tibble: 1 × 4
       lb[,1]  ub[,1] method   Delta
        <dbl>   <dbl> <chr>    <lgl>
    1 -0.0431 0.00325 Original NA   

    $type
    [1] "relative_magnitude"

``` r
# Drop 0 as that is not really allowed.
hd_cs_rm_never$robust_ci <- hd_cs_rm_never$robust_ci[-1,]

## -----------------------------------------------------------------------------
# make sensitivity analysis plots
cs_HDiD_smooth <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                      hd_cs_smooth_never$orig_ci)


cs_HDiD_relmag <- createSensitivityPlot_relativeMagnitudes(hd_cs_rm_never$robust_ci,
                                         hd_cs_rm_never$orig_ci)
```

``` r
cs_HDiD_smooth
```

![](Main_files/figure-gfm/RR-1.png)<!-- -->

``` r
cs_HDiD_relmag
```

![](Main_files/figure-gfm/RR-2.png)<!-- -->

## Discussion

Discuss your findings and compare estimates from different estimators
(e.g., are your results sensitive to different specifications or
estimators? Are your results sensitive to violation of parallel trends
assumptions?).

``` r
## Write results, tables plots etc to use in the PDF file.
## Save plots
#ggsave(here("plots","cs_HDiD_smooth.png"),
#       cs_HDiD_smooth,  
#       dpi = 500,
#       width = 14, 
#       height = 7)


# Save plots
#ggsave(here("plots","cs_HDiD_relmag.png"),
#       cs_HDiD_relmag,  
#       dpi = 500,
#       width = 14, 
#       height = 7)
```

## Reflections

Reflect on this assignment. What did you find most challenging? What did
you find most surprising?
