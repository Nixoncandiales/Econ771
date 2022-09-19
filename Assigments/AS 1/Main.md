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
    |      name      |            character             |    NA    |     0     |       0       |
    |    address     |            character             |    NA    |     0     |       0       |
    |      city      |            character             |    NA    |     0     |       0       |
    |     state      |            character             |    NA    |     0     |       0       |
    |      zip       |             numeric              |    NA    |    270    |     2.13      |
    |      tel       |            character             |    NA    |     0     |       0       |
    |     active     |             numeric              |    NA    |     0     |       0       |
    |    termcode    |haven_labelled, vctrs_vctr, double|    NA    |     0     |       0       |
    |    termdate    |               Date               |    NA    |   7239    |     57.04     |
    |    partdate    |               Date               |    NA    |    65     |     0.51      |
    |    prev_pn     |            character             |    NA    |     0     |       0       |
    |    medaffil    |haven_labelled, vctrs_vctr, double|    NA    |   1348    |     10.62     |
    |  resprog_ada   |             numeric              |    NA    |    86     |     0.68      |
    |  resprog_ama   |             numeric              |    NA    |    86     |     0.68      |
    |  resprog_aoa   |             numeric              |    NA    |    86     |     0.68      |
    |  resprog_oth   |             numeric              |    NA    |    86     |     0.68      |
    |   residents    |             numeric              |    NA    |     0     |       0       |
    |   shortterm    |             numeric              |    NA    |     0     |       0       |
    |      cah       |             numeric              |    NA    |     0     |       0       |
    |provider_subtype|haven_labelled, vctrs_vctr, double|    NA    |   1071    |     8.44      |
    |  typ_control   |haven_labelled, vctrs_vctr, double|    NA    |   1345    |     10.6      |
    |   nonprofit    |             numeric              |    NA    |     0     |       0       |
    |   forprofit    |             numeric              |    NA    |     0     |       0       |
    |      govt      |             numeric              |    NA    |     0     |       0       |
    |    maryland    |             numeric              |    NA    |     0     |       0       |
    |    nonstate    |             numeric              |    NA    |     0     |       0       |
    |   urbancbsa    |             numeric              |    NA    |    466    |     3.67      |
    |    beds_tot    |             numeric              |    NA    |     0     |       0       |
    |   beds_cert    |             numeric              |    NA    |     0     |       0       |
    |    lastyear    |             numeric              |    NA    |     0     |       0       |
    --------------------------------------------------------------------------------------------

     Overall Missing Values           12148 
     Percentage of Missing Values     3.09 %
     Rows with Missing Values         7581 
     Columns With Missing Values      11 

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
              mutate(hosp_rev = tot_pat_rev, 
                     unc_care = sum(tot_uncomp_care_charges,uncomp_care, na.rm=TRUE)) %>%
              mutate_at(c('unc_care'), ~na_if(., 0)) %>%
              select(pn=provider_number, year, unc_care, hosp_rev )  %>%
              filter(!(is.na(unc_care) & is.na(hosp_rev))) #discard the observations NA observation for both unc_care and hosp_rev
          
df_2 <- data_pos %>% #force pn as integer and discard those facilities that are not hospitals
              mutate_at('pn', as.integer) %>% 
              select(pn = pn, nonprofit, forprofit, active, state) %>% 
              mutate(own_typ = case_when(nonprofit == 0  & forprofit == 0  ~ 'other',
                                         nonprofit == 0  & forprofit == 1  ~ 'forprofit',
                                         nonprofit == 1  & forprofit == 0  ~ 'nonprofit')) #%>%
             # filter((own_typ == 'forprofit') | (own_typ == 'nonprofit' )) #only include in the analysis hospitals forprofit and nonprofit

df <- left_join(df_1, df_2, by='pn') %>%
              mutate_at('own_typ',  replace_na, 'other') %>%
              relocate(pn, year, state, own_typ) %>%
              filter(!(unc_care == 'NA')) # drop all observations not contain uncompensated care information
df
```

    # A tibble: 60,051 × 9
    # Rowwise: 
          pn  year state own_typ unc_care   hosp_rev nonprofit forprofit active
       <int> <int> <chr> <chr>      <dbl>      <dbl>     <dbl>     <dbl>  <dbl>
     1 10001  2003 AL    other   41267219  532023593         0         0      1
     2 10001  2004 AL    other   37413733  592438087         0         0      1
     3 10001  2005 AL    other   37457443  657842984         0         0      1
     4 10001  2006 AL    other   41670968  714123644         0         0      1
     5 10001  2010 AL    other   90806676 1116894148         0         0      1
     6 10001  2011 AL    other   22446946 1208331516         0         0      1
     7 10001  2012 AL    other   25683016 1263055782         0         0      1
     8 10001  2013 AL    other   23652954 1305720014         0         0      1
     9 10001  2014 AL    other   24962490 1451185686         0         0      1
    10 10001  2015 AL    other   20412518 1550672017         0         0      1
    # … with 60,041 more rows

``` r
df_3 <- data_aca %>% # crosswalk the states names to states abbreviations and drop Puerto Rico from the analysis
              filter(!(State=='Puerto Rico')) %>%
              mutate(state= encodefrom(., State, stcrosswalk, stname, stfips, stabbr)) %>%
              select(!State) %>%
              relocate(state) #make sure the ID variable has the same name on both data sets

df <- left_join(df %>% filter(!(state=="PR")), df_3, by=c('state', 'year')) %>% # Filtering out PR since is not in df_3, to avoid future NA
  relocate(pn, year, state, own_typ ,expand_ever, expand, expand_year, unc_care)

df <- df %>% group_by(state) %>% 
            mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
            fill(starts_with("exp"), .direction = "up") %>%  # Fill the NA for the years that do not appear in the aca data set.
            mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
                   expand=ifelse(is.na(expand),FALSE,expand),
                   treated=ifelse(expand_ever==FALSE,0,1),
                   post_treat=ifelse(expand==FALSE,0,1),
                   D = treated*post_treat) %>% 
            ungroup()
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
df_1 %>%
  group_by(year) %>%
  summarise_at(c('unc_care', 'hosp_rev'),list(mean = mean, sd = sd, min = min, max = max), na.rm=T) %>%
  knitr::kable()
```

| year | unc_care_mean | hosp_rev_mean | unc_care_sd | hosp_rev_sd | unc_care_min | hosp_rev_min | unc_care_max | hosp_rev_max |
|-----:|--------------:|--------------:|------------:|------------:|-------------:|-------------:|-------------:|-------------:|
| 2003 |      13557293 |     196326204 |    32036098 |   339256130 |      -128490 |     -1757898 |    777987403 |   4722758791 |
| 2004 |      15328897 |     217080321 |    36661491 |   379301539 |            1 |       154394 |    820253000 |   5525730727 |
| 2005 |      17409739 |     237498725 |    37813838 |   419216031 |            1 |            1 |    939134000 |   6398553843 |
| 2006 |      20958801 |     262155653 |    47151668 |   464190671 |     -2667140 |      -104189 |   1074625000 |   7784094716 |
| 2007 |      23563868 |     285967064 |    51279558 |   508039587 |            1 |        63650 |   1203374820 |   8577046126 |
| 2008 |      26429603 |     311240216 |    57062599 |   555733346 |            1 |            4 |   1361805561 |   9293788259 |
| 2009 |      27437058 |     341918436 |    46417931 |   613209280 |            1 |       119236 |    583975318 |   9846464732 |
| 2010 |      29887574 |     365195409 |    72408993 |   647958858 |            1 |       306861 |   2793923000 |   9857534601 |
| 2011 |      17394154 |     393805140 |    47222987 |   712227455 |    -28840406 |    -27582223 |   1111027264 |  10572291195 |
| 2012 |      18338225 |     417753037 |    55879179 |   765536407 |           85 |    -11799711 |   1371421445 |  11865320139 |
| 2013 |      19648564 |     446296883 |    57646114 |   833905151 |          216 |        94880 |   1403146636 |  12751708196 |
| 2014 |      19607345 |     478119813 |    63262016 |   905191126 |           15 |         6624 |   1874409188 |  13376352387 |
| 2015 |      19024979 |     517619678 |    61755917 |   970877276 |           22 |         9368 |   1990560423 |  14143533186 |
| 2016 |      19810030 |     562218133 |    66724247 |  1070376474 |           84 |   -177031923 |   2231833221 |  15618749067 |
| 2017 |      22135100 |     603003321 |    69491982 |  1167558591 |           34 |       124513 |   2062118188 |  16863431079 |
| 2018 |      24883218 |     651712556 |    74503094 |  1283839130 |            1 |       282914 |   2183167185 |  18677245214 |
| 2019 |      28705587 |     706457120 |    83757685 |  1419791246 |            2 |            3 |   2495183582 |  22000932119 |

``` r
df %>% ggplot(aes(x = year, y = unc_care, group=year)) + 
  geom_boxplot() + 
  theme_bw() -> plot1

df %>% ggplot(aes(x = year, y = hosp_rev, group=year)) + 
  geom_boxplot() + 
  theme_bw()  -> plot2

plot1  / plot2 
```

![](Main_files/figure-gfm/plot-summary-stats-1.png)<!-- -->

## By Ownership Type

Create a figure showing the mean hospital uncompensated care from 2000
to 2018. Show this trend separately by hospital ownership type (private
not for profit and private for profit).

``` r
df %>%
  filter(!(own_typ=='other')) %>%
  group_by(year, own_typ) %>%
  summarise_at(c('unc_care'), list(unc_care_mean = mean), na.rm=T) %>%
  ggplot(aes(x=year, y=unc_care_mean, color=own_typ)) +
  geom_point(size = 1) +
  geom_smooth(aes(fill = own_typ), size = 1) +
  theme_bw() -> plot3

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
df %>% 
  mutate(d = case_when(expand == TRUE ~ 1),
         d_14 = case_when(expand == TRUE & expand_year==2014 ~ 1),
         d_15 = case_when(expand == TRUE & expand_year==2015 ~ 1),
         d_16 = case_when(expand == TRUE & expand_year==2016 ~ 1)) %>%
  mutate(across(d:d_16, ~ifelse(is.na(.),0,.))) -> df
```

``` r
mod.twfe <- lapply(df %>%
                select(d:d_16), #Select the treatments 
              function(D) felm(unc_care ~ D | pn + year | 0 | state, df)) #Apply the specification across the different treatments and store the results in a list
```

``` r
#Another Way of achiving the model
#prueba <- map(df %>%
#                   select(d:d_16),  # Select the treatments
#                function(D){
#                    felm(unc_care ~ D | pn + year | 0 | pn, df) #Apply the specification across the different treatments and store the results in a list
#                  })
stargazer(mod.twfe, type='text', note="1-4 representes d, d_14,d_15 and d_16 respectevely")
```


    ============================================================================================================
                                                                 Dependent variable:                            
                                     ---------------------------------------------------------------------------
                                                                      unc_care                                  
                                            (1)                (2)                (3)                (4)        
    ------------------------------------------------------------------------------------------------------------
    D                                -23,201,895.000*** -23,201,895.000*** -23,201,895.000*** -23,201,895.000***
                                      (6,098,702.000)    (6,098,702.000)    (6,098,702.000)    (6,098,702.000)  
                                                                                                                
    ------------------------------------------------------------------------------------------------------------
    Observations                           59,759             59,759             59,759             59,759      
    R2                                     0.666              0.666              0.666              0.666       
    Adjusted R2                            0.631              0.631              0.631              0.631       
    Residual Std. Error (df = 54114)   36,684,811.000     36,684,811.000     36,684,811.000     36,684,811.000  
    ============================================================================================================
    Note:                                                                            *p<0.1; **p<0.05; ***p<0.01

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
mod.esct <- feols(unc_care~i(year, treated, ref=2013) | state + year,
               cluster=~state,
               data=df)
esttable(mod.esct)
```

                                                mod.esct
    Dependent Var.:                             unc_care
                                                        
    treated x year = 2003      4,849,287.9 (3,828,543.7)
    treated x year = 2004      4,611,395.2 (3,907,318.1)
    treated x year = 2005     5,850,083.6. (3,474,631.1)
    treated x year = 2006     6,863,700.1. (3,936,159.4)
    treated x year = 2007      4,201,327.3 (2,885,937.9)
    treated x year = 2008      1,554,082.2 (2,512,567.3)
    treated x year = 2009       -500,388.2 (2,357,793.0)
    treated x year = 2010       -386,477.2 (2,769,931.6)
    treated x year = 2011     2,479,988.3. (1,431,194.2)
    treated x year = 2012        1,428,877.4 (861,291.0)
    treated x year = 2014   -8,629,352.9** (2,546,339.6)
    treated x year = 2015 -15,036,045.9*** (4,043,143.6)
    treated x year = 2016 -15,829,644.5*** (4,506,359.5)
    treated x year = 2017 -21,684,354.7*** (5,112,217.0)
    treated x year = 2018 -26,347,573.7*** (6,358,620.9)
    treated x year = 2019 -33,421,973.8*** (8,894,827.6)
    Fixed-Effects:        ------------------------------
    state                                            Yes
    year                                             Yes
    _____________________ ______________________________
    S.E.: Clustered                            by: state
    Observations                                  59,759
    R2                                           0.07863
    Within R2                                    0.01022
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
iplot(mod.esct)
```

![](Main_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
##### Differential timgin treatment
df %>% mutate(dif_timing = ifelse(expand_ever==FALSE, 0, ifelse(expand_year==0,0,year-expand_year)),
              time_to_treat = ifelse(dif_timing < -7, -7, dif_timing)) -> df

mod.esdt <- feols(unc_care~i(time_to_treat, treated, ref=-1) | state + year,
                  cluster=~state,
                  data=df)
modelsummary(mod.esdt, stars=TRUE)
```

|                              |       Model 1       |
|:-----------------------------|:-------------------:|
| time_to_treat = -7 × treated |     3534170.951     |
|                              |    (2812239.493)    |
| time_to_treat = -6 × treated |     2219365.752     |
|                              |    (2721654.334)    |
| time_to_treat = -5 × treated |     951669.195      |
|                              |    (2255831.555)    |
| time_to_treat = -4 × treated |    -1423750.132     |
|                              |    (2011959.108)    |
| time_to_treat = -3 × treated |    -1568163.195     |
|                              |    (2113404.951)    |
| time_to_treat = -2 × treated |      28496.307      |
|                              |    (1029982.045)    |
| time_to_treat = 0 × treated  |     2824417.238     |
|                              |    (2826222.119)    |
| time_to_treat = 1 × treated  | -13582088.234\*\*\* |
|                              |    (2774410.181)    |
| time_to_treat = 2 × treated  | -17186169.020\*\*\* |
|                              |    (3228864.734)    |
| time_to_treat = 3 × treated  | -21875456.875\*\*\* |
|                              |    (4017220.751)    |
| time_to_treat = 4 × treated  | -24765164.150\*\*\* |
|                              |    (5081994.078)    |
| time_to_treat = 5 × treated  | -28596232.480\*\*\* |
|                              |    (6736364.006)    |
| Num.Obs.                     |        59759        |
| AIC                          |      2306075.6      |
| BIC                          |      2306192.6      |
| RMSE                         |     57981916.00     |
| Std.Errors                   |      by: state      |
| FE: state                    |          X          |
| FE: year                     |          X          |

**Note:** ^^ + p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001

``` r
iplot(mod.esdt)
```

![](Main_files/figure-gfm/event-study-1.png)<!-- -->

``` r
esttable(mod.esdt)
```

                                                       mod.esdt
    Dependent Var.:                                    unc_care
                                                               
    treated x time_to_treat = -7      3,534,171.0 (2,812,239.5)
    treated x time_to_treat = -6      2,219,365.8 (2,721,654.3)
    treated x time_to_treat = -5        951,669.2 (2,255,831.6)
    treated x time_to_treat = -4     -1,423,750.1 (2,011,959.1)
    treated x time_to_treat = -3     -1,568,163.2 (2,113,405.0)
    treated x time_to_treat = -2         28,496.3 (1,029,982.0)
    treated x time_to_treat = 0       2,824,417.2 (2,826,222.1)
    treated x time_to_treat = 1  -13,582,088.2*** (2,774,410.2)
    treated x time_to_treat = 2  -17,186,169.0*** (3,228,864.7)
    treated x time_to_treat = 3  -21,875,456.9*** (4,017,220.8)
    treated x time_to_treat = 4  -24,765,164.2*** (5,081,994.1)
    treated x time_to_treat = 5  -28,596,232.5*** (6,736,364.0)
    Fixed-Effects:               ------------------------------
    state                                                   Yes
    year                                                    Yes
    ____________________________ ______________________________
    S.E.: Clustered                                   by: state
    Observations                                         59,759
    R2                                                  0.07768
    Within R2                                           0.00920
    ---
    Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         expand_year = ifelse(expand_ever==FALSE, 10000, expand_year),
         time_to_treat = ifelse(expand_ever==FALSE, -1, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -7, -7, time_to_treat))
mod.sa <- feols(unc_care~sunab(expand_year, time_to_treat) | state + year,
                  cluster=~state,
                  data=reg.dat)
```

    NOTE: 1 observation removed because of NA values (RHS: 1).

``` r
modelsummary(mod.sa, stars=TRUE)
```

|                    |       Model 1       |
|:-------------------|:-------------------:|
| time_to_treat = -7 |     -640411.475     |
|                    |    (1071884.701)    |
| time_to_treat = -6 |  -3273981.341\*\*   |
|                    |    (1204360.162)    |
| time_to_treat = -5 |    -2922055.028     |
|                    |    (2292913.985)    |
| time_to_treat = -4 |    -2697935.952     |
|                    |    (1768359.344)    |
| time_to_treat = -3 |     244351.952      |
|                    |    (1440417.868)    |
| time_to_treat = -2 |     -678394.279     |
|                    |    (1714215.042)    |
| time_to_treat = 0  | -10727040.419\*\*\* |
|                    |    (2968384.767)    |
| time_to_treat = 1  | -17670575.095\*\*\* |
|                    |    (3798652.250)    |
| time_to_treat = 2  | -19746437.962\*\*\* |
|                    |    (4330858.372)    |
| time_to_treat = 3  | -24630381.479\*\*\* |
|                    |    (5328572.542)    |
| time_to_treat = 4  | -27667569.340\*\*\* |
|                    |    (6379658.620)    |
| time_to_treat = 5  | -33268395.176\*\*\* |
|                    |    (8706109.100)    |
| Num.Obs.           |        59758        |
| AIC                |      2305994.9      |
| BIC                |      2306111.9      |
| RMSE               |     57961471.21     |
| Std.Errors         |      by: state      |
| FE: state          |          X          |
| FE: year           |          X          |

**Note:** ^^ + p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001

``` r
coefplot(mod.sa)
```

![](Main_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Event Study - SA specification

Present an event study graph based on the results in part 5. Hint: you
can do this automatically in `R` with the `fixest` package (using the
`sunab` syntax for interactions), or with `eventstudyinteract` in
`Stata`. These packages help to avoid mistakes compared to doing the
tables/figures manually and also help to get the standard errors
correct.

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
reg.dat <- df %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(post = (year>=2014), 
         treat=post*expand_ever,
         expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
  filter(!is.na(unc_care)) %>%
  group_by(state) %>%
  mutate(stategroup=cur_group_id()) %>% ungroup()

mod.cs <- att_gt(yname="unc_care", tname="year", idname="stategroup",
                 gname="expand_year",
                 data=reg.dat, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE)
#mod.cs.event <- aggte(mod.cs, type="dynamic")

mod.cs
```


    Call:
    att_gt(yname = "unc_care", tname = "year", idname = "stategroup", 
        gname = "expand_year", data = reg.dat, panel = TRUE, allow_unbalanced_panel = TRUE, 
        est_method = "dr")

    Reference: Callaway, Brantly and Pedro H.C. Sant'Anna.  "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230, 2021. <https://doi.org/10.1016/j.jeconom.2020.12.001>, <https://arxiv.org/abs/1803.09015> 

    Group-Time Average Treatment Effects:
     Group Time     ATT(g,t) Std. Error [95% Simult.  Conf. Band]  
      2014 2004           NA         NA            NA          NA  
      2014 2005           NA         NA            NA          NA  
      2014 2006           NA         NA            NA          NA  
      2014 2007           NA         NA            NA          NA  
      2014 2008           NA         NA            NA          NA  
      2014 2009           NA         NA            NA          NA  
      2014 2010           NA         NA            NA          NA  
      2014 2011           NA         NA            NA          NA  
      2014 2012           NA         NA            NA          NA  
      2014 2013    -79249.73   717099.4      -1770612   1612112.5  
      2014 2014 -10573280.71  2306017.3     -16012290  -5134271.4 *
      2014 2015 -18006418.97  3632429.8     -26573926  -9438911.8 *
      2014 2016 -18972948.26  4287386.0     -29085245  -8860651.1 *
      2014 2017 -23777818.73  4589055.2     -34601638 -12953999.7 *
      2014 2018 -27333199.61  5788721.4     -40986571 -13679828.5 *
      2014 2019 -32726979.36  8215885.8     -52105099 -13348859.5 *
      2015 2004           NA         NA            NA          NA  
      2015 2005           NA         NA            NA          NA  
      2015 2006           NA         NA            NA          NA  
      2015 2007           NA         NA            NA          NA  
      2015 2008           NA         NA            NA          NA  
      2015 2009           NA         NA            NA          NA  
      2015 2010           NA         NA            NA          NA  
      2015 2011           NA         NA            NA          NA  
      2015 2012           NA         NA            NA          NA  
      2015 2013   -488100.12   642429.4      -2003344   1027144.1  
      2015 2014  -5785882.63  2057271.6     -10638196   -933568.8 *
      2015 2015  -5262500.24  2581962.1     -11352357    827356.9  
      2015 2016  -8056314.42  2487413.9     -13923169  -2189460.2 *
      2015 2017 -14711027.65  2939878.8     -21645073  -7776982.4 *
      2015 2018 -19591879.90  4042675.0     -29126998 -10056761.9 *
      2015 2019 -24875322.56  6653593.7     -40568595  -9182049.9 *
      2016 2004           NA         NA            NA          NA  
      2016 2005           NA         NA            NA          NA  
      2016 2006           NA         NA            NA          NA  
      2016 2007           NA         NA            NA          NA  
      2016 2008           NA         NA            NA          NA  
      2016 2009           NA         NA            NA          NA  
      2016 2010           NA         NA            NA          NA  
      2016 2011           NA         NA            NA          NA  
      2016 2012           NA         NA            NA          NA  
      2016 2013   -759704.93   820072.6      -2693941   1174531.5  
      2016 2014  -1890198.86  2387575.1      -7521572   3741174.0  
      2016 2015  -3332041.18  1535818.1      -6954446    290364.1  
      2016 2016  -1294344.90  1007154.0      -3669834   1081144.6  
      2016 2017 -13948304.30  3121844.9     -21311538  -6585070.9 *
      2016 2018 -18996278.44  4101253.5     -28669561  -9322996.3 *
      2016 2019 -26165952.74  5950947.8     -40201954 -12129951.9 *
      2019 2004           NA         NA            NA          NA  
      2019 2005           NA         NA            NA          NA  
      2019 2006           NA         NA            NA          NA  
      2019 2007           NA         NA            NA          NA  
      2019 2008           NA         NA            NA          NA  
      2019 2009           NA         NA            NA          NA  
      2019 2010           NA         NA            NA          NA  
      2019 2011           NA         NA            NA          NA  
      2019 2012           NA         NA            NA          NA  
      2019 2013  -2764663.42   811523.0      -4678734   -850592.4 *
      2019 2014     78892.61  2537843.3      -5906905   6064690.5  
      2019 2015  -2666942.41  1551240.9      -6325724    991839.2  
      2019 2016   3766810.46  2300543.3      -1659288   9192908.6  
      2019 2017  -2905977.98  1864267.2      -7303068   1491112.5  
      2019 2018    308384.39  3060349.7      -6909805   7526574.1  
      2019 2019 -14292542.83  3361098.6     -22220084  -6365001.8 *
    ---
    Signif. codes: `*' confidence band does not cover 0

    Control Group:  Never Treated,  Anticipation Periods:  0
    Estimation Method:  Doubly Robust

``` r
ggdid(mod.cs)
```

![](Main_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#mod.cs.event
#ggdid(mod.cs.event)
#coefplot(mod.cs.event)
```

## Rambachan and Roth (RR)

Rambachan and Roth (RR) show that traditional tests of parallel
pre-trends may be underpowered, and they provide an alternative
estimator that essentially bounds the treatment effects by the size of
an assumed violation in parallel trends. One such bound RR propose is to
limit the post-treatment violation of parallel trends to be no worse
than some multiple of the pre-treatment violation of parallel trends.
Assuming linear trends, such a violation is reflected by
$$\Delta(\bar{M}) = \left\{ \delta : \forall t \geq 0, \lvert (\delta_{t+1} - \delta_{t}) - (\delta_{t} - \delta_{t-1}) \rvert \leq \bar{M} \times \max_{s<0} \lvert (\delta_{s+1} - \delta_{s}) - (\delta_{s} - \delta_{s-1}) \rvert \right\}.$$
Using the `HonestDiD` package in `R` or `Stata`, present a sensitivity
plot of your CS ATT estimates using $\bar{M} = \{0, 0.5, 1, 1.5, 2\}$.
Check out the GitHub repo [here](https://github.com/pedrohcgs/CS_RR) for
some help in combining the `HonestDiD` package with CS estimates.

## Discussion

Discuss your findings and compare estimates from different estimators
(e.g., are your results sensitive to different specifications or
estimators? Are your results sensitive to violation of parallel trends
assumptions?).

## Reflections

Reflect on this assignment. What did you find most challenging? What did
you find most surprising?
