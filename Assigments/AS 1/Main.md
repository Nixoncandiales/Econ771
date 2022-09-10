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
if (!exists("data_hcris")) data_hcris <- read.delim(here("Assigments", "As 1", "Output", "HCRIS", "HCRIS_Data.txt"))
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
if (!exists("data_pos")) data_pos <- read_stata(here("Assigments", "As 1", "Output", "POS", "pos_lastyear.v12.dta"))
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
if (!exists("data_aca")) data_aca <- read.delim(here("Assigments", "As 1", "Output", "ACA", "acs_medicaid.txt"))
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

### Summary Statistics

Provide and discuss a table of simple summary statistics showing the
mean, standard deviation, min, and max of hospital total revenues and
uncompensated care over time.

From the `HCRIS_data.txt` we select the variables `provider_number`,
`year`, `uncomp_care`, `tot_uncomp_care_charges`, `tot_pat_rev`. We
create a new variable that stores the uncompensated care records, then
we group by year and calculate the summary statistics as follows.

``` r
data_hcris %>% 
  rowwise() %>% 
  mutate(hosp_rev = tot_pat_rev, unc_care = 
           sum(tot_uncomp_care_charges,uncomp_care, na.rm=TRUE)) %>%
  mutate_at(c('unc_care'), ~na_if(., 0)) %>%
  select(pn=provider_number, year, unc_care, hosp_rev )  %>%
  filter(!(is.na(unc_care) & is.na(hosp_rev)) )-> data_hcris1

data_hcris1 %>%
  group_by(year) %>%
  summarise_at(c('unc_care', 'hosp_rev'),list(mean = mean, sd = sd, min = min, max = max), na.rm=T) %>%
  knitr::kable()
```

| year | unc_care_mean | hosp_rev_mean | unc_care_sd | hosp_rev_sd | unc_care_min | hosp_rev_min | unc_care_max | hosp_rev_max |
|-----:|--------------:|--------------:|------------:|------------:|-------------:|-------------:|-------------:|-------------:|
| 1997 |           NaN |      17406411 |          NA |    25347614 |          Inf |       239580 |         -Inf |    128092000 |
| 1998 |           NaN |     106218796 |          NA |   169829486 |          Inf |       155387 |         -Inf |   2255621364 |
| 1999 |           NaN |     117511354 |          NA |   189181015 |          Inf |            1 |         -Inf |   2586692428 |
| 2000 |           NaN |     131767289 |          NA |   217132934 |          Inf |            1 |         -Inf |   2823988041 |
| 2001 |           NaN |     147463809 |          NA |   248432404 |          Inf |         2795 |         -Inf |   3267554934 |
| 2002 |             1 |     170499912 |          NA |   291278067 |            1 |          347 |            1 |   3957656325 |
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
| 2020 |      29100316 |     707845527 |    82874954 |  1450390406 |           -2 |        19212 |   2245174712 |  29390141705 |
| 2021 |      30474261 |     793550259 |    90941820 |  1641471955 |            1 |    -62618391 |   2655216314 |  34521586839 |
| 2022 |           NaN |      81138475 |          NA |          NA |          Inf |     81138475 |         -Inf |     81138475 |

``` r
data_hcris1 %>%
  ggplot(aes(x = year, y = unc_care, group=year)) + 
  geom_boxplot() + 
  theme_tufte() -> plot1

data_hcris1 %>%
  ggplot(aes(x = year, y = hosp_rev, group=year)) + 
  geom_boxplot() + 
  theme_tufte()  -> plot2

plot1 | plot2
```

![](Main_files/figure-gfm/plot-summary-stats-1.png)<!-- -->

### By Ownership Type

Create a figure showing the mean hospital uncompensated care from 2000
to 2018. Show this trend separately by hospital ownership type (private
not for profit and private for profit).

``` r
data_hcris1 <- data_hcris1 %>% filter(year>=2000 & year<=2018) #subset by the years

data_pos1 <- data_pos %>%
  select(pn = unique('pn'), nonprofit, forprofit, active) %>%
  mutate_at('pn', as.integer) %>% # This will force strings ID into NA (those are no-hospital POS)
  filter( pn >0 ) %>% #remove NA cases
  mutate(own_typ = case_when(nonprofit == 0  & forprofit == 0  ~ 'other',
                             nonprofit == 0  & forprofit == 1  ~ 'forprofit',
                             nonprofit == 1  & forprofit == 0  ~ 'nonprofit')) %>% # Create a factor variable for ownership type
  filter((own_typ == 'forprofit') | (own_typ == 'nonprofit' )  ) %>%  # discard govt and other forms of ownership
  select(pn, own_typ) # Select the variables to merge

hcris_1 <- left_join(data_hcris1, data_pos1, by="pn") %>% 
  mutate_at('own_typ',  replace_na, 'other') %>% # replace NA by other
  select(pn, year, unc_care, own_typ) %>%
  filter(!(own_typ == 'other'))
##This last line of code is because if I include other types of ownerships I am getting duplicated cases.. i need to dig a bit more into this behavior. 
```

|    pn | year | unc_care | own_typ   |
|------:|-----:|---------:|:----------|
| 10007 | 2000 |       NA | nonprofit |
| 10007 | 2001 |       NA | nonprofit |
| 10007 | 2002 |       NA | nonprofit |
| 10007 | 2003 |   432407 | nonprofit |
| 10007 | 2004 |  3140810 | nonprofit |
| 10007 | 2005 |  5788297 | nonprofit |

``` r
hcris_1 %>%
  group_by(year, own_typ) %>%
  summarise_at(c('unc_care'), list(unc_care_mean = mean), na.rm=T) %>%
  ggplot(aes(x=year, y=unc_care_mean, group=own_typ, color=own_typ)) -> plot3

plot3 + geom_point(size = 1) +
  geom_smooth(aes(fill = own_typ), size = 1) +
  theme_tufte()
```

![](Main_files/figure-gfm/plot3-1.png)<!-- -->

### 3

### 4

### 5

### 6

### 7

### 8

### 9

### 10
