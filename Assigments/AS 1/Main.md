Assigment 1
================

## Downloading the Raw Data

We start by downloading and processing the
[HCRIS](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/HCRIS),
[POS](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/POS),
and
[ACA](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/ACA)
raw data sets. Then calling the output data from`HCRIS_Data.txt` and
`pos_lastyear.v12.dta`

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

### Summary Statistics

Provide and discuss a table of simple summary statistics showing the
mean, standard deviation, min, and max of hospital total revenues and
uncompensated care over time.

From the `HCRIS_data.txt` we select the variables *provider_number*,
*year*, *tot_uncomp_care_charges*, *tot_pat_rev*

``` r
data_hcris %>% select(provider_number, year, tot_uncomp_care_charges, tot_pat_rev) %>% as_tibble()
```

    # A tibble: 142,504 × 4
       provider_number  year tot_uncomp_care_charges tot_pat_rev
                 <int> <int>                   <dbl>       <dbl>
     1           10001  1998                      NA   304888068
     2           10001  1999                      NA   330880661
     3           10001  2000                      NA   359149872
     4           10001  2001                      NA   437847861
     5           10001  2002                      NA   509731719
     6           10001  2003                      NA   532023593
     7           10001  2004                      NA   592438087
     8           10001  2005                      NA   657842984
     9           10001  2006                      NA   714123644
    10           10001  2007                      NA   772492758
    # … with 142,494 more rows

Then we group by year and calculate the summary statistics.

``` r
data_hcris %>% select(provider_number, year, tot_uncomp_care_charges, tot_pat_rev) %>% 
  group_by(year) %>% summarise(Mean = mean(tot_uncomp_care_charges, na.rm = TRUE), 
                               SD = sd(tot_uncomp_care_charges, na.rm = TRUE), Min = min(tot_uncomp_care_charges, na.rm = TRUE), 
                               Max = max(tot_uncomp_care_charges, na.rm = TRUE)) %>% drop_na(Mean)
```

    # A tibble: 11 × 5
        year      Mean        SD       Min        Max
       <int>     <dbl>     <dbl>     <dbl>      <dbl>
     1  2011 17217489. 47251398. -28840406 1111027264
     2  2012 18338225. 55879179.        85 1371421445
     3  2013 19648564. 57646114.       216 1403146636
     4  2014 19607345. 63262016.        15 1874409188
     5  2015 19024979. 61755917.        22 1990560423
     6  2016 19810030. 66724247.        84 2231833221
     7  2017 22135100. 69491982.        34 2062118188
     8  2018 24883218. 74503094.         1 2183167185
     9  2019 28705587. 83757685.         2 2495183582
    10  2020 29100316. 82874954.        -2 2245174712
    11  2021 30474261. 90941820.         1 2655216314

``` r
data_hcris %>% select(provider_number, year, tot_uncomp_care_charges, tot_pat_rev) %>% 
  group_by(year) %>% summarise(Mean = mean(tot_pat_rev, na.rm = TRUE), 
                               SD = sd(tot_pat_rev, na.rm = TRUE), Min = min(tot_pat_rev, na.rm = TRUE), 
                               Max = max(tot_pat_rev, na.rm = TRUE)) %>% drop_na(Mean)
```

    # A tibble: 26 × 5
        year       Mean         SD      Min        Max
       <int>      <dbl>      <dbl>    <dbl>      <dbl>
     1  1997  17406411.  25347614.   239580  128092000
     2  1998 106218796. 169829486.   155387 2255621364
     3  1999 117511354. 189181015.        1 2586692428
     4  2000 131767289. 217132934.        1 2823988041
     5  2001 147463809. 248432404.     2795 3267554934
     6  2002 170499912. 291278067.      347 3957656325
     7  2003 196326204. 339256130. -1757898 4722758791
     8  2004 217080321. 379301539.   154394 5525730727
     9  2005 237498725. 419216031.        1 6398553843
    10  2006 262155653. 464190671.  -104189 7784094716
    # … with 16 more rows
