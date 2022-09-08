Assigment 1
================

# 1) Downloading the Raw Data

[Prueba](https://github.com/Nixoncandiales/Econ771/tree/main/Assigments/AS%201/Code/HCRIS)
This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

         speed           dist       
     Min.   : 4.0   Min.   :  2.00  
     1st Qu.:12.0   1st Qu.: 26.00  
     Median :15.0   Median : 36.00  
     Mean   :15.4   Mean   : 42.98  
     3rd Qu.:19.0   3rd Qu.: 56.00  
     Max.   :25.0   Max.   :120.00  

## Including Plots

You can also embed plots, for example:

![](Prueba_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

### Example

``` r
# Meta --------------------------------------------------------------------

# Title:  Combine ACS and Medicaid Expansion Data
# Author: Nixon Candiales
# Date Created: 9/7/2022
# Date Edited:  ----


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
```

    Loading required package: pacman

``` r
pacman::p_load(tidyverse, ggplot2, here, descriptr, haven)

# Read Data -----------------------------------------------------------

data_hcris <- read.delim(here("Assigments", "As 1", "Output", "HCRIS", "HCRIS_Data.txt"))
data_pos <- read_stata(here("Assigments", "As 1", "Output", "POS", "pos_lastyear.v12.dta"))

print(ds_screener(data_hcris))
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

``` r
print(ds_screener(data_pos))
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

``` r
# Analysis -----------------------------------------------------------

ds_summary_stats(data_hcris, tot_uncomp_care_charges, tot_pat_rev)
```

    ---------------------- Variable: tot_uncomp_care_charges ----------------------

                                Univariate Analysis                              

     N                       142504.00      Variance             4.832795e+15 
     Missing                  95493.00      Std Deviation         69518308.19 
     Mean                  22510720.85      Range                2684056720.00 
     Median                 3491609.00      Interquartile Range   16996816.50 
     Mode                  46500521.00      Uncorrected SS       2.510117e+20 
     Trimmed Mean          12202020.53      Corrected SS         2.271897e+20 
     Skewness                    12.64      Coeff Variation            308.82 
     Kurtosis                   286.86      Std Error Mean          320626.53 

                                      Quantiles                                   

                    Quantile                                Value                  

                   Max                                  2655216314.00              
                   99%                                   287182950.80              
                   95%                                   100081338.50              
                   90%                                   54789935.00               
                   Q3                                    17523163.00               
                   Median                                 3491609.00               
                   Q1                                     526346.50                
                   10%                                     97495.00                
                   5%                                      36983.00                
                   1%                                      4495.10                 
                   Min                                   -28840406.00              

                                    Extreme Values                                

                      Low                                    High                  

        Obs                       Value            Obs                       Value    
        3609                    -28840406         38742                    2655216314 
       18372                     -7849786         38740                    2495183582 
       31584                        -2            38741                    2245174712 
        7484                        1             38737                    2231833221 
       23074                        1             38739                    2183167185 



    ---------------------------- Variable: tot_pat_rev ----------------------------

                                Univariate Analysis                              

     N                       142504.00      Variance             7.364722e+17 
     Missing                   5701.00      Std Deviation        858179571.51 
     Mean                 384084133.34      Range                34698618762.00 
     Median                88116196.00      Interquartile Range  343056565.00 
     Mode                 159399197.00      Uncorrected SS       1.209321e+23 
     Trimmed Mean         250053449.53      Corrected SS         1.007509e+23 
     Skewness                     7.65      Coeff Variation            223.44 
     Kurtosis                   118.16      Std Error Mean         2320226.39 

                                      Quantiles                                   

                    Quantile                                Value                  

                   Max                                  34521586839.00             
                   99%                                  3827506004.84              
                   95%                                  1703098756.30              
                   90%                                  1032935511.20              
                   Q3                                    370656047.50              
                   Median                                88116196.00               
                   Q1                                    27599482.50               
                   10%                                   10965012.80               
                   5%                                     6172250.60               
                   1%                                     1996881.60               
                   Min                                  -177031923.00              

                                    Extreme Values                                

                      Low                                    High                  

         Obs                      Value             Obs                      Value    
       115812                  -177031923          82658                  34521586839 
       134726                   -62618391          82657                  29390141705 
       111156                   -27582223          12079                  24973914387 
       123041                   -11799711          20471                  24812982853 
       123040                   -11146879          82656                  22000932119 

``` r
#To do:

# Mean
# Standard Deviation
# Min
# Max

#Over two variables (Hospital Total Reveneus, Uncompensated Care Over Time - tot_uncomp_care_charges)


#tidytable(data, info_cols = list(), calc_cols = list(`#missing` =
 # function(x) sum(is.na(x))), num_cols = list(mean = mean, sd = sd),
 # custom_vars = c(), custom_cols = list(), col_order = c(),
 # row_order = list(), digits = 2, add_cat_header_row = TRUE)
```
