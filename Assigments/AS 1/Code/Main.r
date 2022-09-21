# Meta --------------------------------------------------------------------

## Author:        Nixon Torres Candiales
## Date Created:  09/01/2022
## Date Edited:   8/25/2022
## Notes:         -- Assigment 1. R-script 
##                   Need the 3 data sets on folder
##                   See Read ME.

## ---- Preliminaries ----------------------------------------------------------- ----------------------------------------------------------- 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, haven, descriptr, HonestDiD, did, fixest, RColorBrewer, patchwork, ggthemes, DT, plotly, here, crosswalkr, plm, stargazer, modelsummary, fixest, DRDID, ggthemes, patchwork, devtools, lfe)


## ----Import the data ----------------------------------------------------------- ----------------------------------------------------------- 
here::i_am("Main.Rmd")
if (!exists("data_hcris")) data_hcris <- read.delim(here("Output", "HCRIS", "HCRIS_Data.txt"))
if (!exists("data_pos")) data_pos <- read_stata(here("Output", "POS", "pos.dta"))
if (!exists("data_aca")) data_aca <- read.delim(here("Output", "ACA", "acs_medicaid.txt"))

# Describe the data
ds_screener(data_hcris)
ds_screener(data_pos)
ds_screener(data_aca)

## ---- Merge the data sets ----------------------------------------------------------- ----------------------------------------------------------- 

## Merge the first twotwo data sets
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
    distinct(pn, state, year, own_typ, unc_care, hosp_rev) # I do not fully understand the behavior of left_join it creates multiple row, so I am just taking the unique values per PN and YEAR

## Merge the third data set by left join to the previous created.
  df_3 <- data_aca %>% # crosswalk the states names to states abbreviations and drop Puerto Rico from the analysis
    #filter(!(State=='Puerto Rico')) %>%  ###  SHOULD WE INCLUDE PUERTO RICO ON THE ANALISYS??
    mutate(state= encodefrom(., State, stcrosswalk, stname, stfips, stabbr)) %>%
    select(!State) %>%
    relocate(state) #make sure the ID variable has the same name on both data sets
  
  df <- left_join(df,# %>% filter(!(state=="PR")), 
                  df_3, by=c('state', 'year')) %>% # Filtering out PR since is not in df_3, to avoid future NA
    relocate(pn, year, state, own_typ ,expand_ever, expand, expand_year, unc_care)
  
  
  ## ---- Summary Statistics ----------------------------------------------------------- ----------------------------------------------------------- 
df %>% ungroup() %>%
  summarise_at(c("unc_care", "hosp_rev"), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)

df_1 %>%
  group_by(year) %>%
  summarise_at(c('unc_care', 'hosp_rev'),list(mean = mean, sd = sd, min = min, max = max), na.rm=T) 

# Boxplots  
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

## Plot means unc_care by Hospital ownership type
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
  
  ## ---- Identify Outliers ----------------------------------------------------------- ----------------------------------------------------------- 
  is_outlier <- function(x, ...) {   #The objective of this function is to identify outliers
    return(x < quantile(x, 0.25, ...) -40.5 * IQR(x, ...) | x > quantile(x, 0.75, ...) + 40.5 * IQR(x, ...))
  }
  
  df %>%
    group_by(year) %>%
    mutate(outlier = is_outlier(unc_care, na.rm = TRUE)) %>%
    filter(!(outlier == FALSE) & ( year == 2010 | year == 2016 )) 
  
  ## PN : 151327 Year 2016
  ## PN : 150056 Year 2010
  ## Those two values are extremely atypical... consider remove those!!
 
  ## ---- DiD Strategy ----------------------------------------------------------- -----------------------------------------------------------    
  ##Create dummies for the control groups
  df %>% filter(!(pn==151327 & year ==2016) & unc_care > 0) %>%
    mutate(d = case_when(expand == TRUE ~ 1),
           d_14 = case_when((expand == TRUE & expand_year==2014) ~ 1),
           d_15 = case_when((expand == TRUE & expand_year==2015) ~ 1),
           d_16 = case_when((expand == TRUE & expand_year==2016) ~ 1)) %>%
    mutate(across(d:d_16, ~ifelse(is.na(.),0,.))) -> df
  
  # Aply the function across
  mod.twfe <- lapply(df %>% 
                       select(d:d_16), #Select the treatments 
                     function(Treatment) felm(unc_care ~ Treatment | pn + year | 0 | pn, df)) #Apply the specification across the different treatments and store the results in a list
  stargazer(mod.twfe, type='text', note="1-4 representes d, d_14,d_15 and d_16 respectevely") # Consider using ModelSummary
  
  ## ---- Event Study Strategy ----------------------------------------------------------- -----------------------------------------------------------   
  ## Common treatment timing
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
  iplot(mod.esct)
  
  ## Differential timgin treatment
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
  esttable(mod.esdt)
  iplot(mod.esdt)  

  ## ---- SA Strategy ----------------------------------------------------------- -----------------------------------------------------------     
  reg.dat <- df %>% 
    group_by(state) %>% 
    mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
    fill(starts_with("exp"), .direction = "up")  %>%  # Fill the NA for the years that do not appear in the aca data set.
    mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
           expand=ifelse(!is.na(expand),expand,FALSE))
  
  sa <- function(data, i){ # Create a function to apply the SA expecification based on year of treatment
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
  
  
  mod.sa <- list( #create a list containing the models for displaying those later
    "mod.sa.2016" = sa(reg.dat, 2016),
    "mod.sa.2015" = sa(reg.dat, 2015),
    "mod.sa.2014" = sa(reg.dat, 2014)
  )
  
  modelsummary(mod.sa, stars = TRUE, output = "markdown")
  
  coefplot(mod.sa, main="Effect of Medicaid Eaxpansion on Uncompensated Care") #Plot the event study
  
  
  ## ---- CS Strategy ----------------------------------------------------------------------------------------------------------------------      
  # Note that by changing the cluster to pn level gives a bug!!!!!!!
  # Explore this behaviour further.
  
  #prepare the data
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
  mod.cs.event
  
  ## Plots
  ggdid(mod.cs) 
  ggdid(mod.cs.event, 
        title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group")
  
  ## ---- RR Strategy ----------------------------------------------------------- -----------------------------------------------------------     
  
  # At this point I am only able to reproduce their code. when trying to addapt to mod.cs.event gives a bug
  # the year previous to the treatment is zero and se NA ... this might be the reason of the bug... again, explore futher!!
  
  # Install some packages
  library(devtools)
  install_github("bcallaway11/BMisc", dependencies = TRUE)
  install_github("bcallaway11/did", dependencies = TRUE)
  install_github("asheshrambachan/HonestDiD", dependencies = TRUE)

  # Load packages

  # Libraries
  # Load libraries
  library(ggplot2)
  library(here)
  library(foreign)
  library(tidyverse)
  library(dplyr)
  library(did)
  library(HonestDiD)
  
  ## 
  
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
  
  
  # code for running honest_did
  hd_cs_smooth_never <- honest_did(CS_es_never_cond,
                                   type="smoothness")
  hd_cs_smooth_never
  
  
  hd_cs_rm_never <- honest_did(CS_es_never_cond, type="relative_magnitude")
  hd_cs_rm_never
  # Drop 0 as that is not really allowed.
  hd_cs_rm_never$robust_ci <- hd_cs_rm_never$robust_ci[-1,]
  

  # make sensitivity analysis plots
  cs_HDiD_smooth <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                          hd_cs_smooth_never$orig_ci)
  
  
  cs_HDiD_relmag <- createSensitivityPlot_relativeMagnitudes(hd_cs_rm_never$robust_ci,
                                                             hd_cs_rm_never$orig_ci)
  
  cs_HDiD_smooth
  cs_HDiD_relmag
  
  
  
  ## ---- Write results -----------------------------------------------------------------------------
  ## Plots
  #ggsave(here("plots","cs_HDiD_smooth.png"),
  #       cs_HDiD_smooth,  
  #       dpi = 500,
  #       width = 14, 
  #       height = 7)
  
  #ggsave(here("plots","cs_HDiD_relmag.png"),
  #       cs_HDiD_relmag,  
  #       dpi = 500,
  #       width = 14, 
  #       height = 7)
  
  ## Tables
  

