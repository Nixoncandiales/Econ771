## ----setup, include=FALSE, cache=FALSE, tidy=TRUE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, haven, descriptr, HonestDiD, did, fixest, RColorBrewer, patchwork, ggthemes, DT, plotly, here, crosswalkr, plm, stargazer, modelsummary, fixest, DRDID, ggthemes, patchwork, devtools, lfe, naniar, xtable)
knitr::opts_knit$set(root.dir = here("Assigments", "AS 1"))

knitr::opts_chunk$set(echo = TRUE, comment=NA)
knitr::opts_chunk$set(error = TRUE, cache =F)
hook_in <- function(x, options) {
    x <- x[!grepl("^#\\s+", x)]
    paste0("```r\n",
          paste0(x, collapse="\n"),
          "\n```")
}
knitr::knit_hooks$set(source = hook_in)


## ----import-data, tidy=TRUE, echo=FALSE, cache=TRUE, comment=NA, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
here::i_am("Main.Rmd")
if (!exists("data_hcris")) data_hcris <- read.delim(here("Output", "HCRIS", "HCRIS_Data.txt"))
if (!exists("data_pos")) data_pos <- read_stata(here("Output", "POS", "pos.dta"))
if (!exists("data_aca")) data_aca <- read.delim(here("Output", "ACA", "acs_medicaid.txt"))


## ----data-hcris, include=TRUE, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA-------------------------------------------------------------------------------------------------------------------------------------------------------
ds_screener(data_hcris)


## ----data-pos, include=TRUE, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA---------------------------------------------------------------------------------------------------------------------------------------------------------
ds_screener(data_pos)


## ----data-medicaid, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_screener(data_aca)


## ----merge-1, include=TRUE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

df 



## ----merge-2, include=TRUE, comment=NA, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_3 <- data_aca %>% # crosswalk the states names to states abbreviations and drop Puerto Rico from the analysis
              #filter(!(State=='Puerto Rico')) %>%
              mutate(state= encodefrom(., State, stcrosswalk, stname, stfips, stabbr)) %>%
              select(!State) %>%
              relocate(state) #make sure the ID variable has the same name on both data sets

df <- left_join(df,# %>% filter(!(state=="PR")), 
                df_3, by=c('state', 'year')) %>% # Filtering out PR since is not in df_3, to avoid future NA
  relocate(pn, year, state, own_typ ,expand_ever, expand, expand_year, unc_care)


## ----summary-stats, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
df %>% ungroup() %>%
  summarise_at(c("unc_care", "hosp_rev"), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE) -> table1

df %>%
  group_by(year) %>%
  summarise_at(c('unc_care', 'hosp_rev'),list(mean = mean, sd = sd, min = min, max = max), na.rm=T) %>%
  relocate(starts_with("unc"), starts_with("hosp")) -> table2

table1
table2


## ----plot-summary-stats, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
df <- df %>%  filter(!(pn==151327 & year ==2016) & unc_care > 0)

df %>% ggplot(aes(x = year, y = unc_care, group=year)) +
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



## ----outliers, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# #Identify Outliers Function
# #The objective of this function is to identify outliers
# is_outlier <- function(x, ...) {
#     return(x < quantile(x, 0.25, ...) -40.5 * IQR(x, ...) | x > quantile(x, 0.75, ...) + 40.5 * IQR(x, ...))
# }
# 
# df %>%
#     group_by(year) %>%
#     mutate(outlier = is_outlier(unc_care, na.rm = TRUE)) %>%
#     filter(!(outlier == FALSE) & ( year == 2010 | year == 2016 )) 
# 
# ## PN : 151327 Year 2016
# ## PN : 150056 Year 2010
# 
# ## Those two values are extremely atypical... consider remove those!!


## ----load-libraries, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(crosswalkr)
library(lfe)
library(stargazer)


## ----plot-own-typ, warning=FALSE, comment=NA, message = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
df %>%
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



## ----DiD, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
 df %>%        group_by(pn) %>% 
               fill(starts_with("exp"), .direction = "up")  %>%  # Fill the NA for the years that do not appear in the aca data set.
               mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
               mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
                      expand=ifelse(!is.na(expand),expand,FALSE)) %>%
               ungroup()%>%
               mutate(treatment_year = ifelse(expand_year<=2019, expand_year,0),
                      treated = ifelse(expand_year<=2019, expand_ever*1,0),
                      post = (year>=treatment_year & !(treatment_year==0) ),
                      D = treated*post,
                      D14 = ifelse(treatment_year<2014, 0, ifelse(treatment_year>2014, 999, treated*post)),
                      D15 = ifelse(treatment_year<2015, 0, ifelse(treatment_year>2015, 999, treated*post)),
                      D16 = ifelse(treatment_year<2016, 0, ifelse(treatment_year>2016, 999, treated*post)))-> df

 replace_with_na(df, list(D14=999,D15=999,D16=999)) -> df


## ----DiD-TWFE, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
mod.twfe <- lapply(df %>% 
                select(D:D16), #Select the treatments 
              function(Treatment) felm(unc_care ~ Treatment | pn + year | 0 | pn, df)) #Apply the specification across the different treatments and store the results in a list
stargazer(mod.twfe, type='text', note="1-4 representes D, D14,D15 and D16 respectevely")


## ----event-study-common, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
### Common treatment timing
 mod.esct <- feols(unc_care~i(year, treated, ref=2013) | pn + year,
                cluster=~pn,
                data=df)
esttable(mod.esct)
iplot(mod.esct)


## ----event-study-diff, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
##### Differential timgin treatment
df %>% mutate(dif_timing = ifelse(expand_ever==FALSE, 0, ifelse(expand_year==0,0,year-expand_year)),
                time_to_treat = ifelse(dif_timing < -7, -7, dif_timing)) %>% 
                ungroup() -> df
 
 mod.esdt <- feols(unc_care~i(time_to_treat, treated, ref=-1) | pn + year,
                   cluster=~pn,
                   data=df)
esttable(mod.esdt)
 iplot(mod.esdt)


## ----sa, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
# reg.dat <- df %>% 
#             group_by(state) %>% 
#             mutate(expand_year=ifelse(is.na(expand_year),0,expand_year)) %>%
#             fill(starts_with("exp"), .direction = "up")  %>%  # Fill the NA for the years that do not appear in the aca data set.
#             mutate(expand_ever=ifelse(is.na(expand_ever),FALSE,expand_ever),
#                    expand=ifelse(!is.na(expand),expand,FALSE))
# 
# sa <- function(data, i){
#   
#   data %>%
#       mutate(post = (year>=i), 
#       treat=post*expand_ever,
#       expand_year = ifelse(expand_ever==FALSE, 10000, ifelse(expand_year>=i,expand_year,100000)),
#       time_to_treat = ifelse(expand_ever==FALSE, -1, year-expand_year),
#       time_to_treat = ifelse(time_to_treat < ifelse(i==2014,-5,
#                                                     ifelse(i==2015, -6 , -7 )), 
#                                              ifelse(i==2014,-5, ifelse(i==2015, -6 , -7 )) , 
#                              time_to_treat)) -> x
# 
#    feols(unc_care~sunab(expand_year, time_to_treat) | pn + year,
#                       cluster=~pn,
#                       data=x)
# }
# 
# 
# mod.sa <- list(
#   "mod.sa.2016" = sa(reg.dat, 2016),
#   "mod.sa.2015" = sa(reg.dat, 2015),
#   "mod.sa.2014" = sa(reg.dat, 2014)
# )
# 
# modelsummary(mod.sa, stars = TRUE, output = "markdown")


dat.reg <- df %>%
           mutate(treatment_year = ifelse(treatment_year<2014 | treatment_year>2016, 10000, treatment_year),
                  time_to_treat = ifelse(treated==0,-1, year-treatment_year),
                  time_to_treat = ifelse(time_to_treat < -5, -5, time_to_treat))
 
                  mod.sa <- feols(unc_care~sunab(treatment_year, time_to_treat)|pn+ year,
                                     cluster=~pn,
                                     data=dat.reg)
                  
esttable(mod.sa)


## ----sa-plot, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
coefplot(mod.sa, main="Effect of Medicaid Eaxpansion on Uncompensated Care")


## ----cs, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
df %>% group_by(state) %>% 
  mutate(state_id=cur_group_id()) %>% 
  group_by(pn) %>% 
  mutate(pn_id=cur_group_id()) %>% 
  ungroup() %>% 
  distinct(pn_id, year, .keep_all = TRUE) ->df

mod.cs <- att_gt(yname="unc_care", 
                 tname="year", 
                 idname="pn_id",
                 gname="treatment_year",
                 data=a, 
                 panel=TRUE, 
                 est_method="dr",
                 cband=TRUE,
                 bstrap=TRUE,
                 allow_unbalanced_panel=TRUE,
                 base_period="universal")

mod.cs.event<- aggte(mod.cs, type="dynamic", min_e = -7, max_e = 6)
mod.cs
mod.cs.event
ggdid(mod.cs)
ggdid(mod.cs.event, 
      title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group")


## ----Aux-func-RR, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
# Install some packages
library(devtools)
install_github("bcallaway11/BMisc", dependencies = TRUE)
install_github("bcallaway11/did", dependencies = TRUE)
install_github("asheshrambachan/HonestDiD", dependencies = TRUE)
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
                                                             parallel=parallel
                                                             )
    
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
                                          parallel=parallel, 
                                          Mvec=Mvec)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}


## ----RR, tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------

# code for running honest_did
hd_cs_smooth_never <- honest_did(mod.cs.event,
                           type="smoothness", Mvec=c(0.5,1,1.5,2))
hd_cs_smooth_never

hd_cs_rm_never <- honest_did(mod.cs.event, type="relative_magnitude", Mbarvec=c(500,1000,1500,2000))
hd_cs_rm_never

# Drop 0 as that is not really allowed.
hd_cs_rm_never$robust_ci <- hd_cs_rm_never$robust_ci[-1,]

# make sensitivity analysis plots
cs_HDiD_smooth <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                      hd_cs_smooth_never$orig_ci)
cs_HDiD_relmag <- createSensitivityPlot_relativeMagnitudes(hd_cs_rm_never$robust_ci,
                                         hd_cs_rm_never$orig_ci)


## ----RR-plots,  tidy=TRUE, echo=TRUE, cache=TRUE, comment=NA, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
cs_HDiD_smooth
cs_HDiD_relmag


## ----write-results----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## Plots
here::i_am("Main.Rmd")
  ggsave(here("Output","Figures", "plot0.png"),
         plot1,
         dpi = 500,
         width = 14,
         height = 7)

  ggsave(here("Output","Figures", "plot1.png"),
         plot2,
         dpi = 500,
         width = 14,
         height = 7)

  ggsave(here("Output","Figures", "plot2.png"),
         plot3,
         dpi = 500,
         width = 14,
         height = 7)

  png(file=here("Output","Figures", "plot3.png"),
      width=14, height=7, units="in", res=500)
  iplot(mod.esct, main="Effect of Medicaid Eaxpansion on Uncompensated Care")
  dev.off()

  png(file=here("Output","Figures", "plot4.png"),
      width=14, height=7, units="in", res=500)
  iplot(mod.esdt)
  dev.off()

  png(file=here("Output","Figures", "plot5.png"),
      width=14, height=7, units="in", res=500)
  coefplot(mod.sa, main="Effect of Medicaid Eaxpansion on Uncompensated Care")
  dev.off()

  png(file=here("Output","Figures", "plot6.png"),
      width=14, height=7, units="in", res=500)
  ggdid(mod.cs)
  dev.off()

  png(file=here("Output","Figures", "plot7.png"),
      width=14, height=7, units="in", res=500)
  ggdid(mod.cs.event,
        title = "Event-study aggregation \n DiD based on conditional PTA and using never-treated as comparison group")
  dev.off()

  png(file=here("Output","Figures", "plot8.png"),
      width=14, height=7, units="in", res=500)
  cs_HDiD_smooth
  dev.off()

  png(file=here("Output","Figures", "plot9.png"),
      width=14, height=7, units="in", res=500)
  cs_HDiD_relmag
  dev.off()


  ## Tables
#  xtable(table1)
#  xtable(table2)
  stargazer(mod.twfe, type='latex', align=TRUE, title = "Table 3",
                      note="1-4 represents d, d_14,d_15 and d_16 respectevely",
                      out = here("Output", "Tables", "table3.tex")) # Consider using ModelSummary
  modelsummary(mod.esct, stars = TRUE, title = "Table 4",
                         output= here("Output", "Tables", "table4.tex"))
  modelsummary(mod.esdt, stars=TRUE, title = "Table 5",
                         output= here("Output", "Tables", "table5.tex"))
  modelsummary(mod.sa, stars = TRUE, title = "Table 6",
                         output= here("Output", "Tables", "table6.tex"))
  modelsummary(mod.cs, title = "Table 7",
                         output= here("Output", "Tables", "table7.tex"))
  modelsummary(mod.cs.event, title = "Table 8",
                         output= here("Output", "Tables", "table8.tex"))

  rm(data_aca, data_hcris, data_pos, dat.reg, reg.dat, min_wage, fig_CS, df_1, df_2, df_3)
  save.image(here("Output", "Output.Rdata"))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("Main.Rmd", documentation = 1, output="Code/Main.R")

