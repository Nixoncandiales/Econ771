# This function subsets the data, create the lags and run the 
# regressions for panel a and b, and
# store the results in a list

My_lag <- function(t,l){
       dat.temp <- dat %>% 
                     group_by(uniqueIDNum) %>% 
                     mutate(
                                across(starts_with("LIS") & ends_with(c("ssq","gsq", "eg", "os")), 
                                ~ lag(.x, l))
                            ) %>%
                     filter(RDwindow20062==1 & year==t)
        return(dat.temp)
}
My_reg <- function(){
       reg.panel.a <- feols(lnS ~ belowBench2006 + 
                     LISPremiumNeg + 
                     LISPremiumPos, 
                     cluster="firmID", 
                     dat=dat.temp)
        
        if (t==2010){
                       reg.panel.b <- feols(lnS ~ belowBench2006 +
                                    LISPremiumNeg +
                                    LISPremiumPos +
                                    LISPremiumNegSq +
                                    LISPremiumPosSq |
                                    state + firmID +
                                    L4btypedetail[L4BA_0] +
                                    L4btypedetail[L4BA_1_99] +
                                    L4btypedetail[L4BA_100] +
                                    L4btypedetail[L4BA_101_99] +
                                    L4btypedetail[L4BA_200_49] +
                                    L4btypedetail[L4BA_250Up],
                            dat = dat.temp,
                            cluster = "firmID")
                        }

        if (t==2009){
            reg.panel.b <-  feols(lnS ~ belowBench2006 +
                                    LISPremiumNeg +
                                    LISPremiumPos +
                                    LISPremiumNegSq +
                                    LISPremiumPosSq |
                                    state + firmID +
                                    L3btypedetail[L3BA_0] +
                                    L3btypedetail[L3BA_1_99] +
                                    L3btypedetail[L3BA_100] +
                                    L3btypedetail[L3BA_101_99] +
                                    L3btypedetail[L3BA_200_49] +
                                    L3btypedetail[L3BA_250Up],
                            dat = dat.temp,
                            cluster = "firmID")
                        }

        if (t==2008){
            reg.panel.b <- feols(lnS ~ belowBench2006 +
                                    LISPremiumNeg +
                                    LISPremiumPos +
                                    LISPremiumNegSq +
                                    LISPremiumPosSq |
                                    state + firmID +
                                    L2btypedetail[L2BA_0] +
                                    L2btypedetail[L2BA_1_99] +
                                    L2btypedetail[L2BA_100] +
                                    L2btypedetail[L2BA_101_99] +
                                    L2btypedetail[L2BA_200_49] +
                                    L2btypedetail[L2BA_250Up],
                            dat = dat.temp,
                            cluster = "firmID")
                        }

        if (t==2007){
            reg.panel.b <- feols(lnS ~ belowBench2006 +
                                    LISPremiumNeg +
                                    LISPremiumPos +
                                    LISPremiumNegSq +
                                    LISPremiumPosSq |
                                    state + firmID +
                                    L1btypedetail[L1BA_0] +
                                    L1btypedetail[L1BA_1_99] +
                                    L1btypedetail[L1BA_100] +
                                    L1btypedetail[L1BA_101_99] +
                                    L1btypedetail[L1BA_200_49] +
                                    L1btypedetail[L1BA_250Up],
                            dat = dat.temp,
                            cluster = "firmID")
                        }

        if (t==2006){
            reg.panel.b <- feols(lnS ~ belowBench2006 +
                                    LISPremiumNeg +
                                    LISPremiumPos +
                                    LISPremiumNegSq +
                                    LISPremiumPosSq |
                                    state + firmID +
                                    L0btypedetail[L0BA_0] +
                                    L0btypedetail[L0BA_1_99] +
                                    L0btypedetail[L0BA_100] +
                                    L0btypedetail[L0BA_101_99] +
                                    L0btypedetail[L0BA_200_49] +
                                    L0btypedetail[L0BA_250Up],
                            dat = dat.temp,
                            cluster = "firmID")
                        }

       return(list(reg.panel.a,reg.panel.b))
}

# Add a parenthesis to SE
addparentheses <- function(x){ paste0("(",x,")")}


# Add a dollar sign to display in latex
format.dollar <- function(x, ...) paste0( "$", unclass(x))