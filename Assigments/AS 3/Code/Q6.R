
# Create empty list to be appended later

panel.a <- list()
panel.b <- list()

################################################################################
# 2006
################################################################################

# Data
reg.dat <- dat %>% filter(RDwindow20062==1 & year==2006)

# Panel A
panel.a <- list(feols(lnS ~ belowBench2006 + LISPremiumNeg + 
                        LISPremiumPos, 
                        cluster="firmID", 
                        dat=reg.dat))


#Panel B 
 panel.b <- list(feols(lnS ~ belowBench2006 +
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
                    dat = reg.dat,
                    cluster = "firmID"))

################################################################################
# 2007
################################################################################

# Data
reg.dat <- dat %>% 
                group_by(uniqueIDNum) %>% 
                mutate(LISPremiumNeg =  lag(LISPremiumNeg,1),
                       LISPremiumNegSq = lag(LISPremiumNegSq, 1),
                       LISPremiumPos = lag(LISPremiumPos,1),
                       LISPremiumPosSq = lag(LISPremiumPosSq,1)) %>%
                filter(RDwindow20062==1 & year==2007)


# Panel A
panel.a <- append(panel.a, list(feols(lnS ~ belowBench2006 + 
                                            LISPremiumNeg + 
                                            LISPremiumPos, 
                                            cluster="firmID", 
                                            dat=reg.dat)))



#Panel B
panel.b <- append(panel.b, list(feols(lnS ~ belowBench2006 +
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
                                    dat = reg.dat,
                                    cluster = "firmID")))

################################################################################
# 2008
################################################################################

# Data
reg.dat <- dat %>% 
                group_by(uniqueIDNum) %>% 
                mutate(LISPremiumNeg =  lag(LISPremiumNeg,2),
                       LISPremiumNegSq = lag(LISPremiumNegSq, 2),
                       LISPremiumPos = lag(LISPremiumPos,2),
                       LISPremiumPosSq = lag(LISPremiumPosSq,2)) %>%
                filter(RDwindow20062==1 & year==2008)


# Panel A
panel.a <- append(panel.a, list(feols(lnS ~ belowBench2006 + 
                                            LISPremiumNeg + 
                                            LISPremiumPos, 
                                            cluster="firmID", 
                                            dat=reg.dat)))

#Panel B
panel.b <- append(panel.b, list(feols(lnS ~ belowBench2006 +
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
                                        dat = reg.dat,
                                        cluster = "firmID")))

################################################################################
# 2009
################################################################################

# Data
reg.dat <- dat %>% 
                group_by(uniqueIDNum) %>% 
                mutate(LISPremiumNeg =  lag(LISPremiumNeg,3),
                       LISPremiumNegSq = lag(LISPremiumNegSq, 3),
                       LISPremiumPos = lag(LISPremiumPos,3),
                       LISPremiumPosSq = lag(LISPremiumPosSq,3)) %>%
                filter(RDwindow20062==1 & year==2009)


# Panel A
panel.a <- append(panel.a, list(feols(lnS ~ belowBench2006 + 
                                            LISPremiumNeg + 
                                            LISPremiumPos, 
                                            cluster="firmID", 
                                            dat=reg.dat)))


#Panel B
panel.b <- append(panel.b, list(feols(lnS ~ belowBench2006 +
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
                                        dat = reg.dat,
                                        cluster = "firmID")))

################################################################################
# 2010
################################################################################

# Data
reg.dat <- dat %>% 
                group_by(uniqueIDNum) %>% 
                mutate(LISPremiumNeg =  lag(LISPremiumNeg,4),
                       LISPremiumNegSq = lag(LISPremiumNegSq, 4),
                       LISPremiumPos = lag(LISPremiumPos,4),
                       LISPremiumPosSq = lag(LISPremiumPosSq,4)) %>%
                filter(RDwindow20062==1 & year==2010)


# Panel A
panel.a <- append(panel.a, list(feols(lnS ~ belowBench2006 + 
                                            LISPremiumNeg + 
                                            LISPremiumPos, 
                                            cluster="firmID", 
                                            dat=reg.dat)))


#Panel B
panel.b <- append(panel.b, list(feols(lnS ~ belowBench2006 +
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
                                        dat = reg.dat,
                                        cluster = "firmID")))

# Rename lists to years
names(panel.a) <- seq(2006,2010,1)
names(panel.b) <- seq(2006,2010,1)

# Write the tables
options("modelsummary_format_numeric_latex" = "plain")

modelsummary(panel.a, 
            "Output/tab/tab6.a.tex", stars = TRUE, drop = "Int", 
            gof_map = c("nobs", "r.squared"), 
            coef_map = c("belowBench2006"="Below benchmark, 2006", 
                         "LISPremiumNeg"="Below benchmark", 
                         "LISPremiumPos"="Above benchmark"),
            title="Effect of LIS Benchmark Status in 2006 on Plan Enrollment",
            booktabs=TRUE) %>% 
            kableExtra::pack_rows("Premium—subsidy, 2006", 3, 6)

print("Table6.panel.a was written on Disk on Output/tab/tab6.a.tex")

panel.a <- modelsummary(panel.a, 
            "markdown", stars = TRUE, drop = "Int", 
            gof_map = c("nobs", "r.squared"), 
            coef_map = c("belowBench2006"="Below benchmark, 2006", 
                         "LISPremiumNeg"="Below benchmark", 
                         "LISPremiumPos"="Above benchmark"),
            title="Effect of LIS Benchmark Status in 2006 on Plan Enrollment",
            booktabs=TRUE) %>% 
            kableExtra::pack_rows("Premium—subsidy, 2006", 3, 6)


modelsummary(panel.b,
          "Output/tab/tab6.b.tex", stars = TRUE, keep = "belowBench2006",
          coef_map = c("belowBench2006"="Below benchmark, 2006"),
          gof_map = c("nobs", "r.squared"), title = "Effect of LIS Benchmark Status in 2006 on Plan Enrollment")


print("Table6.panel.b was written on Disk on Output/tab/tab6.b.tex")

panel.b <- modelsummary(panel.b,
          "markdown", stars = TRUE, keep = "belowBench2006",
          coef_map = c("belowBench2006"="Below benchmark, 2006"),
          gof_map = c("nobs", "r.squared"), title = "Effect of LIS Benchmark Status in 2006 on Plan Enrollment")
