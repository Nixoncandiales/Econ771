#########################################################################
# Part a
#########################################################################

lnS <- dat %>% pull(lnS) 
LISPremium <- dat %>% pull(LISPremium) 

Q4.a <- rdplot(y = lnS, 
                 x = LISPremium,
                 title = "RD Plot",
                 x.label = "Running Variable",
                 y.label = "Outcome", 
                 h=10,
                 scale=1,
                 x.lim=c(-10,10),
                 kernel = "uniform")

ggsave("Output/fig/Q4A.png")
print("Figure Q4A.png has been written in Disk on Output/fig/Q4A.png")

#########################################################################
# Part b
#########################################################################

Q4.b <- rdplot(y = lnS, 
                 x = LISPremium,
                 title = "RD Plot",
                 x.label = "Running Variable",
                 y.label = "Outcome", 
                 binselect = "esmv",
                 kernel = "uniform")

ggsave("Output/fig/Q4B.png")
print("Figure Q4B.png has been written in Disk on Output/fig/Q4B.png")