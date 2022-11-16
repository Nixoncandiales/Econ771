#---------------------------------------------------------------
# Part a
#---------------------------------------------------------------
Q4.a <- rdplot(y = lnS, 
                 x = LISPremium,
                 title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
                 x.label = "Monthly premium − LIS subsidy, 2006",
                 y.label = "log enrollment share, 2006", 
                 h=10,
                 scale=1,
                 x.lim=c(-10,10),
                 kernel = "uniform")

ggsave("Output/fig/Q4A.png")
print("Figure Q4A.png has been written in Disk on Output/fig/Q4A.png")

#---------------------------------------------------------------
# Part b
#---------------------------------------------------------------

Q4.b <- rdplot(y = lnS, 
                 x = LISPremium,
                 title = "RD Plot",
                 x.label = "Running Variable",
                 y.label = "Outcome", 
                 binselect = "esmv",
                 kernel = "uniform")

bin.avg <- as_tibble(Q4.b$vars_bins)

plot.bin <- bin.avg %>% 
            ggplot(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + 
            geom_point() + theme_bw() +
            geom_vline(aes(xintercept=0),linetype='dashed') +
            geom_vline(aes(xintercept=-10),linetype='solid', color='red') +
            geom_vline(aes(xintercept=10),linetype='solid', color='red') +
            scale_x_continuous(
                breaks = c(-35, 85),
                label = c("Treated", "Untreated")
            ) +
            xlab("Monthly premium − LIS subsidy, 2006") + 
            ylab("log enrollment share, 2006") +
            labs(title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
              subtitle = "IMSE-optimal bins: 66/94",
              caption = "Dots are local averages with a equally spaced bin size")

ggsave("Output/fig/Q4B.png")
print("Figure Q4B.png has been written in Disk on Output/fig/Q4B.png")

#---------------------------------------------------------------
# Clear Memory
#---------------------------------------------------------------
rm(bin.avg, plot.bin, Q4.a, Q4.b)
gc()