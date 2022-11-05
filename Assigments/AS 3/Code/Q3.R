#---------------------------------------------------------------
# J-=J+=10
#---------------------------------------------------------------
Q3.a <- rdplot( y = lnS, 
                x = LISPremium,
                title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
                x.label = "Monthly premium − LIS subsidy, 2006",
                y.label = "log enrollment share, 2006",
                hide=TRUE,
                n = 10)

bin.avg <- as_tibble(Q3.a$vars_bins)

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
              subtitle = "number of bins = 10",
              caption = "Dots are local averages with a equally spaced bin size")

ggsave("Output/fig/Q3A.png")
print("Figure Q3A.png has been written in Disk on Output/fig/Q3A.png")       

Q3.a <- rdplot( y = lnS, 
                x = LISPremium,
                title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
                x.label = "Monthly premium − LIS subsidy, 2006",
                y.label = "log enrollment share, 2006",
                 n=10,
                 scale=1,
                 x.lim=c(-10,10),
                 h=10)
ggsave("Output/fig/Q3A2.png")

#---------------------------------------------------------------
# J-=J+=30
#---------------------------------------------------------------

Q3.b <- rdplot( y = lnS, 
                x = LISPremium,
                title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
                x.label = "Monthly premium − LIS subsidy, 2006",
                y.label = "log enrollment share, 2006",
                hide=TRUE, 
                n = 30)

bin.avg <- as_tibble(Q3.b$vars_bins)

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
            ylab("log enrollment share, 2006")+
            labs(title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
              subtitle = "number of bins = 30",
              caption = "Dots are local averages with a equally spaced bin size")

ggsave("Output/fig/Q3B.png")
print("Figure Q3B.png has been written in Disk on Output/fig/Q3B.png")
Q3.b <- rdplot( y = lnS, 
                x = LISPremium,
                title = "The Effect of 2006 Benchmark Status on 2006 Enrollment",
                x.label = "Monthly premium − LIS subsidy, 2006",
                y.label = "log enrollment share, 2006",
                 n=30,
                 scale=1,
                 x.lim=c(-10,10))
ggsave("Output/fig/Q3B2.png")

#---------------------------------------------------------------
# Clear Memory
#---------------------------------------------------------------
rm(bin.avg, plot.bin, Q3.a, Q3.b)
gc()
