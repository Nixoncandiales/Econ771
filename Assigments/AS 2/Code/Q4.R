#Q4
df_temp <- reg.dat
# delta_D
reg <- feols(log_y ~ int, data=df_temp) # se:iid but est is ok
delta_D <- reg$coefficients[['int']]
R2_D <- 1 - (reg$ssr)/(var(df_temp$log_y)*dim(df_temp)[1])

# delta_Dx
reg <- feols(log_y ~ int | npi+Year, data=df_temp)
delta_Dx <- reg$coefficients[['int']]
R2_Dx <- 1 - (reg$ssr)/(var(df_temp$log_y)*dim(df_temp)[1])

rm(list=c('reg'))
gc()

# table
rho_list <- seq(0, 2, 0.5)
R2max_list <- seq(0.5, 1, 0.1)
tab <- data.frame()
for (i in 1:length(rho_list)){
  row <- data.frame()
  for (j in 1:length(R2max_list)){
    rho <- rho_list[i]
    R2max <- R2max_list[j]
    if (R2max > R2_Dx) {
      delta_star <- delta_Dx - rho*(delta_D-delta_Dx)*(R2max-R2_Dx)/(R2_Dx-R2_D)
      interval <- paste0('[',round(delta_Dx,2),',',round(delta_star,2),']')
    } else {
      interval <- 'NA'
    }
    colname <- paste0('$R_{max}^2=',R2max,'$')
    row[1,colname] <- interval
  }
  tab <- bind_rows(tab, row)
}
rownames(tab) <- c('$\rho=0$','$\rho=0.5$','$\rho=1$','$\rho=1.5$',
                   '$\rho=2$')
tab <- tab[,5:6]
tab4 <- xtable(tab,
               align=c('c','c','c'),
               caption = "Altonji, Elder, and Taber (2005)")

rm(list=c('row','tab','colname','delta_D','delta_Dx','delta_star','i','j',
          'R2_D','R2_Dx','R2max','R2max_list','rho','rho_list','interval'
          ))