#---------------------------------------------------------------
# Estimate the RD
#---------------------------------------------------------------
    source("Code/my_functions.R")
    # Parameters to enter the loop
    years <- 2006:2010
    lags <- 0:4
    kernel <- c("uniform", "triangular")
    df <- c()

    # Estimate the RD by Rdrobust for all years, Panel.a, and Panel.b and store the results in a data.frame 
    for (i in 1:5){
        dat.temp <-  My_lag(years[i], lags[i]) # Own function to subset and create lags... 
        ###########
        ###########  I tried hard to include lags and covariates to the rdrobust but got erros!!!!!!
        ###########
            for (j in 1:2){
                for (k in 1:2){
                    mod.rd <- rdrobust(dat.temp$lnS, dat.temp$LISPremium, p = j, kernel = kernel[k])
                    summary(mod.rd)
                    n <- mod.rd[["N"]][1] + mod.rd[["N"]][2] #Get the total number of observations
                    b <- mod.rd[["Estimate"]][1] # Get the conventional estimate RD
                    se.rb <- mod.rd[["Estimate"]][4] # Get the Robust Se
                    t <- years[i] 
                    rows <- rbind(b, se.rb, n, k, j, t) # Bind the results in a column
                    df <- cbind(df,rows) # Append the results to the data.frame
                }
            }
    }
    rownames(df) <- c("b", "se", "n", "k", "p", "t" ) # Variable names
    df <- as_tibble(round(df,3) %>% t(.)) # Create the tibble
    df <- df %>% 
            mutate(
                kernel = case_when(
                    k==1 ~ "Uniform",
                    k==2 ~ "Triangular"
                ),
                poly = case_when(
                    p==1 ~ "Local Linear",
                    p==2 ~ "Local Quadratic Polynomial"
                )
            )
    df
#---------------------------------------------------------------
# Format Table 7 
#---------------------------------------------------------------
 ## Panel A
 # Group by Kernel Uniform
 tab7 <- df %>% 
            filter(p==1 & k==1) %>% 
            select(b,se,n, kernel) %>% 
            t() %>% 
            as_tibble()
 # Group by Kernel Triangular
 tab7 <- rbind(tab7, df %>% 
                        filter(p==1 & k==2) %>% 
                        select(b,se,n, kernel) %>% 
                        t() %>% 
                        as_tibble()
            )

 ## Panel B
# Group by Kernel Uniform
 tab7 <- rbind(tab7, df %>% 
                        filter(p==2 & k==1) %>% 
                        select(b,se,n, kernel) %>% 
                        t() %>% 
                        as_tibble()
            )
 # Group by Kernel Triangular
 tab7 <- rbind(tab7, df %>% 
                        filter(p==2 & k==2) %>% 
                        select(b,se,n, kernel) %>% 
                        t() %>% 
                        as_tibble())

name <- rep(c("Conventional estimate", "", "Observations", "Kernel"),4)
names(tab7) <- 2006:2010
tab7 <- cbind(tab7, name) %>% relocate(name)

for(i in c(2,6,10,14)){
    tab7[i,] <- addparentheses(tab7[i,])
}

#---------------------------------------------------------------
# Clear Memory and delete aux objects 
#---------------------------------------------------------------
rm(My_lag, My_reg, i, j, k, t, n, b, se.rb, years, lags, rows, kernel, name, dat.temp, df, mod.rd)
gc()

tab7_tex <- knitr::kable(tab7, "latex", booktabs = T, align = "lccccc",
                   caption = "Rdrobust estimation with optimal bandwith") %>% 
            kableExtra::pack_rows("Panel A. Local linear", 1, 8) %>%
            kableExtra::pack_rows("Panel B. Quadratic Polinomial", 9, 16)
tab7_tex
cat(tab7_tex, file = 'Output/tab/table7.tex') # Write the table on disk
rm(tab7_tex)            