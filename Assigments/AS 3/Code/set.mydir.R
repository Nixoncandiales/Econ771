
# Clean Memory
rm(list=ls())
gc()

# Create a function to set the Working Directory no matter the os.
set.mydir <- function(name="nixon") {

    temp <- Sys.info()

        if (temp['sysname']=="Windows"){
            setwd("~/Github/Econ771/Assigments/AS 3")
        #   here::i_am(Setmydir.R)

            return(print(paste0("Hello ", temp['nodename'], 
                                " you are in ", temp['sysname'], 
                                " and your root directory is: ", getwd()
                                )
                        )
                    )
        }
}

set.mydir()