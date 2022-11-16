
# Clean Memory
rm(list=ls())
gc()

# Create a function to set the Working Directory no matter the os.
set.mydir <- function(name="Nixon", as=3) {

    temp <- Sys.info()

        if (temp['sysname']=="Darwin"){
            dir_temp = paste0("/Users/nix/Documents/GitHub/Econ771/Assigments/AS ", as)
            setwd(dir_temp)
            return(print(paste0("Hello ", name, 
                    " you are in ", temp['sysname'], 
                    " and your root directory is: ", getwd()
                                )
                        )
                   )
        }
        if (temp['sysname']=="Windows"){
            dir_temp = paste0("~/GitHub/Econ771/Assigments/AS ", as)
            setwd(dir_temp)
        #   here::i_am(Setmydir.R)

            return(print(paste0("Hello ", temp['nodename'], 
                                " you are in ", temp['sysname'], 
                                " and your root directory is: ", getwd()
                                )
                        )
                    )
        }
}
