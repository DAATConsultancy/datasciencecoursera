corr <- function(directory, treshold = 0){
        
        c_cases <- complete(directory)
        t_cases <- c_cases[c_cases[,"nobs"]>treshold, "id"]
        
        cr <- c()
        
        for (i in seq_along(t_cases)){
                
                filename <- paste(directory, "/", paste0(rep('0', 3-nchar(t_cases[i])), collapse = ""), t_cases[i], ".csv", sep="")  #Create path to filename
                
                new_data <- read.csv(filename)
                
                cr <- c(cr, cor(new_data[,"sulfate"],new_data[,"nitrate"], use = "pairwise.complete.obs"))

        }

        cr
}