complete <- function(directory, id = 1:332){
        
        complete_cases <- data.frame (id = integer(), nobs = integer())
        for(i in seq_along(id)){
                
                filename <- paste(directory, "/", paste0(rep('0', 3-nchar(id[i])), collapse = ""), id[i], ".csv", sep="")  #Create path to filename
                
                new_data <- read.csv(filename)
                
                comp_data <- complete.cases(new_data)
                
                complete_cases <- rbind(complete_cases, data.frame(id = id[i], nobs = sum(comp_data)))
                
        }
        complete_cases
}