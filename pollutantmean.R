pollutantmean <- function (directory, pollutant, id = 1:332) {

        pol_data <- data.frame()
        
        for (i in seq_along(id)){
                
                filename <- paste(directory, "/", paste0(rep('0', 3-nchar(id[i])), collapse = ""), id[i], ".csv", sep="")  #Create path to filename

                new_data <- read.csv(filename)
                
                pol_data <- rbind(pol_data, new_data)
        }
        
        pol_na <- is.na(pol_data[,pollutant])
        pol_mean <- mean(pol_data[!pol_na,pollutant])
        print(pol_mean)
}
