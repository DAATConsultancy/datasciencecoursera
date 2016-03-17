rankhospital <- function(state, outcome, num = 'best') {
        hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## Read outcome data
        hosp_data[, 11] <- as.numeric(hosp_data[, 11])
        hosp_data[, 17] <- as.numeric(hosp_data[, 17])
        hosp_data[, 23] <- as.numeric(hosp_data[, 23])
        
        
        possible_states <- hosp_data$State
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ##Rename columns for easier use in script
        colnames(hosp_data)[11] <- possible_outcomes[1]
        colnames(hosp_data)[17] <- possible_outcomes[2]
        colnames(hosp_data)[23] <- possible_outcomes[3]
        
        
        ## Check if state and outcome are valid
        if(!any(possible_states == state)){
                stop("invalid state")
        }
        else if (!any(outcome == possible_outcomes)){
                stop("invalid outcome")
        }
        
        ## Create list with relevant state data removing na values for given outcome
        sub_hosp_data <- hosp_data[!is.na(hosp_data[,outcome]) & hosp_data[,7] == state,c(2,7,11,17,23)] 
        
        ##Sort data on relevant outcome and select best hospital
        sorted_hosp <- sub_hosp_data[order(sub_hosp_data[,outcome],sub_hosp_data[,"Hospital.Name"]),]
        
        ##Created rank variable used for selecting the appropriate hospital
        if(num == "best"){
                rank <- 1
        }
        else if(num =="worst"){
                rank <- nrow(sorted_hosp)
        }
        else{
                rank <- num
        }
                         
        ranked_hospital <- sorted_hosp[rank,"Hospital.Name"]
        
        ranked_hospital            ## Return hospital name in that state with ranked 30-day death rate
        
        
        
}