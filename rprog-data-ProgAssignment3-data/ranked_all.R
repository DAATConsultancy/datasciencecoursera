
rankall <- function(outcome, num = "best") {
        
        hosp_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## Read outcome data
        hosp_data[, 11] <- as.numeric(hosp_data[, 11])
        hosp_data[, 17] <- as.numeric(hosp_data[, 17])
        hosp_data[, 23] <- as.numeric(hosp_data[, 23])
        
        possible_states <- unique(hosp_data$State)
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        ##Rename columns for easier use in script
        colnames(hosp_data)[11] <- possible_outcomes[1]
        colnames(hosp_data)[17] <- possible_outcomes[2]
        colnames(hosp_data)[23] <- possible_outcomes[3]
        
        
        ## Check that if outcome is valid
        if(!any(possible_states == state)){
                stop("invalid state")
        }
        else if (!any(outcome == possible_outcomes)){
                stop("invalid outcome")
        }
        
        ## Create list with relevant data removing na values for given outcome
        sub_hosp <- hosp_data[!is.na(hosp_data[,outcome]) ,c(2,7,11,17,23)] 
        
        ranked_all <- data.frame(hospital = character(), state = character())

        ## For each state, find the hospital of the given rank        
        for (i in seq_along(possible_states){
                
                l_state <- possible_states[i] 
                state_sub <- sub_hosp[sub_hosp[,"State"]==l_state,]
                
                ##Created rank variable used for selecting the appropriate hospital
                if(num == "best"){
                        rank <- 1
                }
                else if(num =="worst"){
                        rank <- nrow(state_sub)
                }
                else{
                        rank <- num
                }
                
                ##Sort data on relevant outcome and select best hospital
                sort_state <- state_sub[order(state_sub[,outcome], state_sub[,"Hospital.Name"]),]
                ranked_hospital <- sort_state[rank,"Hospital.Name"]
                
                ranked_all <- rbind(ranked_all, c(ranked_hospital, l_state))
        } 
       

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ranked_all
}