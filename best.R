best <- function(state, outcome){
    df_outcome <- read.csv("outcome-of-care-measures.csv")
    states <- as.list(unique(df_outcome[["State"]]))
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    ## Check that state and outcome are valid
    if(!(state %in% states)){
        stop("invalid state")
    }
    if(!(outcome %in% outcomes)){
        stop("invalid outcome")
    }
    
    ## Filter data frame by state
    df_outcome_filtered = df_outcome[df_outcome[,"State"]==state,]
    
    ## Return hospital name in that state with lowest 30-day death rate
    if(outcome == "heart attack"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        res_row <- df_outcome_filtered[which.min(df_outcome_filtered[[col_name]]),]
        hospital_name <- res_row[["Hospital.Name"]]
        return(hospital_name)
    }
    if(outcome == "heart failure"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        res_row <- df_outcome_filtered[which.min(df_outcome_filtered[[col_name]]),]
        hospital_name <- res_row[["Hospital.Name"]]
        return(hospital_name)
    }
    if(outcome == "pneumonia"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        res_row <- df_outcome_filtered[which.min(df_outcome_filtered[[col_name]]),]
        hospital_name <- res_row[["Hospital.Name"]]
        return(hospital_name)
    }
    

                        
}