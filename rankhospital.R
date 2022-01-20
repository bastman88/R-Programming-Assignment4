
rankhospital <- function(state, outcome, num){
    
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
    df_outcome_filtered <- df_outcome[df_outcome[,"State"]==state,]
    num_hospitals <- length(as.list(unique(df_outcome_filtered[["Hospital.Name"]])))
    
    ## Check if num is greater than number of hospitals in the given state
    if(num_hospitals<num&&is.numeric(num)){
        return(NA)
    }
    
    
    
    ## Set col_name
    if(outcome == "heart attack"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    if(outcome == "heart failure"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    if(outcome == "pneumonia"){
        col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    ## Return hospital name in that state with lowest 30-day death rate
    ## Coerce column to numeric
    df_outcome_filtered[,col_name] <- as.numeric(df_outcome_filtered[,col_name])
    ## Drop rows with missing values
    df_outcome_filtered2 <- df_outcome_filtered[complete.cases(df_outcome_filtered[col_name]),]
    res_df <- df_outcome_filtered2[order(as.numeric(df_outcome_filtered2 [[col_name]]),df_outcome_filtered2["Hospital.Name"]),]
    
    if(num == "worst"){
        res_row <- tail(res_df, n = 1)
        hospital_name <- res_row[["Hospital.Name"]]
        return(hospital_name)
    }
    if(num == "best"){
        res_row <- res_df[1,]
        hospital_name <- res_row[["Hospital.Name"]]
        return(hospital_name)
    }
    res_row <- res_df[num,]
    hospital_name <- res_row[["Hospital.Name"]]
    return(hospital_name)
}