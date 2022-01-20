rankall <- function(outcome, num = "best"){
    
    df_outcome <- read.csv("outcome-of-care-measures.csv")
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    ## Check if outcome is valid
    if(!(outcome %in% outcomes)){
        stop("invalid outcome")
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
    df_outcome[,col_name] <- as.numeric(df_outcome[,col_name])
    ## Drop rows with missing values
    df_outcome_2 <- df_outcome[complete.cases(df_outcome[col_name]),]
    col_1 <- df_outcome_2["State"]
    col_2 <- as.numeric(df_outcome_2 [[col_name]])
    col_3 <- df_outcome_2["Hospital.Name"]
    res_df <- df_outcome_2[order(col_1,col_2,col_3),]
    
    
    
    if(num == "worst"){
        res_rows <- lapply(split(res_df,res_df$State), FUN = function(x) tail(x ,n = 1))
        res_rows_2 <- lapply(res_rows, FUN = function(x) x[c("Hospital.Name", "State")])
        hospital_ranking <- do.call("rbind", res_rows_2)
        colnames(hospital_ranking) <- c("hospital", "state")
        return(hospital_ranking)
    }
    if(num == "best"){
        num <- 1
    }
    
    return_nth_row <- 
    res_rows <- lapply(split(res_df,res_df$State), FUN = function(x) x[num,c("Hospital.Name", "State")])
    hospital_ranking <- do.call("rbind", res_rows)
    colnames(hospital_ranking) <- c("hospital", "state")
    return(hospital_ranking)
}