prepareListByState <- function(outcome, columnName, state=NULL) {
    ## Read outcome data
    ## Check that state (in nut NULL) and outcome are valid
    ## Return list of data frames by state
    
    outc <- read.csv("data3/outcome-of-care-measures.csv")
    
    if(!is.null(state)) {
        # obtain set of states as possible values
        states <- levels(outc$State)
        
        if(!is.element(state, states)){
            stop("invalid state")
        }
    }
    
    # define outcome alias to real column name map
    outcomes_map <- list(
        "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    if(!is.element(outcome, names(outcomes_map))){
        stop("invalid outcome")
    }
    
    print(outcomes_map[[outcome]])
    
    # address column name by alias
    columnName <- outcomes_map[[outcome]];
    
    # convert needed column fro mcharacter to numeric
    outc[, columnName] <- as.numeric(as.vector(outc[, columnName]))
    
    outForState <- split(outc,outc$State)
    
    ret <- list(
        "byState" = outForState, 
        "columnName" = columnName) 
}

rankState <- function(outForState, columnName, num) {
    
    sorted <- outForState[order(outForState[[columnName]], outForState[["Hospital.Name"]]),]
    #     sorted
    sorted_cc <- sorted[complete.cases(sorted[[columnName]]), ]
    #     sorted_cc
    
    sorted_cc_length <- length(sorted_cc[[1]])
    
    num_map <- list(
        "best" = 1,
        "worst" = sorted_cc_length
    )
    
    rank_index = NULL;
    
    if (is.numeric(num)){
        rank_index <- num
    } else if (is.element(num, names(num_map))){
        rank_index <- as.numeric(num_map[num])
    } else {
        stop("invalid num, should be \"best\", \"worst\" or numeric")
    }   
    
    print(paste("rank_index:", rank_index, "sorted_cc_length:",sorted_cc_length))
    
    if(sorted_cc_length < rank_index){
        return(NA)
    }
    
    #     sorted_cc
    #     print(sorted_cc[rank_index, ]$Hospital.Name)
    ret <- as.character(sorted_cc[rank_index, ]$Hospital.Name)
    ret
}
