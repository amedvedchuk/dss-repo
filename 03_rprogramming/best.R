source("utilityFunctions.R")

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    prepared <- prepareListByState(outcome, state=state);
    
    outForState <- prepared$byState[[state]]
    ret <- as.character(outForState[which.min(outForState[[prepared$columnName]]),]$Hospital.Name)
    ret
    
}