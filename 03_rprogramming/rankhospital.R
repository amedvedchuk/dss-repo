source("utilityFunctions.R")


rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
     
    prepared <- prepareListByState(outcome, state=state)
    outForState <- prepared$byState[[state]]
    columnName <- prepared$columnName
    
    ret <- rankState(outForState, columnName, num)
    ret
    
}