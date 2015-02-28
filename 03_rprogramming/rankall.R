source("utilityFunctions.R")

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    prepared <- prepareListByState(outcome)
    outForState <- prepared$byState
    columnName <- prepared$columnName
    
    x<-lapply(outForState, rankState, columnName, num)
    df <- stack(x)
    names(df)<- c("hospital", "state")
    df
}


