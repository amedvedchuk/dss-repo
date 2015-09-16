

getSumOf4thColFwf <- function(){
    
#     conn <- url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
    
    data <- read.fwf("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", widths = c(15, 4, 9, 4, 9, 4, 9, 4, 4), skip = 4, header = FALSE)

#     close(conn)
    
    head(data)

    data
    
}