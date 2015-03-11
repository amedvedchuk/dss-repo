
getCharCountAtLines <- function(lineIndex){
    
    conn <- url("http://biostat.jhsph.edu/~jleek/contact.html")
    
    toplines <- readLines(conn, n = max(lineIndex))
    
    close(conn)
    
    neededLines <- toplines[lineIndex]

    neededLines
        result <- sapply(neededLines, nchar, USE.NAMES = FALSE)
    result
    
    
}
