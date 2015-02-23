
readspec <- function(directory, id = 1:332){
  
  files <- paste(directory, "/" ,formatC(id, width = 3, flag = "0", format = "d"), ".csv", sep = "")
  print(files)
  
  readed3 <- do.call("rbind", lapply(files, read.csv))
  
  readed3
  
}