
library(logging)

Logger <- setRefClass("Logger",
                      fields=list(name = "character"),
                      methods=list(
                        log = function(level, ...) 
                        { levellog(level, ..., logger=name) },
                        debug = function(...) { log("DEBUG", ...) },
                        info = function(...) { log("INFO", ...) },
                        warn = function(...) { log("WARN", ...) },
                        error = function(...) { log("ERROR", ...) }
                      ))

basicConfig()

l <- Logger$new(name="hierarchic.logger.name")

l$warn(paste0("asdsadasdasdasd","sdfsfsdfsfdsfsdfsdf"))

Logger$help("help")

Logger2 <- list(name = "character",
                  
                        log = function(level, ...) 
                        { levellog(level, ..., logger=name) },
                        debug = function(...) { log("DEBUG", ...) },
                        info = function(...) { log("INFO", ...) },
                        warn = function(...) { log("WARN", ...) },
                        error = function(...) { log("ERROR", ...) }
                      )

Logger2$info("sdfsfsdfs")
Logger2$warn(12)



paste0(c(1,2,3), as.character(c("a","b","c")), sep="", collapse = "")
c("a","b","c")
