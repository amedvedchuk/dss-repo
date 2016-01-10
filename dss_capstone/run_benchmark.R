
setwd("../dsci-benchmark/")

source("../dsci-benchmark/benchmark.R")

m_predict <- function(phrase){
    res <- predict_backoff(dtl, phrase
                    ,non_stop = F
                    ,show_last = 3
                    ,verbose = F
                    ,simple_out = T)                  
    res
}

m_predict("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")

benchmark(m_predict, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
