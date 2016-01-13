
setwd("../dsci-benchmark/")
setwd("../../dsci-benchmark/")


source("../dsci-benchmark/benchmark.R")
source("benchmark.R")

m_predict <- function(phrase){
    res <- predict_backoff(dtl4, phrase
                    ,non_stop = F
                    ,show_last = 3
                    ,verbose = F
                    ,is_hash = F
                    ,simple_out = T)                  
    res
}

m_predict("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predict_backoff(dl = dtl5, "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
predict_backoff(dtl4, "Go on a romantic date at the")
predict_backoff(dtlh4, "Go on a romantic date at the", is_hash = T)


benchmark(m_predict, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
